# Cohort creation regression tests

The `Cohort creation tests` GitHub Actions workflow runs this matrix on every
pull request against an isolated PostgreSQL database using synthetic records.
It exercises the application's SQL generator, cohort condition evaluator,
selected ICD and explorer queries, and parts of the Shiny server logic. It does
not test the browser interface, async fetching, saving cohorts, or a full
MIMIC installation.

| Category | Test | Expected result |
| --- | --- | --- |
| Adjacent ICU stays | A lab event falls in two stays' attribution buffers | Both stays match under the current rule; no unique attribution is promised |
| Lower boundary | Event exactly at an hourly window's start | Included |
| Upper boundary | Event exactly at an hourly window's end | Excluded |
| Missing numeric value | `valuenum IS NULL` with a numeric comparison | Not selected |
| Missing event time | `charttime IS NULL` with an hourly restriction | No stay is selected |
| AND | Two event predicates | Intersection of stay IDs |
| OR | Two event predicates | Union of stay IDs |
| EXCLUDE | Predicate matches or is empty | Matching stays are removed from the stay universe |
| Parentheses | `A AND (B OR C)` | Explicitly enumerated reference stays |
| Invalid condition | A filter followed by AND or OR with no second operand | Actionable validation message before database queries |
| Aggregation | SUM over two adjacent hourly windows | Boundary event contributes once |
| Connection reuse | Repeated database column-type lookups | Reuses the existing connection |
| Query output | Fetch a cohort condition | SQL metadata remains available without printing query parameters |
| Outcomes refresh | Repeated ICD counts across strata | Correct counts; temporary tables are removed |
| Cohort explorer loading | Summary counts and unstratified outcomes including duplicates and missing IDs | Match the previous records; ICD results match without uploading admissions |
| Outcome pairing | Characteristics in different row orders, missing values, duplicate values, and strata | One row per ICU stay, matching characteristics to that stay; first sorted non-missing value is kept per characteristic |
| Outcomes spinner | Namespaced IDs for different modules | Spinner preserves UI without hashing the cohort-dependent UI object |
| Outcomes refresh | Cohort selection, button initialization, and repeated clicks | Initialization does not rerun outcomes; cohort changes and clicks do |
| Patient explorer | Switch and refresh cohorts in one Shiny module | Search data updates and previous patient plot state resets |
| ICD inclusion and exclusion | Prefixes, versions, AND and OR | Database-side matches select or remove the expected admissions |

Both ICU chart events and hospital lab events are tested for hourly boundaries,
missing values, missing timestamps, and SUM. COUNT is not currently offered as
a cohort filter aggregation.

Hospital events are matched to all stays in the same admission whose inclusive
attribution buffer extends from 24 hours before ICU admission to one hour after
ICU discharge. Overlapping buffers can therefore associate the same event with
multiple stays. An unrestrained hospital-event query can also return an
unmatched row with null stay identifiers; the missing-time test above concerns
temporal attribution with an hourly restriction. Hourly windows use
`[start, end)` and their selected `hr` range is inclusive.

ICD exclusion with AND requires every selected prefix to match at least once
per admission. ICD inclusion with AND retains the existing count-based
behavior, which can differ when an admission has duplicate prefix matches.

## Running locally

Install R packages `DBI`, `RPostgres`, `dplyr`, `dbplyr`, `shiny`, `shinyjs`,
`shinycssloaders`, and `tidyr`. Create an **empty,
disposable** PostgreSQL database named `mimicwizard_cohort_test`, then run:

```sh
export COHORT_TEST_DB=mimicwizard_cohort_test
export PGHOST=localhost PGPORT=5432 PGUSER=cohort_test PGPASSWORD=cohort_test
Rscript --vanilla tests/cohort-creation.R
```

The test user needs permission to create schemas. Fixtures are created inside a
transaction and rolled back, including on test failure. The runner refuses a
different database name, pre-existing MIMIC schemas, or existing
`public.cohort`, `public.d_cohorts`, or `public.demographics` tables. Never
point it at a production database.

To make the GitHub check mandatory, configure a repository branch protection
rule or ruleset requiring the `cohort-tests` job; a workflow alone cannot enable
branch protection.
