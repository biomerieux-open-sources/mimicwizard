# Cohort creation regression tests

The `Cohort creation tests` GitHub Actions workflow runs this matrix on every
pull request against an isolated PostgreSQL database using synthetic records.
It executes the application's SQL generator and cohort condition evaluator;
it does not test the browser interface, async fetching, saving cohorts, ICD
filters, or a full MIMIC installation.

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
| Aggregation | SUM over two adjacent hourly windows | Boundary event contributes once |
| Connection reuse | Repeated database column-type lookups | Reuses the existing connection |
| Outcomes refresh | Repeated ICD counts across strata | Correct counts; temporary tables are removed |
| Patient explorer | Switch and refresh cohorts in one Shiny module | Search data updates and previous patient plot state resets |

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

## Running locally

Install R packages `DBI`, `RPostgres`, `dplyr`, `dbplyr`, `shiny`, `shinyjs`,
and `tidyr`. Create an **empty,
disposable** PostgreSQL database named `mimicwizard_cohort_test`, then run:

```sh
export COHORT_TEST_DB=mimicwizard_cohort_test
export PGHOST=localhost PGPORT=5432 PGUSER=cohort_test PGPASSWORD=cohort_test
Rscript --vanilla tests/cohort-creation.R
```

The test user needs permission to create schemas. Fixtures are created inside a
transaction and rolled back, including on test failure. The runner refuses a
different database name, pre-existing MIMIC schemas, or an existing
`public.cohort` table. Never point it at a production database.

To make the GitHub check mandatory, configure a repository branch protection
rule or ruleset requiring the `cohort-tests` job; a workflow alone cannot enable
branch protection.
