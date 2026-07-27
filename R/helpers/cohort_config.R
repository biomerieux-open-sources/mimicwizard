# ------------------------------------------------------------------------------
# Cohort configuration helpers
#
# Serialize / deserialize the cohort creation state (the exact `chainCondition`
# array produced client-side by `htmlToCondition` plus ICD selectors) so a
# cohort can be:
#   - saved to database  (public.d_cohorts.cohort_definition JSONB)
#   - exported as a JSON file (reproducibility across installs)
#   - reloaded into the search bar (edit / duplicate)
# ------------------------------------------------------------------------------

COHORT_CONFIG_VERSION <- 1L


# Normalize an ICD selector value into a character vector of individual codes.
# `input$icd_to_keep`/`input$icd_to_deny` come from the search-selection API
# widget as a single comma-separated string (its hidden input holds all
# selected values joined by commas), so a naive as.list() would wrap the
# whole string as ONE item instead of one item per code. Handle that, plain
# vectors, and already-split lists uniformly.
split_icd_codes <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  if (is.list(x)) x <- unlist(x)
  x <- unlist(strsplit(as.character(x), ","))
  x <- trimws(x)
  x[nzchar(x)]
}


# Ensure the cohort_definition JSONB column exists on an existing install.
# Idempotent: safe to call at every app startup.
ensure_cohort_definition_column <- function(database) {
  tryCatch({
    DBI::dbExecute(
      database,
      "ALTER TABLE public.d_cohorts ADD COLUMN IF NOT EXISTS cohort_definition jsonb NULL"
    )
  }, error = function(e) {
    warning(paste("Could not ensure cohort_definition column:", conditionMessage(e)))
  })
  invisible(NULL)
}


# Build a config list from the raw client-side JSON string held in
# `input$filter_tojson` and the ICD selector inputs.
#
# `filter_tojson_raw` is the string produced by `Shiny.setInputValue('...filter_tojson', ...)`
# with structure `{ message, result, request_time }`. We keep the `result`
# array (chainCondition) as the authoritative representation.
cohort_config_from_client <- function(filter_tojson_raw,
                                      icd_to_keep = NULL,
                                      icd_to_deny = NULL,
                                      allow_condition = "OR",
                                      deny_condition = "OR",
                                      cohort_name = NULL,
                                      cohort_description = NULL) {
  chain_condition <- list()
  if (!is.null(filter_tojson_raw) && nzchar(filter_tojson_raw)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(filter_tojson_raw, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(parsed) && isTRUE(parsed$message == "OK") && !is.null(parsed$result)) {
      chain_condition <- parsed$result
    }
  }

  list(
    mimicwizard_cohort_config_version = COHORT_CONFIG_VERSION,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    cohort_name = cohort_name,
    cohort_description = cohort_description,
    chain_condition = chain_condition,
    icd_to_allow = as.list(split_icd_codes(icd_to_keep)),
    icd_to_deny  = as.list(split_icd_codes(icd_to_deny)),
    allow_condition = if (isTruthy(allow_condition)) allow_condition else "OR",
    deny_condition  = if (isTruthy(deny_condition))  deny_condition  else "OR"
  )
}


cohort_config_to_json <- function(cfg, pretty = TRUE) {
  jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = pretty, null = "null", na = "null")
}


cohort_config_from_json <- function(json_text) {
  cfg <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  validate_cohort_config(cfg)
  cfg
}


validate_cohort_config <- function(cfg) {
  if (!is.list(cfg)) stop("Cohort configuration must be a JSON object.")
  version <- cfg$mimicwizard_cohort_config_version
  if (is.null(version)) {
    stop("Missing 'mimicwizard_cohort_config_version' field.")
  }
  if (!identical(as.integer(version), COHORT_CONFIG_VERSION)) {
    stop(sprintf(
      "Unsupported cohort configuration version: %s (expected %s).",
      version, COHORT_CONFIG_VERSION
    ))
  }
  if (is.null(cfg$chain_condition) || !is.list(cfg$chain_condition)) {
    stop("Missing or invalid 'chain_condition' field.")
  }
  invisible(TRUE)
}


# Convert the client-side chainCondition array (list of {type, ...} entries)
# into the internal `condition_object` used by `request_constrained()`:
#   list(condition_string, constraint_list, icd_to_allow, icd_to_deny,
#        allow_condition, deny_condition)
#
# Mirrors the transformation currently done inside
# eventSearchbarServer's `condition_object <- reactive(...)`.
chain_condition_to_condition_object <- function(chain_condition,
                                                database,
                                                icd_to_allow = NULL,
                                                icd_to_deny = NULL,
                                                allow_condition = "OR",
                                                deny_condition = "OR") {
  condition_to_filter <- list("AND" = "&", "OR" = "|")
  enclose_to_filter <- list("open" = " ( ", "close" = " ) ")

  condition_string <- ""
  condition_id <- 1
  constraint_list <- list()
  can_expect_condition_separator <- FALSE

  if (length(chain_condition) > 0) {
    for (row in chain_condition) {
      if (is.null(row$type)) next
      condition <- ""
      if (row$type == "filter") {
        if (can_expect_condition_separator) {
          condition <- condition_to_filter[["OR"]]
          condition_string <- paste0(condition_string, condition)
        }
        can_expect_condition_separator <- TRUE

        linksto <- (
          dplyr::tbl(database(), dbplyr::in_schema("public", "distinct_events")) %>%
            dplyr::filter(itemid == !!row$itemid) %>%
            dplyr::select("linksto") %>%
            dplyr::collect()
        )[[1]]

        constraint_list[[as.character(condition_id)]] <- list(
          constraint = list(
            itemid = row$itemid,
            constraint = row$constraint,
            aggr = row$aggr,
            field = row$field,
            value = row$value,
            is_exclusion = isTRUE(row$is_exclusion),
            time_constraint = list(
              is_time_constrained = row$is_time_constrained,
              time_min = row$time_min,
              time_max = row$time_max
            )
          ),
          linksto = linksto
        )
        condition <- paste0("condition_", condition_id)
        condition_id <- condition_id + 1
      } else if (row$type == "condition") {
        condition <- condition_to_filter[[row$element]]
        can_expect_condition_separator <- FALSE
      } else if (row$type == "enclose") {
        if (can_expect_condition_separator && row$element == "open") {
          sep <- condition_to_filter[["OR"]]
          condition_string <- paste0(condition_string, sep)
        }
        if (row$element == "close") {
          can_expect_condition_separator <- TRUE
        } else {
          can_expect_condition_separator <- FALSE
        }
        condition <- enclose_to_filter[[row$element]]
      }
      condition_string <- paste0(condition_string, condition)
    }
  }

  # Normalize ICD selectors: input$icd_to_keep is a single comma-separated string
  # in the current implementation; keep it as such for consistency with the DB path.
  flatten_icd <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) x <- unlist(x)
    if (length(x) == 0) return(NULL)
    paste(x, collapse = ",")
  }

  list(
    condition_string = condition_string,
    constraint_list = constraint_list,
    icd_to_allow = flatten_icd(icd_to_allow),
    icd_to_deny  = flatten_icd(icd_to_deny),
    allow_condition = if (is.null(allow_condition) || !nzchar(allow_condition)) "OR" else allow_condition,
    deny_condition  = if (is.null(deny_condition)  || !nzchar(deny_condition))  "OR" else deny_condition
  )
}


# Reconstruct the condition_object from a saved cohort_definition JSON string
# (i.e. as returned by cohort_config_to_json / cohort_config_from_json).
cohort_config_to_condition_object <- function(cfg, database) {
  chain_condition_to_condition_object(
    chain_condition = cfg$chain_condition,
    database = database,
    icd_to_allow = cfg$icd_to_allow,
    icd_to_deny  = cfg$icd_to_deny,
    allow_condition = cfg$allow_condition,
    deny_condition  = cfg$deny_condition
  )
}


# Build a human-readable description of a condition_object for the SQL accordion.
condition_object_summary <- function(condition_object) {
  parts <- list()

  cs <- condition_object$condition_string
  if (!is.null(cs) && nzchar(cs)) {
    labelled <- gsub("&", " AND ", cs, fixed = TRUE)
    labelled <- gsub("|", " OR ",  labelled, fixed = TRUE)
    # Mark exclusion filters (EXCLUDE checkbox) so the accordion reflects that
    # matching rows are removed from the cohort rather than kept - otherwise
    # the per-filter SQL alone looks like a plain inclusion filter.
    for (cond_id in names(condition_object$constraint_list)) {
      if (isTRUE(condition_object$constraint_list[[cond_id]]$constraint$is_exclusion)) {
        labelled <- gsub(
          paste0("\\bcondition_", cond_id, "\\b"),
          paste0("EXCLUDE(condition_", cond_id, ")"),
          labelled,
          perl = TRUE
        )
      }
    }
    parts[["Logical expression"]] <- labelled
  }

  if (isTruthy(condition_object$icd_to_allow)) {
    parts[["ICD included"]] <- paste0(
      condition_object$icd_to_allow,
      " (", condition_object$allow_condition, " combination)"
    )
  }
  if (isTruthy(condition_object$icd_to_deny)) {
    parts[["ICD excluded"]] <- paste0(
      condition_object$icd_to_deny,
      " (", condition_object$deny_condition, " combination)"
    )
  }

  parts
}


# Render the tag content of an "Associated SQL" accordion panel.
# `per_condition` is a named list `condition_1 = list(sql = ..., params = ...)`.
render_associated_sql_content <- function(per_condition, condition_object) {
  summary_parts <- condition_object_summary(condition_object)

  summary_block <- if (length(summary_parts) > 0) {
    tagList(lapply(names(summary_parts), function(k) {
      div(tags$b(paste0(k, " : ")), tags$span(summary_parts[[k]]))
    }))
  } else {
    tags$em("No filter defined.")
  }

  sql_blocks <- tagList(lapply(names(per_condition), function(k) {
    entry <- per_condition[[k]]
    sql_txt <- entry$sql
    params <- entry$params
    if (is.null(sql_txt)) sql_txt <- "(SQL not available)"
    params_repr <- if (length(params) > 0) {
      paste(vapply(seq_along(params), function(i) {
        paste0("$", i, " = ", format(params[[i]]))
      }, character(1)), collapse = "\n")
    } else {
      "(no parameters)"
    }
    cond_idx <- gsub("condition_", "", k)
    is_excl <- isTRUE(condition_object$constraint_list[[cond_idx]]$constraint$is_exclusion)
    div(
      tags$h4(
        paste0("Filter ", cond_idx),
        if (is_excl) tags$span("EXCLUDE", class = "ui tiny red label", style = "margin-left:8px;")
      ),
      if (is_excl) tags$p(
        style = "color:#9f3a38;font-style:italic;",
        "Exclusion filter: stays/admissions matched by this SQL are removed from the cohort (kept are those NOT matching)."
      ),
      tags$pre(sql_txt, style = "white-space: pre-wrap; word-break: break-word;"),
      tags$details(
        tags$summary("Parameters"),
        tags$pre(params_repr)
      )
    )
  }))

  tagList(
    div(class = "ui small header", "Cohort assembly"),
    summary_block,
    div(class = "ui divider"),
    div(class = "ui small header", "Per-filter SQL"),
    sql_blocks
  )
}
