################################################################################
########################### CONFIGURATION FILE #################################
################################################################################


env_value <- function(name, default = "") {
  value <- Sys.getenv(name, unset = default)
  if (identical(value, "")) default else value
}

env_boolean <- function(name, default) {
  value <- tolower(env_value(name, as.character(default)))
  if (value %in% c("true", "1", "yes")) return(TRUE)
  if (value %in% c("false", "0", "no")) return(FALSE)
  stop(sprintf("%s must be true or false.", name), call. = FALSE)
}

env_port <- function(name, default) {
  value <- suppressWarnings(as.integer(env_value(name, as.character(default))))
  if (is.na(value) || value < 1L || value > 65535L) {
    stop(sprintf("%s must be a port between 1 and 65535.", name), call. = FALSE)
  }
  value
}

# Allow an interactive user to choose a database mode, or force a mode for a deployment.
INTERACTIVE <- env_boolean("MIMICWIZARD_INTERACTIVE", TRUE)
APPLICATION_MODE <- toupper(env_value("MIMICWIZARD_MODE", "DEMO"))

if (!APPLICATION_MODE %in% c("INIT_DEMO", "DEMO", "HOSTED")) {
  stop("MIMICWIZARD_MODE must be INIT_DEMO, DEMO, or HOSTED.", call. = FALSE)
}

# Include a trailing separator because cache consumers append relative cache paths.
CACHE_DIR <- env_value("MIMICWIZARD_CACHE_DIR", "")
if (nzchar(CACHE_DIR) && !grepl("/$", CACHE_DIR)) {
  CACHE_DIR <- paste0(CACHE_DIR, "/")
}

IS_ED_LOADED <- env_boolean("MIMICWIZARD_IS_ED_LOADED", FALSE)
IS_NOTE_LOADED <- env_boolean("MIMICWIZARD_IS_NOTE_LOADED", FALSE)
PORT <- env_port("MIMICWIZARD_PORT", 3838L)

# Database configuration for hosted/full MIMIC-IV PostgreSQL database.
HOSTED_DBNAME <- env_value("MIMICWIZARD_HOSTED_DBNAME")
HOSTED_HOST <- env_value("MIMICWIZARD_HOSTED_HOST")
HOSTED_PORT <- env_port("MIMICWIZARD_HOSTED_PORT", 5432L)
HOSTED_USER <- env_value("MIMICWIZARD_HOSTED_USER")
HOSTED_PASSWORD <- env_value("MIMICWIZARD_HOSTED_PASSWORD")

# Database configuration for a local or externally hosted MIMIC-IV demo database.
DEMO_DBNAME <- env_value("MIMICWIZARD_DEMO_DBNAME", "postgres")
DEMO_HOST <- env_value("MIMICWIZARD_DEMO_HOST", "localhost")
DEMO_PORT <- env_port("MIMICWIZARD_DEMO_PORT", 5432L)
DEMO_USER <- env_value("MIMICWIZARD_DEMO_USER", "postgres")
DEMO_PASSWORD <- env_value("MIMICWIZARD_DEMO_PASSWORD")



if (APPLICATION_MODE == "INIT_DEMO" || APPLICATION_MODE == "DEMO") {
  DATABASE_MODE <- "DEMO"
} else{
  DATABASE_MODE <- "HOSTED"
}

if (!INTERACTIVE && DATABASE_MODE == "HOSTED") {
  missing_hosted_values <- c(
    MIMICWIZARD_HOSTED_DBNAME = HOSTED_DBNAME,
    MIMICWIZARD_HOSTED_HOST = HOSTED_HOST,
    MIMICWIZARD_HOSTED_USER = HOSTED_USER,
    MIMICWIZARD_HOSTED_PASSWORD = HOSTED_PASSWORD
  )
  missing_hosted_values <- names(missing_hosted_values)[!nzchar(missing_hosted_values)]
  if (length(missing_hosted_values) > 0L) {
    stop(
      sprintf("Hosted mode requires: %s.", paste(missing_hosted_values, collapse = ", ")),
      call. = FALSE
    )
  }
}

CONFIG <- list(
  INTERACTIVE = INTERACTIVE,
  APPLICATION_MODE = APPLICATION_MODE,
  CACHE_DIR = CACHE_DIR,
  PORT = PORT,
  DATABASE_MODE = DATABASE_MODE,
  IS_ED_LOADED = IS_ED_LOADED,
  IS_NOTE_LOADED = IS_NOTE_LOADED,
  HOSTED_DBNAME = HOSTED_DBNAME,
  HOSTED_HOST = HOSTED_HOST,
  HOSTED_PORT = HOSTED_PORT,
  HOSTED_USER = HOSTED_USER,
  HOSTED_PASSWORD = HOSTED_PASSWORD,
  DEMO_DBNAME = DEMO_DBNAME,
  DEMO_HOST = DEMO_HOST,
  DEMO_PORT = DEMO_PORT,
  DEMO_USER = DEMO_USER,
  DEMO_PASSWORD = DEMO_PASSWORD
)
