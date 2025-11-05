################################################################################
########################### CONFIGURATION FILE #################################
################################################################################


# Allow user to choose database mode (recommended for local configuration : TRUE)

INTERACTIVE <- FALSE

# Force application mode, only if interactive is set to FALSE
# (accepted value: INIT_DEMO, DEMO, HOSTED)

APPLICATION_MODE <- "HOSTED"

# Repository where the application cache object are written
# Default "" create a cache folder in the application directory
# Shiny Server should have writing rights in this folder
# Need a closing / #/tmp/MIMICWizard/

CACHE_DIR <- ""

# Set to TRUE if you've loaded mimiciv_ed schema (if you're using mimiciv demo version, keep this to false)
# https://physionet.org/content/mimic-iv-ed/2.2/

IS_ED_LOADED <- FALSE

# Set to TRUE if you've loaded mimiciv_note schema (if you're using mimiciv demo version, keep this to false)
# https://www.physionet.org/content/mimic-iv-note/2.2/

IS_NOTE_LOADED <- FALSE


# Database configuration for hosted/full MIMIC-IV Postgres database

HOSTED_DBNAME = ""
HOSTED_HOST = ""
HOSTED_PORT = 
HOSTED_USER = ""
HOSTED_PASSWORD = ""

# Database configuration for local demo MIMIC-IV postgres database
# If you're using PostgreSQL Portable as recommended in the documentation you
# shouldn't have to change anything as its the default parameters

DEMO_DBNAME = "postgres"
DEMO_HOST = "localhost"
DEMO_PORT = 5432
DEMO_USER = "postgres"
DEMO_PASSWORD = ""



################################################################################
########################## END OF CONFIGURATION ################################
############################ DO NOT EDIT BELOW #################################
################################################################################



if (APPLICATION_MODE == "INIT_DEMO" || APPLICATION_MODE == "DEMO") {
  DATABASE_MODE <- "DEMO"
} else{
  DATABASE_MODE <- "HOSTED"
}

CONFIG <- list(
  INTERACTIVE = INTERACTIVE,
  APPLICATION_MODE = APPLICATION_MODE,
  CACHE_DIR = CACHE_DIR,
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
