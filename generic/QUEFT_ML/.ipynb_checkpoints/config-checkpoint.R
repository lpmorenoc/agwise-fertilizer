################################################################################
# config.R
# Central configuration for spatial QUEFTS–ML–AutoML pipeline
################################################################################

# ---- General paths and metadata ----
EXP_CSV  <- "Maize1.csv"     # Input experimental trials
COUNTRY  <- "Kenya"
CROP     <- "Maize"

OUT_DIR  <- file.path(getwd(), "spatial_outputs_revMLfwdQUEFTS")
COV_PATH <- file.path(OUT_DIR, "covariates")

dir.create(OUT_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(COV_PATH, recursive = TRUE, showWarnings = FALSE)

# ---- H2O AutoML settings ----
H2O_NTHREADS          <- -1
H2O_MAX_MEM           <- "16G"
H2O_AUTOML_RUNTIME    <- 3600   # seconds total runtime budget
H2O_AUTOML_MAX_MODELS <- 30     # max number of models in AutoML
H2O_AUTOML_SEED       <- 123
H2O_AUTOML_SORT_METRIC<- "RMSE" # "RMSE", "MAE", etc.

# ---- ML "max-fertilizer" scenario ----
N_MAX <- 150
P_MAX <- 60
K_MAX <- 90

# ---- Forward QUEFTS fertilizer grid ----
N_RATES <- seq(0, 180, by = 30)
P_RATES <- seq(0,  60, by = 10)
K_RATES <- seq(0, 120, by = 20)

# ---- NUE objective weight ----
NUE_ALPHA <- 0.3

# ---- Attainable yield (Ya) strategy ----
USE_SPATIAL_YA_FROM_ML_MAX <- TRUE
FALLBACK_YA                <- 15000  # kg/ha

# ---- Parallel / optimization settings ----
USE_PARALLEL            <- TRUE
REVERSE_CHUNK_SIZE      <- 50000   # cells per chunk for reverse QUEFTS
FORWARD_USE_PARALLEL    <- TRUE    # parallelize over NPK combos
FORWARD_PROGRESS        <- TRUE

set.seed(123)
options(stringsAsFactors = FALSE)
################################################################################
