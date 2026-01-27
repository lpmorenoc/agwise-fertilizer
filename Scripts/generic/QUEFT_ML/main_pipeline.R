################################################################################
# main_pipeline.R
# End-to-end ML (H2O AutoML) -> ML maps -> Reverse QUEFTS -> Forward QUEFTS
################################################################################

rm(list = ls())

# 1. Load config and libraries -------------------------------------------------
source("config.R")
source(file.path("R","load_libraries.R"))
load_required_packages()

# 2. Source all modules --------------------------------------------------------
source(file.path("R","data_import.R"))
source(file.path("R","utils_covariates.R"))
source(file.path("R","feature_engineering.R"))
source(file.path("R","model_training_automl.R"))
source(file.path("R","raster_predict_optimized.R"))
source(file.path("R","utils_terra.R"))
source(file.path("R","parallel_backend.R"))
source(file.path("R","reverse_quefts_solver.R"))
source(file.path("R","forward_quefts_optimizer.R"))
source(file.path("R","plotting.R"))  # optional

# 3. Parallel backend ----------------------------------------------------------
setup_parallel(use_parallel = USE_PARALLEL)

# 4. Load and clean trials -----------------------------------------------------
message("=== STEP 1: Load & clean trial data ===")
ds <- load_trial_data(EXP_CSV)
readr::write_csv(ds, file.path(OUT_DIR,"ds_clean.csv"))

# 5. Covariates ---------------------------------------------------------------
message("=== STEP 2: Build covariate stack ===")
cov_out   <- download_and_build_covariates(COUNTRY, COV_PATH, wc_res = 10)
cov_stack <- cov_out$stack
terra::writeRaster(cov_stack, file.path(OUT_DIR,"covariate_stack.tif"), overwrite = TRUE)

# 6. Feature engineering -------------------------------------------------------
message("=== STEP 3: Extract covariates at trial locations ===")
cov_at_pts <- extract_covariates_at_points(ds, cov_stack)
ml_df      <- build_ml_table(ds, cov_at_pts)
readr::write_csv(ml_df, file.path(OUT_DIR,"ml_df.csv"))

# 7. H2O AutoML training ------------------------------------------------------
message("=== STEP 4: Train H2O AutoML model ===")
init_h2o(H2O_NTHREADS, H2O_MAX_MEM)
on.exit(shutdown_h2o(), add = TRUE)

aml_res <- train_yield_model_automl(
  ml_df,
  max_runtime_secs = H2O_AUTOML_RUNTIME,
  max_models       = H2O_AUTOML_MAX_MODELS,
  seed             = H2O_AUTOML_SEED,
  sort_metric      = H2O_AUTOML_SORT_METRIC
)
mdl <- aml_res$model
h2o::h2o.saveModel(mdl, path = OUT_DIR, force = TRUE)

# 8. ML maps: control & max ----------------------------------------------------
message("=== STEP 5: Generate ML maps (control & max) ===")
Y_control_ml <- predict_raster_h2o(
  cov_stack, mdl,
  Nval = 0, Pval = 0, Kval = 0,
  out_file = file.path(OUT_DIR, paste0("Y_control_ML_", CROP, ".tif"))
)

Y_max_ml <- predict_raster_h2o(
  cov_stack, mdl,
  Nval = N_MAX, Pval = P_MAX, Kval = K_MAX,
  out_file = file.path(OUT_DIR, paste0("Y_max_ML_", CROP, ".tif"))
)

# 9. Build Ya raster -----------------------------------------------------------
message("=== STEP 6: Build spatial attainable yield (Ya) ===")
if (USE_SPATIAL_YA_FROM_ML_MAX) {
  Ya_r <- Y_max_ml
} else {
  Ya_r <- Y_max_ml
  Ya_r[] <- FALLBACK_YA
}
terra::writeRaster(Ya_r, file.path(OUT_DIR,"Ya_spatial.tif"), overwrite = TRUE)

# 10. Reverse QUEFTS (INS/IPS/IKS) --------------------------------------------
message("=== STEP 7: Reverse QUEFTS (INS,IPS,IKS) ===")
rev_res <- solve_reverse_quefts_rasters(
  Yc_r        = Y_control_ml,
  Ymax_r      = Y_max_ml,
  Ya_r        = Ya_r,
  Nmax        = N_MAX,
  Pmax        = P_MAX,
  Kmax        = K_MAX,
  crop        = CROP,
  chunk_size  = REVERSE_CHUNK_SIZE,
  use_parallel= USE_PARALLEL
)

INS_r <- rev_res$INS
IPS_r <- rev_res$IPS
IKS_r <- rev_res$IKS

terra::writeRaster(INS_r, file.path(OUT_DIR,"INS_rev.tif"), overwrite = TRUE)
terra::writeRaster(IPS_r, file.path(OUT_DIR,"IPS_rev.tif"), overwrite = TRUE)
terra::writeRaster(IKS_r, file.path(OUT_DIR,"IKS_rev.tif"), overwrite = TRUE)

# 11. Forward QUEFTS + NUE optimization ---------------------------------------
message("=== STEP 8: Forward QUEFTS + NUE optimization ===")
fwd_res <- forward_quefts_optimize(
  INS_r = INS_r, IPS_r = IPS_r, IKS_r = IKS_r,
  Ya_r  = Ya_r,
  N_rates = N_RATES,
  P_rates = P_RATES,
  K_rates = K_RATES,
  crop      = CROP,
  NUE_alpha = NUE_ALPHA,
  use_parallel  = FORWARD_USE_PARALLEL,
  show_progress = FORWARD_PROGRESS
)

best_yield <- fwd_res$best_yield
bestN      <- fwd_res$bestN
bestP      <- fwd_res$bestP
bestK      <- fwd_res$bestK
Y0         <- fwd_res$Y0
yield_gap  <- fwd_res$yield_gap

terra::writeRaster(best_yield, file.path(OUT_DIR,"best_yield.tif"), overwrite = TRUE)
terra::writeRaster(bestN,      file.path(OUT_DIR,"bestN.tif"),      overwrite = TRUE)
terra::writeRaster(bestP,      file.path(OUT_DIR,"bestP.tif"),      overwrite = TRUE)
terra::writeRaster(bestK,      file.path(OUT_DIR,"bestK.tif"),      overwrite = TRUE)
terra::writeRaster(Y0,         file.path(OUT_DIR,"Y0_forwardQUEFTS.tif"), overwrite = TRUE)
terra::writeRaster(yield_gap,  file.path(OUT_DIR,"Yield_gap.tif"),       overwrite = TRUE)

# 12. Summary ------------------------------------------------------------------
cat("
=== PIPELINE SUMMARY ===
")
cat(" Country: ", COUNTRY, "
")
cat(" Crop:    ", CROP, "
")
cat(" Trials (clean): ", nrow(ds), "
")
cat(" ML rows:        ", nrow(ml_df), "
")
cat(" N combos:       ", length(N_RATES), "
")
cat(" P combos:       ", length(P_RATES), "
")
cat(" K combos:       ", length(K_RATES), "
")
cat(" Output folder:  ", OUT_DIR, "

")
################################################################################
