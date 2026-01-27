################################################################################
# R/raster_predict_optimized.R
# Efficient H2O-based raster prediction
################################################################################

#' Predict raster yield using H2O model and fixed NPK
#' @param cov_stack SpatRaster of covariates (no N,P,K yet)
#' @param mdl H2O model
#' @param Nval numeric, fertilizer N rate
#' @param Pval numeric, fertilizer P rate
#' @param Kval numeric, fertilizer K rate
#' @param out_file path to GeoTIFF output
#' @return SpatRaster of predicted yield
predict_raster_h2o <- function(cov_stack, mdl, Nval, Pval, Kval, out_file) {

  message(">>> Predicting raster (N=",Nval,", P=",Pval,", K=",Kval,") ...")

  v  <- terra::values(cov_stack, mat = TRUE)
  df <- as.data.frame(v)
  df$N <- Nval
  df$P <- Pval
  df$K <- Kval

  preds <- as.vector(h2o::h2o.predict(mdl, h2o::as.h2o(df)))

  r_out <- cov_stack[[1]]
  terra::values(r_out) <- preds

  terra::writeRaster(r_out, out_file, overwrite = TRUE)
  message("    ✓ Wrote: ", out_file)
  r_out
}
################################################################################
