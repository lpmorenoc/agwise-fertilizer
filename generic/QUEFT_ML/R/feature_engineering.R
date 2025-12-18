################################################################################
# R/feature_engineering.R
# Extract covariates at points and build ML-ready dataset
################################################################################

#' Extract covariates at trial locations
#' @param ds tibble with Longitude, Latitude
#' @param cov_stack SpatRaster of covariates
#' @return tibble of covariates at point locations
extract_covariates_at_points <- function(ds, cov_stack) {

  pts <- terra::vect(
    ds[, c("Longitude","Latitude")],
    geom = c("Longitude","Latitude"),
    crs  = "EPSG:4326"
  )

  cov_at_pts <- terra::extract(cov_stack, pts)
  cov_at_pts <- dplyr::as_tibble(cov_at_pts) %>%
    dplyr::select(-ID)

  cov_at_pts
}

#' Build ML data frame from trials + covariates
#' @param ds trials tibble with Y, N, P, K
#' @param cov_at_pts covariate tibble (one row per trial)
#' @return tibble suitable for ML training
build_ml_table <- function(ds, cov_at_pts) {

  ml_df <- dplyr::bind_cols(
    ds %>% dplyr::select(Y, N, P, K),
    cov_at_pts
  ) %>%
    tidyr::drop_na()

  if (nrow(ml_df) < 20) {
    stop("Insufficient records for ML after extraction: ", nrow(ml_df))
  }

  ml_df
}
################################################################################
