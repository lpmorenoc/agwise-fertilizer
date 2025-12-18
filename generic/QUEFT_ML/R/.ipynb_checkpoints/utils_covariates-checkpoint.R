################################################################################
# R/utils_covariates.R
# Download, build, and manage covariate rasters
################################################################################

#' Download and build covariate stack for a country
#' @param country Country name as used by geodata / rnaturalearth
#' @param cov_path Folder to save intermediate rasters
#' @param wc_res WorldClim resolution (in arc-minutes), e.g. 10
#' @return list(stack = SpatRaster, boundary = sf polygon)
download_and_build_covariates <- function(country, cov_path, wc_res = 10) {

  dir.create(cov_path, showWarnings = FALSE, recursive = TRUE)
  message(">>> Downloading national boundary and covariates for: ", country)

  # Country boundary
  country_sf <- tryCatch({
    g <- geodata::gadm(country, level = 0, path = cov_path)
    if (!is.null(g$sf)) g$sf else sf::st_as_sf(g$sp)
  }, error = function(e) {
    message("  GADM failed; using rnaturalearth: ", e$message)
    rnaturalearth::ne_countries(scale = "medium", country = country, returnclass = "sf")
  })
  country_vect <- terra::vect(country_sf)

  # WorldClim BIO
  wc <- tryCatch({
    r <- geodata::worldclim_country(country, var="bio", res=wc_res, path=cov_path)
    if (!is.null(r$bio)) r$bio else geodata::worldclim_global("bio", res=wc_res, path=cov_path)
  }, error = function(e) {
    geodata::worldclim_global("bio", res=wc_res, path=cov_path)
  })
  wc <- terra::mask(terra::crop(wc, country_vect), country_vect)

  # Soil variables
  soil_vars <- c("nitrogen","phosphorus","potassium","soc","ph",
                 "clay","silt","sand","cec","bd","ec")

  soil_layers <- list()
  message("  Downloading soil layers...")
  for (v in soil_vars) {
    try({
      r <- geodata::soil_world(var = v, depth = 5, path = cov_path)
      r <- terra::mask(terra::crop(terra::resample(r, wc), country_vect), country_vect)
      soil_layers[[v]] <- r
      message("   ✓ ", v)
    }, silent = TRUE)
  }

  # Elevation & terrain
  elev <- tryCatch({
    r <- geodata::elevation_30s(country, path = cov_path)
    terra::mask(terra::crop(terra::resample(r, wc), country_vect), country_vect)
  }, error = function(e) NULL)

  slope  <- if (!is.null(elev)) terra::terrain(elev, "slope","degrees") else NULL
  aspect <- if (!is.null(elev)) terra::terrain(elev, "aspect","degrees") else NULL

  ras_list <- c(soil_layers, list(elev = elev, slope = slope, aspect = aspect, wc = wc))
  ras_list <- purrr::discard(ras_list, is.null)

  cov_stack <- terra::rast(ras_list)
  names(cov_stack) <- make.names(names(cov_stack))

  # Drop empty layers
  keep <- sapply(1:terra::nlyr(cov_stack), function(i) {
    any(!is.na(terra::values(cov_stack[[i]])))
  })
  cov_stack <- cov_stack[[which(keep)]]

  out_file <- file.path(cov_path, paste0("covariate_stack_", gsub(" ","_",country), ".tif"))
  terra::writeRaster(cov_stack, out_file, overwrite = TRUE)
  message("  Covariate stack saved: ", out_file)

  list(stack = cov_stack, boundary = country_sf)
}
################################################################################
