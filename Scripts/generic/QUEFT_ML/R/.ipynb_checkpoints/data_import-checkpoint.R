################################################################################
# R/data_import.R
# Trial data import and basic cleaning
################################################################################

#' Detect column by candidate names (case-insensitive)
#' @param df data.frame
#' @param candidates character vector of possible column names
#' @return single column name or NULL
detect_column <- function(df, candidates) {
  nm  <- names(df)
  hit <- nm[tolower(nm) %in% tolower(candidates)]
  if (length(hit)) hit[1] else NULL
}

#' Load and harmonize trial data
#' @param exp_csv Path to CSV file with field experiments
#' @return cleaned tibble with columns: Longitude, Latitude, Y, N, P, K, TLID
load_trial_data <- function(exp_csv) {

  if (!file.exists(exp_csv)) {
    stop("Trial CSV not found: ", exp_csv)
  }

  raw <- read.csv(exp_csv)

  col_map <- list(
    lon   = c("longitude","lon","x","Longitude"),
    lat   = c("latitude","lat","y","Latitude"),
    yield = c("Grain_yield.kg.ha.","yield","GY","Grain_yield"),
    N     = c("N","N_fertilizer","N_rate","N_kg_ha"),
    P     = c("P","P_fertilizer","P_rate","P_kg_ha"),
    K     = c("K","K_fertilizer","K_rate","K_kg_ha")
  )

  lonc <- detect_column(raw, col_map$lon)
  latc <- detect_column(raw, col_map$lat)
  yc   <- detect_column(raw, col_map$yield)

  if (is.null(lonc) || is.null(latc) || is.null(yc)) {
    stop("Could not find required lon/lat/yield columns.")
  }

  Nc <- detect_column(raw, col_map$N)
  Pc <- detect_column(raw, col_map$P)
  Kc <- detect_column(raw, col_map$K)

  ds <- raw %>%
    dplyr::rename(
      Longitude = !!lonc,
      Latitude  = !!latc,
      Y         = !!yc,
      N         = !!Nc,
      P         = !!Pc,
      K         = !!Kc
    ) %>%
    dplyr::mutate(
      Longitude = as.numeric(Longitude),
      Latitude  = as.numeric(Latitude)
    ) %>%
    dplyr::filter(
      !is.na(Longitude), !is.na(Latitude),
      dplyr::between(Longitude,-180,180),
      dplyr::between(Latitude, -90, 90)
    ) %>%
    dplyr::mutate(
      TLID = paste0(round(Longitude, 3), "_", round(Latitude, 3))
    )

  ds
}
################################################################################
