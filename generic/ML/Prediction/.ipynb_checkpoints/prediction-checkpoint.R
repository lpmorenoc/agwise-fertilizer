# ==============================
# Load packages & init H2O
# ==============================
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  terra,
  tidyverse,
  ggplot2,
  sf,
  h2o,
  geodata
)

h2o.init()   # or configure as needed

# ==============================
# User settings
# ==============================
scenario   <- c("normal")   # could also be c("below","normal","above")
crop       <- "Maize"

nstart     <- 46
nend       <- 169
ninterval  <- 10

pstart     <- 10
pend       <- 75
pinterval  <- 5

model_crop <- "GBM_model_R_1749061096810_3"

# Ethiopia admin boundaries (level 2) – used for zone selection
eth <- geodata::gadm("ETH", level = 2, path = tempdir())

# Model path and output path
pathModel <- "/home/jovyan/shared-data/Data/Maize/Intermediate/grainwt/"
pathOut   <- file.path("~", "shared-data", "Data", crop, "result", "geoSpatial", "June6")

# Zones for prediction
zone_list <- c("Mirab Hararghe", "Misraq Harerge")

# ==============================
# Function: predict_NPKY
# ==============================
predict_NPKY <- function(scenario,
                         crop,
                         nstart, nend, ninterval,
                         pstart, pend, pinterval,
                         model_crop,
                         zone_list,
                         pathOut,
                         pathModel,
                         eth_zone) {
  
  # --------------------------
  # 1. Load trained H2O model
  # --------------------------
  model_path <- file.path(pathModel, model_crop)
  if (!file.exists(model_path)) {
    stop("Model file not found at: ", model_path)
  }
  
  uploaded_model <- h2o.loadModel(model_path)
  
  # --------------------------
  # 2. Define valid zone mask
  # --------------------------
  valid_zone <- eth_zone |>
    st_as_sf() |>
    dplyr::filter(NAME_2 %in% zone_list) |>
    terra::vect()
  
  # --------------------------
  # 3. Load scenario raster
  # --------------------------
  scenario_map <- switch(
    scenario,
    "below"  = "scenario_below.tif",
    "normal" = "scenario_normal.tif",
    "above"  = "scenario_above.tif",
    stop("Unknown scenario: ", scenario)
  )
  
  base_raster_path <- file.path("~",
                                "shared-data",
                                "Data",
                                "Maize",
                                "geoSpatial",
                                "geo_4ML_AOI",
                                "NPrate",
                                scenario_map)
  
  if (!file.exists(base_raster_path)) {
    stop("Scenario raster not found at: ", base_raster_path)
  }
  
  # Raster stack of covariates used by the ML model
  pred_stack <- terra::rast(base_raster_path) |>
    aggregate(fact = 4, fun = "mean")
  
  # Align projection, crop & mask to valid zone
  pred_stack <- pred_stack |>
    terra::project(valid_zone) |>
    terra::crop(valid_zone) |>
    terra::mask(valid_zone)
  
  # --------------------------
  # 4. Convert to points & build prediction data
  # --------------------------
  pred_vect <- terra::as.points(pred_stack)
  
  # Extract admin names for each point
  pts_zone <- terra::extract(eth_zone, pred_vect) |>
    as.data.frame() |>
    dplyr::select(NAME_2)
  
  # Coordinates for later spatial join
  pred_coord <- data.frame(terra::geom(pred_vect)) |>
    dplyr::select(x, y)
  
  # Main predictor data frame
  pred_df <- as.data.frame(pred_vect) |>
    dplyr::bind_cols(pts_zone)
  
  # --------------------------
  # 5. Generate N–P combinations
  # --------------------------
  np_grid <- expand.grid(
    n_rate2 = seq(nstart, nend, ninterval),
    p_rate2 = seq(pstart, pend, pinterval)
  )
  
  # --------------------------
  # 6. Loop over NP grid & predict yield
  # --------------------------
  final_df <- NULL
  
  for (i in seq_len(nrow(np_grid))) {
    message("Predicting for N = ", np_grid$n_rate2[i],
            ", P = ", np_grid$p_rate2[i],
            " (", i, "/", nrow(np_grid), ")")
    
    df_h2o <- pred_df |>
      dplyr::mutate(
        n_kgpha = np_grid$n_rate2[i],
        p_kgpha = np_grid$p_rate2[i]
      ) |>
      as.h2o()
    
    pr <- h2o.predict(uploaded_model, df_h2o) |>
      as.data.frame()
    
    colnames(pr) <- paste("yield",
                          np_grid$n_rate2[i],
                          np_grid$p_rate2[i],
                          sep = ".")
    
    if (is.null(final_df)) {
      final_df <- pr
    } else {
      final_df <- cbind(final_df, pr)
    }
  }
  
  # --------------------------
  # 7. Attach coordinates & save
  # --------------------------
  final_df_sp <- dplyr::bind_cols(pred_coord, final_df)
  
  if (!dir.exists(pathOut)) dir.create(pathOut, recursive = TRUE)
  
  out_file <- file.path(
    pathOut,
    paste0("Predicted_Hararge_july10_", crop, "_", scenario, ".rds")
  )
  
  saveRDS(final_df_sp, out_file)
  
  message("✅ Prediction completed successfully for scenario '",
          scenario, "'. Output: ", out_file)
  
  invisible(final_df_sp)
}


# ==============================
# Run for all scenarios
# ==============================
for (k in seq_along(scenario)) {
  message("=== Running scenario: ", scenario[k], " ===")
  predict_NPKY(
    scenario   = scenario[k],
    crop       = crop,
    nstart     = nstart, nend = nend, ninterval = ninterval,
    pstart     = pstart, pend = pend, pinterval = pinterval,
    model_crop = model_crop,
    zone_list  = zone_list,
    pathModel  = pathModel,
    pathOut    = pathOut,
    eth_zone   = eth
  )
}
