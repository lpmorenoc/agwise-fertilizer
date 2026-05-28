# Create weather and soil files in APSIM format

# Introduction: 
# This script allows the creation of weather and soil files up to administrative level 2
# Authors : P. Moreno-Cadena , A. Carmona-Cabrero, S. Mkuhlani
# Credentials : AgWise, 2026
# Last modified May 1, 2026 

#################################################################################################################
## sourcing required packages and helper functions                                                                                 ##
#################################################################################################################

source(paste0(project_root, '/generic/APSIM/common_helpers_APSIM.R'))
source(paste0(project_root, '/generic/APSIM/helpers_readGeo_CM_zone_APSIM.R'))

#' Create APSIM weather and soil files for one location
#'
#' Generates APSIM-compatible weather (.met) and soil (.RData) files for a single
#' spatial location based on climate and soil datasets.
#'
#' @param i Integer. Index of the location/folder ID.
#' @param country Character. Country name.
#' @param path.to.extdata Character. Output directory for APSIM weather/soil files.
#' @param path.to.temdata Character. Directory containing template soil data.
#' @param TemperatureMax Data frame. Maximum temperature data.
#' @param TemperatureMin Data frame. Minimum temperature data.
#' @param SolarRadiation Data frame. Solar radiation data.
#' @param Rainfall Data frame. Rainfall data.
#' @param coords Data frame. Coordinates and metadata for locations.
#' @param Soil Data frame. Soil data information.
#' @param AOI Logical. TRUE if data is for Area of Interest, FALSE for trial sites.
#' @param varietyid Character. Crop variety identifier (not used in this function).
#' @param zone Character. Administrative zone name.
#' @param level2 Character or NA. Administrative level 2 name.
#' @param Depth Numeric vector. Soil depth layers (default: c(5,15,30,60,100,200)).
#'
#' @return Character string confirming file creation.
#' @export
#'
#' @examples
#' process_grid_element(1, "Kenya", "extdata/", "temdata/", Tmax, Tmin, Rad, Rain, coords, Soil, TRUE, "Maize", "Kiambu")

                 
process_grid_element <- function(i, country, path.to.extdata, path.to.temdata,
                                 TemperatureMax, TemperatureMin, SolarRadiation, Rainfall,
                                 coords, Soil, AOI, varietyid, zone, level2 = NA,
                                 Depth = c(5, 15, 30, 60, 100, 200)) {
  
  # Define output path for this location
  pathOUT <- define_pathOUT(path.to.extdata = path.to.extdata, i = i, zone = zone, level2 = level2)
  setwd(pathOUT)
  
  ### Weather file creation ###
  # Filter climate data for this location
  TemperatureMax_i <- filter_by_coord(TemperatureMax, coords, i)
  TemperatureMin_i <- filter_by_coord(TemperatureMin, coords, i)
  SolarRadiation_i <- filter_by_coord(SolarRadiation, coords, i)
  Rainfall_i <- filter_by_coord(Rainfall, coords, i)
  
  # Location name for header
  location <- unique(TemperatureMax_i$NAME_2)
  
  # Pivot datasets into APSIM format
  TemperatureMax_i <- pivot_weather(TemperatureMax_i, value_name = "maxt", AOI = AOI)
  TemperatureMin_i <- pivot_weather(TemperatureMin_i, value_name = "mint", AOI = AOI)
  SolarRadiation_i <- pivot_weather(SolarRadiation_i, value_name = "radn", AOI = AOI)
  Rainfall_i <- pivot_weather(Rainfall_i, value_name = "rain", AOI = AOI)
  
  # Build APSIM weather object and add header
  tst <- build_APSIM_WTH(maxt = TemperatureMax_i, mint = TemperatureMin_i, radn = SolarRadiation_i, rain = Rainfall_i)
  tst <- get_APSIM_WTH_header(tst = tst, location = location)
  
  # Write APSIM weather file
  apsimx::write_apsim_met(tst, wrt.dir = pathOUT, filename = paste0('wth_loc_', i, '.met'))
  
  ### Soil file creation ###
  lon_i <- as.numeric(coords[i, 1])
  lat_i <- as.numeric(coords[i, 2])
  
  # Extract soil variables by depth
  LL15 <- get_depth_var(Soil, lon_i, lat_i, "PWP", Depth, scale = 1)
  DUL <- get_depth_var(Soil, lon_i, lat_i, "FC", Depth, scale = 1)
  SAT <- get_depth_var(Soil, lon_i, lat_i, "SWS", Depth, scale = 1)
  SKS <- get_depth_var(Soil, lon_i, lat_i, "KS", Depth, scale = 10)
  SSS <- get_depth_var(Soil, lon_i, lat_i, "KS", Depth, scale = 10, round_digits = 1)
  BDM <- get_depth_var(Soil, lon_i, lat_i, get_var_name("bdod", Depth), Depth)
  LOC <- get_depth_var(Soil, lon_i, lat_i, get_var_name("soc", Depth), Depth, scale = 10)
  LCL <- get_depth_var(Soil, lon_i, lat_i, get_var_name("clay", Depth), Depth)
  LSI <- get_depth_var(Soil, lon_i, lat_i, get_var_name("silt", Depth), Depth)
  Sand <- get_depth_var(Soil, lon_i, lat_i, get_var_name("sand", Depth), Depth)
  LNI <- get_depth_var(Soil, lon_i, lat_i, get_var_name("nitrogen", Depth), Depth, scale = 10)
  LHW <- get_depth_var(Soil, lon_i, lat_i, get_var_name("phh2o", Depth), Depth)
  LDR <- get_site_var(Soil, lon_i, lat_i, "LDR")
  CEC <- get_depth_var(Soil, lon_i, lat_i, get_var_name("cec", Depth), Depth)
  
  # Compute texture and soil parameters
  max_depths <- depths_to_numeric(Depth)
  texture_list <- get_texture_params(LCL, LSI, Sand, max_depths)
  
  # Load soil template
  load(paste(path.to.temdata, "my_sol.RData", sep = "/"))
  
  # Replace template values with extracted soil data
  ex_profile$soil$LL15 <- LL15
  ex_profile$soil$DUL <- DUL
  ex_profile$soil$SAT <- min(SAT, 0.697)  # cap saturation to avoid errors
  ex_profile$soil$AirDry <- LL15 - 0.02
  ex_profile$soil$KS <- replace_zero_with_previous(SSS)
  ex_profile$soil$BD <- BDM
  ex_profile$soil$Carbon <- LOC
  ex_profile$soil$ParticleSizeClay <- LCL
  ex_profile$soil$ParticleSizeSilt <- LSI
  ex_profile$soil$ParticleSizeSand <- 100 - LCL - LSI
  ex_profile$soil$Nitrogen <- LNI * 100
  ex_profile$soil$PH <- LHW
  ex_profile$soil$CEC <- CEC
  ex_profile$soilwat$Salb <- texture_list$ALB
  ex_profile$soilwat$CN2Bare <- texture_list$LRO
  
  # Save soil profile
  save(ex_profile, file = paste0(pathOUT, "/my_sol_", i, ".RData"))
  
  return(sprintf("Created files for location %s", i))
}

  
#' Read geo-based crop model data and transform to APSIM format
#'
#' Reads weather and soil data for a given country/use case, filters by zone/level2,
#' and generates APSIM-compatible weather and soil files for each location.
#'
#' @param country Character. Country name.
#' @param useCaseName Character. Use case name.
#' @param Crop Character. Crop name.
#' @param project_root Character. Root directory of the project.
#' @param AOI Logical. TRUE for Area of Interest, FALSE for trial sites.
#' @param season Integer. Season number (default 1).
#' @param zone Character. Administrative level 1 zone.
#' @param level2 Character or NA. Administrative level 2.
#' @param varietyid Character. Crop variety identifier.
#' @param pathIn_zone Logical. TRUE if input data organized by zone.
#' @param Depth Numeric vector. Soil depth layers.
#' @param Forecast Logical. TRUE if forecast data is used.
#' @param fc_month Integer or NULL. Forecast initialization month.
#' @param fc_year Integer or NULL. Forecast year.
#' @param season_length_months Integer or NULL. Forecast season length.
#' @param datasourcing_path Character. Path to data sourcing directory.
#'
#' @return None. Side effects: creates APSIM weather and soil files.
#' @export
#'
#' @examples
#' readGeo_CM_zone_APSIM("Kenya", "KALRO", "Maize", project_root="/project", AOI=TRUE, zone="Kiambu", varietyid="MaizeVar")

readGeo_CM_zone_APSIM <- function(country, useCaseName, Crop, project_root,
                                  AOI = FALSE, season = 1, zone,
                                  level2 = NA, varietyid,
                                  pathIn_zone = TRUE, Depth = c(5,15,30,60,100,200),
                                  Forecast = FALSE, fc_month = NULL, fc_year = NULL,
                                  season_length_months = NULL,
                                  datasourcing_path = "~/agwise-datasourcing/dataops/datasourcing") {
  
  # Define general input path depending on Forecast flag
  if (!Forecast) {
    general_pathIn <- paste0(datasourcing_path, "/Data/useCase_", country, "_",
                             useCaseName, "/", Crop, "/result/geo_4cropModel")
  } else {
    general_pathIn <- paste0(project_root, "/Data/useCase_", country, "_",
                             useCaseName, "/", Crop, "/transform/FC")
    # Retrieve forecast data (external helper)
    get_bc_forecast_data(project_root, country, useCaseName, Crop, zone, country_code,
                         init_month_user = fc_month, season_length_months = season_length_months,
                         forecast_year = fc_year)
  }
  
  # Define input path for zone/level2
  pathIn <- define_pathIn(general_pathIn, level2, zone, pathIn_zone, Forecast, fc_year, fc_month)
  
  # Define file paths for rainfall, radiation, temperature, soil depending on AOI
  if (AOI) {
    Rainfall_file <- paste0(pathIn, "Rainfall_Season_", season, "_PointData_AOI.RDS")
    SolarRadiation_file <- paste0(pathIn, "solarRadiation_Season_", season, "_PointData_AOI.RDS")
    TemperatureMax_file <- paste0(pathIn, "temperatureMax_Season_", season, "_PointData_AOI.RDS")
    TemperatureMin_file <- paste0(pathIn, "temperatureMin_Season_", season, "_PointData_AOI.RDS")
    # Soil file: ISDA if Depth length == 2, else ISRIC
    if (length(Depth) == 2) {
      Soil_file <- paste0(pathIn, "ISDA_SoilDEM_PointData_AOI_profile.RDS")
      if (!file.exists(Soil_file)) get_ISDA_soilRDS(country, useCaseName, Crop)
    } else {
      Soil_pathIn <- paste0(datasourcing_path, "/Data/useCase_", country, "_",
                            useCaseName, "/", Crop, "/result/geo_4cropModel/", zone, "/")
      Soil_file <- paste0(Soil_pathIn, "SoilDEM_PointData_AOI_profile.RDS")
    }
  } else {
    # Trial sites file paths
    Rainfall_file <- paste0(pathIn, "Rainfall_PointData_trial.RDS")
    SolarRadiation_file <- paste0(pathIn, "solarRadiation_PointData_trial.RDS")
    TemperatureMax_file <- paste0(pathIn, "temperatureMax_PointData_trial.RDS")
    TemperatureMin_file <- paste0(pathIn, "temperatureMin_PointData_trial.RDS")
    if (length(Depth) == 2) {
      Soil_file <- paste0(pathIn, "ISDA_SoilDEM_PointData_trial_profile.RDS")
      if (!file.exists(Soil_file)) get_ISDA_soilRDS(country, useCaseName, Crop)
    } else {
      Soil_pathIn <- paste0(datasourcing_path, "/Data/useCase_", country, "_",
                            useCaseName, "/", Crop, "/result/geo_4cropModel/", zone, "/")
      Soil_file <- paste0(Soil_pathIn, "SoilDEM_PointData_trial_profile.RDS")
    }
  }
  
  # Read and filter RS datasets
  Rainfall <- read_and_filter(Rainfall_file, zone, level2)
  SolarRadiation <- read_and_filter(SolarRadiation_file, zone, level2)
  TemperatureMax <- read_and_filter(TemperatureMax_file, zone, level2)
  TemperatureMin <- read_and_filter(TemperatureMin_file, zone, level2)
  Soil <- read_and_filter(Soil_file, zone, level2)
  
  # Apply forecast corrections if needed
  if (Forecast) {
    Rainfall <- fix_forecast_dataset(Rainfall, "Rainfall", fc_year, fc_month)
    SolarRadiation <- fix_forecast_dataset(SolarRadiation, "SolarRadiation", fc_year, fc_month)
    TemperatureMax <- fix_forecast_dataset(TemperatureMax, "TemperatureMax", fc_year, fc_month)
    TemperatureMin <- fix_forecast_dataset(TemperatureMin, "TemperatureMin", fc_year, fc_month)
  }
  
  # Get metadata and filter soil/weather by metadata
  metaData <- get_metadata(AOI, Rainfall, Soil)
  Soil <- filter_soil_by_meta(Soil, metaData)
  
  # Define output paths for APSIM data
  path.to.extdata <- create_extdata_path_APSIM(project_root, country, useCaseName, Crop, varietyid, AOI)
  path.to.temdata <- create_temdata_path_APSIM(project_root, country, useCaseName, Crop)
  
  # Define coordinates (unique for AOI, full metadata otherwise)
  coords <- if (AOI) unique(metaData[, c("longitude", "latitude")]) else metaData
  
  # Sequence of location indices
  indices <- seq_len(nrow(as.matrix(coords)))
  
  # Initialize log file
  log_file <- paste(path.to.extdata, "01_progress_log_readGeo_CM_APSIM.txt", sep = '/')
  if (file.exists(log_file)) file.remove(log_file)
  
  # Run experiments in parallel
  plan_multisession(per_worker_gb = 3)
  messages_list <- future_lapply(indices, function(i) {
    start_msg <- paste("Start experiment:", i, "of", length(indices), "variety", varietyid)
    
    process_grid_element(i, country, path.to.extdata, path.to.temdata,
                         TemperatureMax, TemperatureMin, SolarRadiation, Rainfall,
                         coords, Soil, AOI, varietyid, zone, level2, Depth)
    
    end_msg <- paste("Finished experiment:", i, "of", length(indices), "variety", varietyid)
    c(start_msg, end_msg)
  }, future.packages = packages_required, future.seed = TRUE)
}

