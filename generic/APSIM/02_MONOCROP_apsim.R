# Create APSIM experimental file using Remote Sensing data

# Introduction: 
# This script prepares .apsimx files for MONOCROP factorial simulations
# This script allows the creation of experimental files up to administrative level 2
# Authors : P.Moreno-Cadena, A. Carmona-Cabrero, S. Mkuhlani
# Credentials : AgWise, 2026
# Last modified May 08, 2026 


#################################################################################################################
## sourcing required packages and helper functions                                                                                 ##
#################################################################################################################

source(paste0(project_root, '/generic/APSIM/common_helpers_APSIM.R'))
source(paste0(project_root, '/generic/APSIM/helpers_APSIM_expfile.R'))

#' Process a single grid element experiment
#'
#' This function prepares and modifies an APSIMX experimental file for a single
#' spatial grid element (location). It sets up weather data, soil profile,
#' cultivar information, sowing dates, and optionally fertilizer schedules.
#'
#' @param i Integer. Index of the grid element (row in `coords`).
#' @param path.to.extdata Character. Path to external APSIM data directory.
#' @param path.to.temdata Character. Path to template APSIM data directory.
#' @param coords Data frame or list. Contains planting dates and coordinates.
#' @param zone Character. Administrative zone identifier.
#' @param level2 Character or NA. Optional administrative level 2 identifier.
#' @param filex_temp Character. Path to APSIMX template file.
#' @param clck Character vector. Start and end dates for simulation period.
#' @param varietyid Character. Cultivar name to assign in APSIM.
#' @param rep Character vector. Variables to report in APSIM output.
#' @param fix_crop_or_soil_parm List or vector. Parameters to adjust soil profile.
#' @param plant_dates Date vector or NULL. Planting dates; if NULL, taken from `coords`.
#' @param fertilizer Logical. Whether to include fertilizer management.
#' @param fertilizer_param Numeric vector of length 2. Fertilizer maximum and step values.
#'
#' @return None. Side effects: modifies APSIMX files in the output directory.
#' @export
#'
#' @examples
#' process_grid_element_experiment(
#'   i = 1,
#'   path.to.extdata = "extdata/",
#'   path.to.temdata = "temdata/",
#'   coords = my_coords,
#'   zone = "ZoneA",
#'   filex_temp = "maize.apsimx",
#'   clck = c("1985-01-01T00:00:00", "2020-12-31T00:00:00"),
#'   varietyid = "A_103",
#'   rep = "[Maize].Grain.Total.Wt*10 as Yield",
#'   fix_crop_or_soil_parm = list(),
#'   fertilizer = TRUE,
#'   fertilizer_param = c(100, 20)
#' )
process_grid_element_experiment <- function(i, path.to.extdata, path.to.temdata, coords,
                                            zone, level2 = NA, filex_temp, clck,
                                            varietyid, rep, fix_crop_or_soil_parm,
                                            plant_dates = NULL,
                                            fertilizer = FALSE,
                                            fertilizer_param) {
  
  # If planting dates are not provided, extract them from coords
  if (is.null(plant_dates) && !is.null(coords)) {
    plant_dates <- coords$planting_dates[[i]]
  }
  
  # Stop execution if planting dates are missing
  if (is.null(plant_dates)) {
    stop(paste("plant_dates is NULL for i =", i))
  }
  
  # Define output path for this grid element
  pathOUT <- define_pathOUT(
    path.to.extdata = path.to.extdata, i = i, zone = zone, level2 = level2)
  
  # Switch working directory to output path
  setwd(pathOUT)
  
  # Construct weather file name for this location
  met_file <- paste0(pathOUT, "/", 'wth_loc_', i, '.met')
  
  # Insert weather file into APSIMX experiment
  apsimx::edit_apsimx(filex_temp, 
                      src.dir = path.to.temdata,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Weather", 
                      value = met_file, 
                      overwrite = TRUE)
  
  # Adjust simulation start and end dates based on clock and weather file
  fix_start_end_dates(filex_temp = filex_temp, 
                      clck = clck, 
                      met_file = met_file, 
                      pathOUT = pathOUT)
  
  # Load soil profile for this grid element
  load(paste0(pathOUT, "/my_sol_", i, ".RData"))
  
  # Modify soil profile according to crop/soil parameters
  modified_soil <- adjust_soil_profile(soil_df = ex_profile$soil,
                                       modify = fix_crop_or_soil_parm)
  ex_profile$soil <- modified_soil
  
  # Replace soil profile in APSIMX experiment
  edit_apsimx_replace_soil_profile(filex_temp, 
                                   src.dir = pathOUT,
                                   wrt.dir = pathOUT,
                                   root = c("pd", "Base_one"), 
                                   soil.profile = ex_profile, 
                                   overwrite = TRUE)
  
  # Set cultivar name in sowing rule
  apsimx::edit_apsimx(filex_temp, 
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Manager",
                      manager.child = "SowingRule",
                      parm = "CultivarName",
                      value = varietyid,
                      overwrite = TRUE)
  
  # Add reporting variables to APSIM output
  apsimx::edit_apsimx(filex_temp,
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Report",
                      parm = "VariableNames", 
                      value = rep, 
                      verbose = TRUE, overwrite = TRUE)
  
  # Update sowing dates in APSIMX experiment
  edit_perm_sowdate(
    file_in  = filex_temp,
    file_out = filex_temp,   # overwrite in-place
    new_dates = plant_dates
  )
  
  # If fertilizer management is enabled, update fertilization parameters
  if (fertilizer) {
    edit_perm_fertilise(
      file_in = filex_temp, 
      file_out = filex_temp, 
      new_max = fertilizer_param[1], 
      new_step = fertilizer_param[2])
  }
}


#' Create APSIM factorial experiments using Remote Sensing data
#'
#' This function orchestrates the creation of APSIMX experimental files across
#' multiple spatial grid elements. It integrates remote sensing planting dates,
#' soil data, and management options to generate factorial simulations.
#'
#' @param country Character. Country code (e.g., "ZM" for Zimbabwe).
#' @param useCaseName Character. Name of the use case/project.
#' @param Crop Character. Crop name (e.g., "maize").
#' @param project_root Character. Root directory of the project.
#' @param AOI Logical. Whether to use Area of Interest workflow (default TRUE).
#' @param filex_temp Character. Path to APSIMX template file.
#' @param varietyid Character. Cultivar name to simulate.
#' @param zone Character. Administrative zone identifier.
#' @param level2 Character or NA. Optional administrative level 2 identifier.
#' @param fertilizer Logical. Whether to include fertilizer management.
#' @param fert_factorial Logical. Placeholder for factorial fertilizer option (currently unused).
#' @param fertilizer_param Numeric vector. Fertilizer parameters passed downstream.
#' @param template_df Data frame. Remote sensing template data for planting dates.
#' @param fert_grid_RS Logical. Placeholder for RS fertilizer grid option (currently unused).
#' @param rs_schedule_df Data frame. Remote sensing schedule with planting dates.
#' @param Forecast Logical. Whether to use forecast planting dates.
#' @param create_RS_schedule Logical. Whether to generate RS schedule from template.
#' @param fc_year Integer. Forecast year (default NA).
#' @param clck Character vector. Start and end dates for simulation period.
#' @param rep Character vector. Variables to report in APSIM output.
#' @param fix_crop_or_soil_parm List or vector. Parameters to adjust soil profile.
#' @param Soil_source Character. Source of soil data.
#' @param datasourcing_path Character. Path to data sourcing directory.
#'
#' @return None. Side effects: creates APSIMX experiment files for all grid elements.
#' @export
#'
#' @examples
#' apsimSpatialFactorial(
#'   country = "ZM",
#'   useCaseName = "MaizeStudy",
#'   Crop = "maize",
#'   project_root = "/project",
#'   filex_temp = "maize.apsimx",
#'   varietyid = "A_103",
#'   zone = "ZoneA",
#'   clck = c("1985-01-01T00:00:00", "2020-12-31T00:00:00"),
#'   rep = "[Maize].Grain.Total.Wt*10 as Yield",
#'   fix_crop_or_soil_parm = list(),
#'   Soil_source = "RS",
#'   datasourcing_path = "/data"
#' )

apsimSpatialFactorial <- function(country, useCaseName, Crop, project_root, AOI = TRUE, 
                                  filex_temp, 
                                  varietyid, zone, level2 = NA, 
                                  fertilizer = FALSE, fert_factorial = FALSE,
                                  fertilizer_param,
                                  template_df = NULL, fert_grid_RS = FALSE, 
                                  rs_schedule_df = NULL, 
                                  Forecast = FALSE, create_RS_schedule = FALSE,
                                  fc_year = NA, clck, rep, fix_crop_or_soil_parm,
                                  Soil_source, datasourcing_path) {
  
  # Print basic info for tracking progress
  print(paste("Variety:", varietyid, "Zone:", zone))
  
  # Create RS planting schedule if requested
  if (create_RS_schedule) {
    if (Forecast) {
      rs_schedule_df <- create_rs_schedule(template_df = template_df, fc_year = fc_year)
      template_df <- template_df %>% select(-c(q25, q50, q75))  # remove quantiles
    } else {
      rs_schedule_df <- create_rs_schedule(template_df = template_df)
      template_df <- template_df %>% select(-c(q25, q50, q75))
    }
  }  
  
  # Workflow branch: AOI (Area of Interest) vs GPS field data
  if (AOI) {
    # Ensure planting dates are available
    if (is.null(rs_schedule_df$planting_dates)) {
      stop("Currently, the workflow only works if RS planting dates are provided.")
    }
    
    # Get coordinates and planting dates for zone
    coords <- get_zone_coords_pdates(country, useCaseName, Crop, zone, Soil_source, 
                                     rs_schedule_df, datasourcing_path)
    
    # If not forecasting, set placeholder year
    if (!Forecast) {
      fc_year = 2000
    }
    
  } else {
    # Alternative workflow: use GPS field data
    GPS_fieldData <- readRDS(paste("/home/jovyan/agwise-datacuration/dataops/datacuration/Data/useCase_", 
                                   country, "_", useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    countryCoord$startingDate <- as.Date(countryCoord$plantingDate, "%Y-%m-%d") %m-% months(1)
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate", "startingDate")
    coords <- countryCoord
  } 
  
  # Create paths for external and template APSIM data
  path.to.extdata <- create_extdata_path_APSIM(
    project_root = project_root, country = country, useCaseName = useCaseName,
    Crop = Crop, varietyid = varietyid, AOI = AOI)
  
  path.to.temdata <- create_temdata_path_APSIM(
    project_root = project_root, country = country, useCaseName = useCaseName, 
    Crop = Crop)
  
  # Define sequence of grid element indices
  indices <- seq_len(nrow(coords))
  
  # Enable parallel processing
  plan_multisession(per_worker_gb = 5)
  
  # Run experiments in parallel across grid elements
  messages_list <- future_lapply(
    indices, 
    function(i) {
      # Start message
      start_msg <- paste("Start experiment:", i, "of", length(indices), "variety", varietyid)
      
      # Process single grid element experiment
      process_grid_element_experiment(i,
                                      path.to.extdata = path.to.extdata,
                                      path.to.temdata = path.to.temdata,
                                      coords = coords,
                                      zone = zone,
                                      level2 = level2,
                                      filex_temp = filex_temp,
                                      clck = clck,
                                      varietyid = varietyid,
                                      rep = rep,
                                      fix_crop_or_soil_parm = fix_crop_or_soil_parm,
                                      plant_dates = NULL,
                                      fertilizer = fertilizer,
                                      fertilizer_param = fertilizer_param)
      
      # End message
      end_msg <- paste("Finished experiment:", i, "of", length(indices), "variety", varietyid)
      
      c(start_msg, end_msg)
    },
    future.packages = packages_required,
    future.seed = TRUE
  )  
}


