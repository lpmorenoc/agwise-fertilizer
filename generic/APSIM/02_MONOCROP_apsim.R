# Create APSIM experimental file using Remote Sensing data

# Introduction: 
# This script prepares .apsimx files for MONOCROP factorial simulations
# This script allows the creation of experimental files up to administrative level 2
# Authors : P.Moreno-Cadena, A. Carmona-Cabrero, A. Sila, S. Mkuhlani, E.Bendito Garcia 
# Credentials : AgWise, 2026
# Last modified April 09, 2026 


#################################################################################################################
## sourcing required packages and helper functions                                                                                 ##
#################################################################################################################

source(paste0(project_root, '/generic/APSIM/common_helpers_APSIM.R'))
source(paste0(project_root, '/generic/APSIM/helpers_APSIM_expfile.R'))


process_grid_element_experiment <- function(i,path.to.extdata,path.to.temdata, coords,
                                            zone,level2=NA,filex_temp,clck,
                                            varietyid,rep,fix_crop_or_soil_parm,
                                            plant_dates = NULL,
                                            fertilizer = FALSE,
                                            fertilizer_param) {

    if (is.null(plant_dates) && !is.null(coords)) {
    plant_dates <- coords$planting_dates[[i]]
  }
  
  if (is.null(plant_dates)) {
    stop(paste("plant_dates is NULL for i =", i))
  }
  
  # Working path (each point)
  pathOUT <- define_pathOUT(
    path.to.extdata = path.to.extdata, i = i, zone = zone, level2 = level2)
  
  # Switch to working path for write/read ops
  setwd(pathOUT)
  
  met_file <- paste0(pathOUT,"/", 'wth_loc_',i,'.met')



  #Define the weather data for each location
  apsimx::edit_apsimx(filex_temp, 
                      src.dir = path.to.temdata,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Weather", 
                      value = met_file, 
                      overwrite = TRUE)
  
  #Modify the number of years of the simulations based on available data and selected time period
  fix_start_end_dates(filex_temp = filex_temp, 
                      clck = clck, 
                      met_file = met_file, 
                      pathOUT = pathOUT)
  
  #Add the soil profile (ex_profile)
  
  load(paste0(pathOUT,"/my_sol_",i,".RData"))
  
  modified_soil <- adjust_soil_profile(soil_df = ex_profile$soil,
                                       modify = fix_crop_or_soil_parm)
  
  ex_profile$soil <- modified_soil
  
  edit_apsimx_replace_soil_profile(filex_temp, 
                                   src.dir = pathOUT,
                                   wrt.dir = pathOUT,
                                   root = c("pd", "Base_one"), 
                                   soil.profile = ex_profile, 
                                   overwrite = TRUE)
  
  
  apsimx::edit_apsimx(filex_temp, 
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Manager",
                      manager.child = "SowingRule",
                      parm = "CultivarName", ## This is for cultivar
                      value = varietyid,
                      overwrite = TRUE)

  for (report in rep){
  apsimx::edit_apsimx(filex_temp,
                      src.dir = pathOUT,
                      wrt.dir = pathOUT,
                      root = c("pd", "Base_one"),
                      node = "Report",
                      parm = "VariableNames", 
                      value = rep, 
                      verbose = TRUE, overwrite = TRUE)
  }
  
  edit_perm_sowdate(
    file_in  = filex_temp,
    file_out = filex_temp,                 # overwrite in-place
    new_dates = plant_dates
  )
  
  if(fertilizer){
    edit_perm_fertilise(
      file_in= filex_temp, 
      file_out = filex_temp, 
      new_max = fertilizer_param[1], 
      new_step = fertilizer_param[2])
  }


}

#wkdir = Working directory where your files will be saved
#cell = The spatial resolution you want e.g 1 degree
#b = Choose the country shapefile you want e.g "ZM" for Zimbabwe
#date = How may years of weather do you want to download e.g c("1985-01-01","2022-01-01")
#crop = The crop in APSIM you want to simulate e.g. "maize.apsimx"
#clck = How many years do you want the simulation to run e.g. c("1985-01-01T00:00:00", "2020-12-31T00:00:00")
#sd = The start date e.g.  "1-jan"
#ed = The end date e.g.  "31-dec"
#variety = The cultivar you want to simulate e.g "A_103"
#rep1 = An additional value to report e.g. "[Maize].Grain.Total.Wt*10 as Yield" ,
#rep2 =An additional value to report e.g. "[Maize].SowingDate"
#' Title
#'
#' @param scfl 
#' @param my_list_clm 
#' @param wkdir 
#' @param crop 
#' @param clck 
#' @param variety 
#' @param rep1 
#' @param rep2 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
apsimSpatialFactorial <- function(country,useCaseName,Crop, project_root, AOI = TRUE, 
                                  filex_temp,Planting_month_date = NULL, 
                                  Harvest_month_date = NULL, ID = "TLID", season = 1, 
                                  varietyid, zone, level2 = NA, 
                                  fertilizer = FALSE,  fert_factorial = FALSE,
                                  fertilizer_param,
                                  template_df = NULL,  fert_grid_RS = FALSE, 
                                  index_soilwat = 1,
                                  pathIn_zone = T,  rs_schedule_df = NULL, 
                                  Forecast = F, create_RS_schedule = F, fc_month = NA,
                                  fc_year = NA, clck, rep, fix_crop_or_soil_parm,
                                  Soil_source, datasourcing_path)
  {
  
  print(paste("Variety:", varietyid, "Zone:", zone))

  # Populate RS planting dates schedule depending on Forecast or not
  if(create_RS_schedule) {
    if (Forecast) {
      rs_schedule_df <- create_rs_schedule(
        template_df = template_df, fc_year = fc_year)
      template_df <- template_df %>% select(-c(q25, q50, q75))
    } else if (!Forecast) {
      rs_schedule_df <- create_rs_schedule(template_df = template_df)
      template_df <- template_df %>% select(-c(q25, q50, q75))
    }
  }  
  
  if (AOI) {
    if(is.null(rs_schedule_df$planting_dates)) {
      stop("Currently, the workflow only works if RS planting dates are provided.")
    }
    coords <- get_zone_coords_pdates(country, useCaseName, Crop, zone, Soil_source, 
                                     rs_schedule_df,datasourcing_path)
    
    if (!Forecast) {
      fc_year = 2000  # placeholder
    }
    
  } else {
    # TODO: THIS REMAINS UNCHANGED
    GPS_fieldData <- readRDS(paste("/home/jovyan/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    countryCoord$startingDate <- as.Date(countryCoord$plantingDate, "%Y-%m-%d") %m-% months(1)
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate","startingDate")
    coords <- countryCoord
  } 
  
  # Get path to EXT data and create if missing
  path.to.extdata <- create_extdata_path_APSIM(
    project_root = project_root, country = country, useCaseName = useCaseName,
    Crop = Crop, varietyid = varietyid, AOI = AOI)
  
  # Get path to Landing data and create if missing
  path.to.temdata <- create_temdata_path_APSIM(
    project_root = project_root, country = country, useCaseName = useCaseName, 
    Crop = Crop)

  # Sequence of location indices
  indices <- seq_len(nrow(coords))
  n_indices <- length(indices)
  
  plan_multisession(per_worker_gb = 5)
  
  messages_list <- future_lapply(
    indices, 
    function(i) {
      start_msg <- paste(
        "Start experiment:", i, "of", length(indices), "variety", varietyid
      )
      
      process_grid_element_experiment(i,
                                      path.to.extdata=path.to.extdata,
                                      path.to.temdata=path.to.temdata,
                                      coords = coords,
                                      zone=zone,
                                      level2=level2,
                                      filex_temp=filex_temp,
                                      clck=clck,
                                      varietyid=varietyid,
                                      rep=rep,
                                      fix_crop_or_soil_parm=fix_crop_or_soil_parm,
                                      plant_dates = NULL,
                                      fertilizer = fertilizer,
                                      fertilizer_param = fertilizer_param)
      
      end_msg <- paste(
        "Finished experiment:", i, "of", length(indices), "variety", varietyid
      )
      
      c(start_msg, end_msg)
    },
    
    future.packages = packages_required,
    future.seed = T
  )  

}


