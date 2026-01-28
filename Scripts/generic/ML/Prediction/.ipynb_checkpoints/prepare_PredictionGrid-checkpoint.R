#' -----------------------------------------------------------------------------
#' source all the generic data sourcing function and get soil, topography and 
#' climate data for the field trial points and ML prediction grid
#' 1. get point based climate + Topography + soil for the crop specified
#' 2. bind all the data and prepare the output for the data driven approach
#' 3. synchronize all the raster layers for the prediction grid in three 
#' different scenarios.
#' @param crops - the crop in which the data is generated
#' example - generate_AnalysisReadyData("Wheat")
#' -----------------------------------------------------------------------------
#' 
#' source("~/shared-data/Scripts/generic/dataSourcing/prepare_predictionGrid.R)
#' generate_AnalysisReadyData(crop="Maize")

generate_AnalysisReadyData <- function(crop){
  
  packages_required <- c("terra", "sf", "tidyverse", "readxl")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # memory.limit()
  
  pathIn <- paste("~/shared-data/Data", crop, "Intermediate/ML/wheat_feb", sep = "/")
  pathOut <- paste("~/shared-data/Data", crop, "geoSpatial/geo_4ML_AOI/NPrate", sep = "/")
  
  soils <- rast("~/shared-data/Data/General/National_geoData/Ethiosis soil/Ethiosis_projected.tif")
  names(soils)
  
  soil_sel <- soils[[c(1,2,14,15,18,19)]]
  
  n <- soil_sel[[1]]
  
  #topography
  
  aspect <- rast(paste("~/shared-data/Data/General/Global_GeoData/Landing/ETH_dem.tif"))|> terrain("aspect")|>
    terra::project(y = n) |>
    crop(n) |> mask(n)
  
  # weather normal scenario
  clim_norm_f <- rast()
  clim_norm <- list.files(path = pathIn, pattern = "_normal.tif", full.names = T)
  for(i in 1:length(clim_norm)){
    print(i)
    r <- rast(clim_norm[i]) |> terra::project(n) |>
      terra::crop(n) |> terra::mask(n)
    terra::add(clim_norm_f) <- r
  }
  
  sc_normal <- c(soil_sp, clim_norm_f[[c(1, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 23, 27, 31, 32)]], tpi, tri, slope, aspect)
  nlyr(sc_normal)
  # plot(sc_normal[[1]])
  
  names(sc_normal) <- c("N_soil", 'P_soil', "pH_","CEC","OM", "EC","soil_type","Rainfall_month1","Rainfall_month2",
                        "Rainfall_month3","nrRainyDays", "totalRF","relativeHumid_month1", "relativeHumid_month2",
                        "relativeHumid_month3", "solarRad_month1", "solarRad_month2", "solarRad_month3",
                        "Tmax_month1","Tmax_month2","Tmax_month3", "Tmin_month1", "Tmin_month2", "Tmin_month3",
                        "TPI","TRI","slope","Alt")
  
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  writeRaster(sc_normal, paste(pathOut, "scenario_normal.tif", sep = "/"), 
              filetype = "Gtiff", overwrite = T)
  rm(clim_norm_f)
  rm(sc_normal)
  
  # weather above scenario
  clim_above_f <- rast()
  clim_above <- list.files(path = pathIn, pattern = "_above.tif", full.names = T)
  for(i in 1:length(clim_above)){
    print(i)
    r <- rast(clim_above[i]) |> terra::project(n) |>
      terra::crop(n) |> terra::mask(n)
    terra::add(clim_above_f) <- r
  }
  
  sc_above <- c(soil_sp, clim_above_f[[c(1, 2, 3, 6, 7, 8, 9, 10, 13, 14, 15, 18, 19, 20, 23, 24, 25)]], tpi, tri, slope, dem)
  nlyr(sc_above)
  # plot(sc_above[[1]])
  
  names(sc_above) <- c("N_soil", 'P_soil', "pH_","CEC","OM","EC","soil_type","Rainfall_month1","Rainfall_month2",
                       "Rainfall_month3","nrRainyDays", "totalRF","relativeHumid_month1", "relativeHumid_month2",
                       "relativeHumid_month3", "solarRad_month1", "solarRad_month2", "solarRad_month3",
                       "Tmax_month1","Tmax_month2","Tmax_month3", "Tmin_month1", "Tmin_month2", "Tmin_month3",
                       "TPI","TRI","slope","Alt")
  writeRaster(sc_above, paste(pathOut, "scenario_above.tif", sep = "/"), 
              filetype = "Gtiff", overwrite = T)
  rm(clim_above_f)
  rm(sc_above)
  
  # weather below scenario
  clim_below_f <- rast()
  clim_below <- list.files(path = pathIn, pattern = "_below.tif", full.names = T)
  for(i in 1:length(clim_below)){
    print(i)
    r <- rast(clim_below[i]) |> terra::project(n) |>
      terra::crop(n) |> terra::mask(n)
    terra::add(clim_below_f) <- r
  }
  sc_below_sol <- c(soil_sp, tpi, tri, slope, dem)
  sc_below_weather <- c(clim_below_f[[c(1, 2, 3, 6, 7, 8, 9, 10, 13, 14, 15, 18,19,20, 23,24, 25)]])
  
  # plot(sc_below[[1]])
  
  names(sc_below_sol) <- c("N_soil", 'P_soil', "pH_","CEC","OM","EC","soil_type","TPI","TRI","slope","Alt")
  
  names(sc_below_weather) <- c("Rainfall_month1","Rainfall_month2",
                               "Rainfall_month3","nrRainyDays", "totalRF","relativeHumid_month1", "relativeHumid_month2",
                               "relativeHumid_month3", "solarRad_month1", "solarRad_month2", "solarRad_month3",
                               "Tmax_month1","Tmax_month2","Tmax_month3", "Tmin_month1", "Tmin_month2", "Tmin_month3")
  
  writeRaster(sc_below_sol, paste(pathOut, "scenario_below_soilDEM.tif", sep = "/"), 
              filetype = "Gtiff", overwrite = T) # correct naming
  
  writeRaster(sc_below_weather, paste(pathOut, "scenario_below_weather.tif", sep = "/"), 
              filetype = "Gtiff", overwrite = T)
  
}

