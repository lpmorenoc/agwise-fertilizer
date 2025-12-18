# A script to create experimental files in DSSAT format for each location to be run
# created by Siyabusa, Patricia and Andrew, and modified by ChatGPT


#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("tidyverse", "lubridate","DSSAT")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# -------------------------------------------------------------------------
# Create multiple sequential experiment files (.SQX) for DSSAT
# -------------------------------------------------------------------------
dssat.expfile <- function(country, useCaseName, Crop, AOI = FALSE,
                          seq_temp, ID="TLID", season = NULL,
                          years = 20, ingeno_maize, ingeno_soy,
                          Province, n_levels = c(0,30,60,90),
                          residues = c("Yes","No"),
                          systems = c("ContinuousMaize","MaizeSoybean")) {
  
  # --- Step 1: load metadata
  if (AOI == TRUE) {
    countryCoord <- readRDS(paste0("~/agwise-datacuration/dataops/datacuration/Data/useCase_",
                                   country, "_", useCaseName, "/", Crop, "/result/AOI_GPS.RDS"))
    countryCoord <- unique(countryCoord[, c("lon", "lat")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude")
    ground <- countryCoord
  } else {
    GPS_fieldData <- readRDS(paste0("~/agwise-datacuration/dataops/datacuration/Data/useCase_",
                                    country, "_", useCaseName, "/", Crop, "/result/compiled_fieldData.RDS"))
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord)[1:2] <- c("longitude", "latitude")
    ground <- countryCoord
  }
  
  # --- Step 2: set paths
  pathIn <- paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",
                   country, "_", useCaseName, "/", Crop, "/result/geo_4cropModel/", Province)
  
  if (AOI == TRUE) {
    Rainfall <- readRDS(paste0(pathIn,"/Rainfall_Season_",season,"_PointData_AOI.RDS"))
    Soil <- readRDS(paste0(pathIn,"/SoilDEM_PointData_AOI_profile.RDS"))
  } else {
    Rainfall <- readRDS(paste0(pathIn,"/Rainfall_PointData_trial.RDS"))
    Soil <- readRDS(paste0(pathIn,"/SoilDEM_PointData_trial_profile.RDS"))
  }
  
  names(Soil)[names(Soil)=="lat"] <- "latitude"
  names(Soil)[names(Soil)=="lon"] <- "longitude"
  Soil <- na.omit(Soil)
  
  if (AOI == TRUE) {
    metaDataWeather <- as.data.frame(Rainfall[,1:7])
  } else {
    metaDataWeather <- as.data.frame(Rainfall[,1:11])
  }
  
  names(metaDataWeather)[names(metaDataWeather)=="lat"] <- "latitude"
  names(metaDataWeather)[names(metaDataWeather)=="lon"] <- "longitude"
  
  metaData_Soil <- Soil[,c("longitude","latitude","NAME_1","NAME_2")]
  metaData <- merge(metaDataWeather[-c(2:3)], metaData_Soil)
  
  coords <- merge(metaData, ground, by=c("longitude","latitude"))
  coords <- coords[coords$NAME_1 == Province, ]
  grid <- as.matrix(coords)
  
  # --- Step 3: define paths
  if (AOI == TRUE) {
    path.to.extdata <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_",
                              country, "_", useCaseName, "/", Crop, "/transform/DSSAT/AOI/", Province)
  } else {
    path.to.extdata <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_",
                              country, "_", useCaseName, "/", Crop, "/transform/DSSAT/fieldData/")
  }
  
  if (!dir.exists(path.to.extdata)) dir.create(path.to.extdata, recursive = TRUE)
  
  path.to.temdata <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_",
                            country, "_", useCaseName, "/", Crop, "/Landing/DSSAT/")
  
  # --- Step 4: crop codes
  crops <- c("Maize", "Soybean")
  cropcode <- c("MZ", "SB")
  
  code_maize <- "MZ"
  code_soy   <- "SB"
  
  # --- Step 5: loop through grid and build SQX files
  results <- map(seq_along(grid[,1]), create_seqfile,
                 path.to.temdata = path.to.temdata,
                 seq_temp = seq_temp,
                 path.to.extdata = path.to.extdata,
                 coords = coords,
                 code_maize = code_maize,
                 code_soy = code_soy,
                 maize_vars = ingeno_maize,
                 soy_var = ingeno_soy,
                 n_levels = n_levels,
                 residues = residues,
                 systems = systems,
                 years = years,
                 Province = Province)
  
  return(results)
}
