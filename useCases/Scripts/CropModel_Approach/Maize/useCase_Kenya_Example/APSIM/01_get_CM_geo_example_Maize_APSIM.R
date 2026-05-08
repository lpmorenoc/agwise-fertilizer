# Create soil and weather data in APSIM format for AOI data


################################################################################
## USER SETTINGS — Edit the parameters below to configure the analysis        ##
################################################################################

pathIn_zone <- T
level2 <- NA
Depth <- c(5, 15, 30, 60, 100, 200)
AOI <- TRUE
season <- 1
country <- "Kenya"
useCaseName <- "Example"
Crop <- "Maize"
varietyid <- "Early"
project_root <- "/home/jovyan/patricia_repos/agwise-fertilizer"
Forecast <- F
fc_month <- NULL
fc_year <- NULL
season_length_months <- NULL
datasourcing_path <- "~/agwise-datasourcing/dataops/datasourcing"

################################################################################
## DO NOT EDIT BELOW THIS LINE — Core processing code                         ##
################################################################################

countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)

#  Selecting Kisumu for testing (other provinces/zones do not have the data from datasourcing)
prov <- prov[17] 

source(paste(project_root,"generic/APSIM/01_readGeo_CM_zone_APSIM.R", sep='/'))

path.to.extdata <- create_extdata_path_APSIM(
  project_root, country, useCaseName, Crop, varietyid, AOI)

log_file <- paste(path.to.extdata, "progress_log_readGeo_CM_APSIM.txt", sep='/')

if (file.exists(log_file)) {
  file.remove(log_file)
}



start_time <- Sys.time()
for (i in 1:length(prov)){
  geoData_AOI <- readGeo_CM_zone_APSIM(country = country, 
                                       useCaseName = useCaseName, Crop = Crop, 
                                       project_root = project_root,
                                       AOI = AOI, season = season, 
                                       zone = prov[i], level2 = level2, 
                                       varietyid = varietyid,
                                       pathIn_zone = pathIn_zone, Depth = Depth,
                                       Forecast = Forecast, fc_month = fc_month, 
                                       fc_year = fc_year, 
                                       season_length_months = season_length_months,
                                       datasourcing_path = datasourcing_path
                                       )
  


  
  message <- paste("Province ",prov[i], " finished:", i,Sys.time())
  cat(message, "\n", file = log_file, append = TRUE)
}

end_time <- Sys.time()
duration <- end_time - start_time
cat(duration, "\n", file = log_file, append = TRUE)


#### Checking if the files make sense ####

load("/home/jovyan/patricia_repos/agwise-fertilizer/useCases/Data/CropModel_Approach/Maize/useCase_Kenya_Example/transform/APSIM/AOI/Early/Kisumu/EXTE0001/my_sol_1.RData")
