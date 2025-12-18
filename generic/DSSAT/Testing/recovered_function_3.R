dssat.expfile <-
function(country, useCaseName, Crop, AOI = TRUE, filex_temp,
                          Planting_month_date = NULL, Harvest_month_date = NULL,
                          ID = "TLID", season = 1, plantingWindow = 1, varietyid,
                          zone, level2 = NA, fertilizer = FALSE, fert_factorial = FALSE, 
                          geneticfiles, index_soilwat = 1,
                          pathIn_zone = FALSE, rs_schedule_df = NULL){

  print(paste("Variety:", varietyid, "Zone:", zone))
  if(AOI == TRUE){
    if(is.null(Planting_month_date) | is.null(Harvest_month_date)){
      print("with AOI=TRUE, Planting_month_date, Harvest_month_date can not be null, please provide mm-dd for both")
      return(NULL)
    }
    countryCoord <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/AOI_GPS.RDS", sep=""))
    countryCoord <- unique(countryCoord[, c("lon", "lat")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]

    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    Harvest_month  <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    if(Planting_month < Harvest_month){ py <- 2000; hy <- 2000 } else { py <- 2000; hy <- 2001 }

    Planting_month_date <- as.Date(paste0(py, "-",Planting_month_date))
    countryCoord$plantingDate <- Planting_month_date
    Planting_month_date <- Planting_month_date %m-% months(1)

    if(Crop == "Cassava"){
      duration <- as.Date(paste0(hy, "-",Harvest_month_date)) - Planting_month_date
      if (duration < 240) hy <- hy + 1
    }
    Harvest_month_date <- as.Date(paste0(hy, "-",Harvest_month_date))
    countryCoord$harvestDate  <- Harvest_month_date
    if(plantingWindow > 1 & plantingWindow <= 5){
      Harvest_month_date <- Harvest_month_date %m+% months(1)
    }else if(plantingWindow > 5 & plantingWindow <=30){
      Harvest_month_date <- Harvest_month_date %m+% months(2)
    }
    countryCoord$startingDate <- Planting_month_date
    countryCoord$endDate      <- Harvest_month_date

    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude","plantingDate","harvestDate","startingDate","endDate")
    ground <- countryCoord
  }else{
    GPS_fieldData <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))
    countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    countryCoord$startingDate <- as.Date(countryCoord$plantingDate, "%Y-%m-%d") %m-% months(1)
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate","startingDate")
    ground <- countryCoord
  }

  # ---- datasourcing paths ----
  general_pathIn <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName,"/", Crop, "/result/geo_4cropModel", sep="")
  if (pathIn_zone == TRUE) {
    if(!is.na(level2) & !is.na(zone)){
      pathIn <- paste(general_pathIn,paste0(zone,'/',level2), sep = "/")
    }else if(is.na(level2) & !is.na(zone)){
      pathIn <- paste(general_pathIn,zone, sep = "/")
    }else if(!is.na(level2) & is.na(zone)){
      print("You need to define first a zone (administrative level 1) to be able to get data for level 2 in datasourcing. Process stopped")
      return(NULL)
    }else{
      pathIn <- general_pathIn
    }
  }else{
    pathIn <- general_pathIn
  }

  if(AOI == TRUE){
    Rainfall <- readRDS(paste(pathIn, "/Rainfall_Season_", season, "_PointData_AOI.RDS", sep=""))
    Soil     <- readRDS(paste(pathIn,"/SoilDEM_PointData_AOI_profile.RDS", sep=""))
  }else{
    Rainfall <- readRDS(paste(pathIn, "Rainfall_PointData_trial.RDS", sep=""))
    Soil     <- readRDS(paste(pathIn, "SoilDEM_PointData_trial_profile.RDS", sep=""))
  }

  names(Soil)[names(Soil)=="lat"] <- "latitude"
  names(Soil)[names(Soil)=="lon"] <- "longitude"
  Soil <- na.omit(Soil)

  if ("Zone" %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="Zone"] <- "NAME_1"}
  if ("lat"  %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="lat"]  <- "latitude"}
  if ("lon"  %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="lon"]  <- "longitude"}

  if(AOI == TRUE){
    metaDataWeather <- as.data.frame(Rainfall[,c("longitude", 'latitude', "startingDate", "endDate", "NAME_1", "NAME_2")])
  }else{
    metaDataWeather <- as.data.frame(Rainfall[,c("longitude", 'latitude', "startingDate", "endDate", "NAME_1", "NAME_2",
                                                 "yearPi","yearHi","pl_j","hv_j")])
  }
  metaData_Soil <- Soil[,c("longitude", "latitude","NAME_1","NAME_2")]
  metaData <- merge(metaDataWeather,metaData_Soil)

  # ---- years span (AOI) ----
  if(AOI==TRUE){
    R1 <- Rainfall[1, ]
    if ("country" %in% names(R1)) {R1<- subset(R1, select = -country)}
    if ("ID" %in% names(R1)) {R1<- subset(R1, select = -ID)}
    R1 <- pivot_longer(R1,
                       cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                       names_to = c("Variable", "Date"),
                       names_sep = "_",
                       values_to = "RAIN")
    number_years <- max(lubridate::year(as.Date(R1$Date, "%Y-%m-%d"))) -
      min(lubridate::year(as.Date(R1$Date, "%Y-%m-%d")))
  }else{
    number_years <- 1
  }

  metaData <- unique(metaData[, c("longitude", "latitude","NAME_1","NAME_2")])
  coords <- merge(metaData, ground)

  if(!is.na(zone)){coords <- coords[coords$NAME_1==zone,] }
  if(!is.na(level2)){coords <- coords[coords$NAME_2==level2,] }

  # ----------------------------------------------------------------------
  # Attach RS planting schedule with robust join: rounded lon/lat (3 dp)
  # ----------------------------------------------------------------------
  if (!is.null(rs_schedule_df)) {
    rs_clean <- rs_schedule_df %>%
      {
        nm <- names(.)
        if ("lon" %in% nm) rename(., longitude = lon) else .
      } %>%
      {
        nm <- names(.)
        if ("lat" %in% nm) rename(., latitude  = lat) else .
      } %>%
      mutate(
        longitude = as.numeric(longitude),
        latitude  = as.numeric(latitude),
        lon_r = if ("lon_r" %in% names(.)) lon_r else round(longitude, 3),
        lat_r = if ("lat_r" %in% names(.)) lat_r else round(latitude, 3)
      ) %>%
      select(lon_r, lat_r, planting_dates, startingDate, harvestDate)

    coords <- coords %>%
      mutate(
        longitude = as.numeric(longitude),
        latitude  = as.numeric(latitude),
        lon_r = round(longitude, 3),
        lat_r = round(latitude, 3)
      ) %>%
      dplyr::left_join(rs_clean, by = c("lon_r","lat_r"), suffix = c("", ".rs")) %>%
      mutate(
        startingDate = ifelse(!purrr::map_lgl(planting_dates, ~is.null(.x) || all(is.na(.x))),
                              as.Date(startingDate.rs), as.Date(startingDate)),
        harvestDate  = ifelse(!purrr::map_lgl(planting_dates, ~is.null(.x) || all(is.na(.x))),
                              as.Date(harvestDate.rs),  as.Date(harvestDate))
      )
  } else {
    coords$planting_dates <- replicate(nrow(coords), as.Date(NA), simplify = FALSE)
  }

  grid <- as.matrix(coords)
  if (nrow(coords) == 0) {
    print("No coordinates to process after filtering. Exiting.")
    return(NULL)
  }

  # ---- output dirs & crop codes ----
  if(AOI == TRUE){
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/",varietyid, sep="")
  }else{
    path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/",varietyid, sep="")
  }
  if (!dir.exists(path.to.extdata)) dir.create(path.to.extdata, recursive = TRUE)

  path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/Landing/DSSAT", sep="")

  crops <- c("Maize", "Potato", "Rice", "Soybean", "Wheat", "Beans", "Cassava")
  cropcode_supported <- c("MZ", "PT", "RI", "SB", "WH", "BN", "CS")
  cropid <- which(crops == Crop)
  crop_code <- cropcode_supported[cropid]

  # ---- parallel over points ----
  setwd(path.to.extdata)
  log_file <- file.path(path.to.extdata, "progress_log_exp.txt")
  if (file.exists(log_file)) file.remove(log_file)

  num_cores <- max(1, future::availableCores() - 3)
  plan(multisession, workers = num_cores)

  indices <- seq_len(nrow(coords))
  results <- future_lapply(indices, function(i) {
    cat(paste("Progress experiment:", i, "out of", length(indices), 
              "zone:", zone), "\n", file = log_file, append = TRUE)

    out <- create_filex(
      i = i,
      path.to.temdata = path.to.temdata,
      filex_temp = filex_temp,
      path.to.extdata = path.to.extdata,
      coords = coords,
      AOI = AOI,
      crop_code = crop_code,
      plantingWindow = plantingWindow,
      number_years = number_years,
      varietyid = varietyid,
      zone = zone,
      level2 = level2,
      fertilizer = fertilizer,
      fert_factorial = fert_factorial,
      geneticfiles = geneticfiles,
      index_soilwat = index_soilwat,
      plant_dates = coords$planting_dates[[i]]  # <<< RS-driven vector of Dates (4 per coordinate)
    )

    cat(paste("Finished:", i, "out of", length(indices),"zone:",zone), "\n", file = log_file, append = TRUE)
    out
  }, future.globals = TRUE)

  plan(sequential)
  gc()

  invisible(results)
}
