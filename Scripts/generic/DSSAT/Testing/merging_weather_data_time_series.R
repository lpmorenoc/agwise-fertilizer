merge_weather_output <- function(country, useCaseName, Crop, AOI = FALSE, season = NULL, varietyid,zone_folder = TRUE, level2_folder = FALSE) {
  
  # Set up parallel processing
  plan(multisession, workers = 10)
    if (AOI == TRUE) {
      if (is.null(season)) {
        stop("With AOI=TRUE, season cannot be null. Please provide a season number.")
      }
      path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/DSSAT/AOI/", varietyid, sep = "")
    } else {
      path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/DSSAT/fieldData/", varietyid, sep = "")
    }
    
    if (!dir.exists(file.path(path.to.extdata))) {
      message("Experiments not found for varietyid: ", varietyid, ". Process stopped for this varietyid.")
      next
    }
    setwd(path.to.extdata)
    
    
    a <- list.files(path = path.to.extdata, pattern = "^WHTE.*\\.WTH$", include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
    
    results <- future_map_dfr(a, function(.x) {
      tryCatch({
        file <- read_filea(.x)
        file$file_name <- .x
        
        if (level2_folder == TRUE & zone_folder == TRUE) {
          test <- mgsub(.x, c(path.to.extdata, "/EXTE.*"), c("", ""))
          test <- strsplit(test, "/")[[1]]
          test <- test[test != ""]
          file$zone <- test[1]
          file$Loc <- file$zone
          file$level2 <- test[2]
        } else if (level2_folder == FALSE & zone_folder == TRUE) {
          file$zone <- mgsub(.x, c(path.to.extdata, "/", "/EXTE.*"), c("", "", ""))
          file$level2 <- NA
        } else if (level2_folder == TRUE & zone_folder == FALSE) {
          stop("You need to define first a zone (administrative level 1) to be able to run the model for level 2 (administrative level 2). Process stopped")
        } else {
          file$zone <- NA
          file$level2 <- NA
          file$Loc <- NA
        }
        file$Variety <- varietyid
        new_file <-file %>%
          mutate(month = month(DATE),
                 year = year(DATE)) %>%
          group_by(LAT,LONG,zone,file_name,year,month) %>%
          dplyr::summarise(monthly_rain = sum(RAIN, na.rm = TRUE), .groups = "drop")%>%
          ungroup()
        new_file
      }, error = function(e) {
        cat("Error processing file:", .x, "\n", e$message, "\n")
        NULL
      })
    })
    
  
  if (AOI == TRUE) {
    dir_path <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/result/DSSAT/AOI/")
  } else {
    dir_path <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/result/DSSAT/fieldData/")
  }
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  if (AOI == TRUE) {
    saveRDS(results, file = paste0(dir_path, "useCase_", country, "_", useCaseName, "_", Crop, "_AOI_season_rainfall", season, ".RDS"))
  }else{
    saveRDS(results, file = paste0(dir_path, "useCase_", country, "_", useCaseName, "_", Crop, "_fieldData_season_rainfall", season, ".RDS"))
  }
  return(results)
}



country="Rwanda"
useCaseName="RAB"
Crop = "Maize"
AOI =TRUE
Season = 1
Plot = TRUE
zone_folder = TRUE
level2_folder = FALSE
varietyid <- "890011"

merge_weather_output(country, useCaseName, Crop, AOI, season, varietyid,zone_folder, level2_folder)
