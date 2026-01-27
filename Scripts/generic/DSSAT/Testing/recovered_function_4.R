dssat.exec <-
function(country, useCaseName, Crop, AOI = TRUE,TRT,varietyid, zone, level2=NA){  
     
  #Set working directory to save the results
   if(AOI == TRUE){
     path.to.extdata_ini <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/",varietyid, sep="")
   }else{
     path.to.extdata_ini <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/",varietyid, sep="")
   }
   
   
   #define working path or path to run the model
   if(!is.na(level2) & !is.na(zone)){
     path.to.extdata <- paste(path.to.extdata_ini,zone,level2, sep = "/")
   }else if(is.na(level2) & !is.na(zone)){
     path.to.extdata <- paste(path.to.extdata_ini,zone, sep = "/")
   }else if(!is.na(level2) & is.na(zone)){
     print("You need to define first a zone (administrative level 1) to be able to run the model for level 2 (administrative level 2). Process stopped")
     return(NULL)
   }else{
     path.to.extdata <- path.to.extdata_ini
   }
   if (!dir.exists(file.path(path.to.extdata))){
     print("You need to create the input files (weather, soil and experimental data) before running the model. Process stopped")
     return(NULL)
   }
   
  setwd(path.to.extdata)
  
  folders <- list.dirs(".", full.names = FALSE, recursive = TRUE)
  folders <- grep(folders, pattern = ".ipynb", value = TRUE, invert = TRUE, fixed = TRUE)
  matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
  
  
  crops <- c("Maize", "Potato", "Rice", "Soybean", "Wheat","Cassava","Beans","Sorghum")
  cropcode_supported <- c("MZ","PT", "RI", "SB", "WH","CS","BN","SG")
  
  cropid <- which(crops == Crop)
  crop_code <- cropcode_supported[cropid]
  # Create a list of indices
  indices <- seq_along(matching_folders)
  
  # Create a log file to see the progress of the simulations
  log_file <- paste(path.to.extdata,"progress_log.txt",sep='/')
  if (file.exists(log_file)) {
    file.remove(log_file)
  }

  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  results <- future_lapply(indices, function(i) {
    message <- paste("Progress:", i, "out of", length(indices))
    cat(message, "\n", file = log_file, append = TRUE)
    
    result <- rundssat(i, path.to.extdata=path.to.extdata, TRT=TRT,  AOI=AOI,crop_code=crop_code)

    message2 <- paste("Finished:", i, "out of", length(indices))
    cat(message2, "\n", file = log_file, append = TRUE)
    
    return(result)
  })
  plan(sequential)
  rm(list = ls())
  gc()
  
}
