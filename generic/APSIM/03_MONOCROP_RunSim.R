# This script runs APSIM simulations for MONOCROPS

packages_required <- c("arrow", "tidyverse","furrr","future", "future.apply","apsimx", "jsonlite", "tools")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


# Run the simulation for the entire study area  
#' Title length(my_list_clm)
#'
#' @param my_list_clm 
#' @param extd.dir 
#' @param stn 
#'
#' @return
#' @export
#'
#' @examples

runapsim <-function(i, path.to.zone, expfile_name, AOI=TRUE){
  setwd(paste(path.to.zone, paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))

  # Run APSIM
  tryCatch({
    harvest_data <- apsimx::apsimx(expfile_name, value = "HarvestReport")
    experiment_name <- tools::file_path_sans_ext(expfile_name)
    write_parquet(harvest_data, paste0("HarvestReport_", experiment_name, "_", i, ".parquet"))
    gc()
  }, error = function(e){
    message("Simulation failed for ", path.to.zone, "EXTE ", i, ": ", e$message)
  }
  )
}

apsim.exec <- function(country, useCaseName, Crop, AOI = TRUE,expfile_name,varietyid, zone, level2 = NA, project_root = NA){  
  
  #Set working directory to save the results
  # zone <- gsub(" ", "_", zone)
  if(AOI == TRUE){
    path.to.varietyid <- paste(project_root,"/useCases/Data/CropModel_Approach/", Crop,"/useCase_", country, "_",useCaseName, "/transform/APSIM/AOI/",varietyid, sep="")
  }else{
    path.to.varietyid <- paste(project_root,"/useCases/Data/CropModel_Approach/", Crop,"/useCase_", country, "_",useCaseName, "/transform/APSIM/fieldData/",varietyid, sep="")
  }
  
  # define working path or path to run the model
  if(!is.na(level2) & !is.na(zone)){
    path.to.zone <- paste(path.to.varietyid, zone, level2, sep = "/")
    path.to.zone <- gsub(" ", "_", path.to.zone)  # APSIM does not allow for whitespaces in directories
  }else if(is.na(level2) & !is.na(zone)){
    path.to.zone <- paste(path.to.varietyid, zone, sep = "/")
    path.to.zone <- gsub(" ", "_", path.to.zone)  # APSIM does not allow for whitespaces in directories
  }else if(!is.na(level2) & is.na(zone)){
    print("You need to define first a zone (administrative level 1) to be able to run the model for level 2 (administrative level 2). Process stopped")
    return(NULL)
  }else{
    path.to.zone <- path.to.varietyid
  }
  if (!dir.exists(file.path(path.to.zone))){
    print("You need to create the input files (weather, soil and experimental data) before running the model. Process stopped")
    return(NULL)
  }
  
  setwd(path.to.zone)
  
  # List EXTE files
  folders <- list.dirs(".", full.names = FALSE, recursive = FALSE)
  folders <- grep(folders, pattern = ".ipynb", value = TRUE, invert = TRUE, fixed = TRUE)
  matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
  
  
  # Create a list of indices
  indices <- seq_along(matching_folders)
  
  # Create a log file to see the progress of the simulations
  log_file <- paste(path.to.zone, "progress_log_run.txt", sep='/')
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  
  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  
  # results <- future_lapply(indices, function(i) {
  future_lapply(indices, function(i) {
    message <- paste("Progress:", i, "out of", length(indices))
    cat(message, "\n", file = log_file, append = TRUE)
    
    # result <- runapsim(i, path.to.zone=path.to.zone,expfile_name=expfile_name,AOI=AOI)
    runapsim(i, path.to.zone=path.to.zone,expfile_name=expfile_name,AOI=AOI)
    
    message2 <- paste("Finished:", i, "out of", length(indices))
    cat(message2, "\n", file = log_file, append = TRUE)
    
    # return(result)
  })
  plan(sequential)
  
  rm(list = ls())
  gc()
  
}




