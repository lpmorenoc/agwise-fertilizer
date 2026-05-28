# This script runs APSIM simulations for MONOCROPS
# It installs and loads required packages, defines helper functions,
# and executes APSIM experiments across zones and varieties.

packages_required <- c("arrow", "tidyverse","furrr","future", "future.apply","apsimx", "jsonlite", "tools")

# Check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])
}

# Load required packages
invisible(lapply(packages_required, library, character.only = TRUE))



#' Run APSIM simulation for a single grid element
#'
#' Executes APSIMX for one experimental file (EXTE folder) and saves the
#' harvest report as a Parquet file. Handles errors gracefully.
#'
#' @param i Integer. Index of the EXTE folder.
#' @param path.to.zone Character. Path to the zone directory containing EXTE folders.
#' @param expfile_name Character. APSIMX experimental file name.
#' @param AOI Logical. Whether the run is for Area of Interest (default TRUE).
#'
#' @return None. Side effect: writes Parquet harvest report file.
#' @export
#'
#' @examples
#' runapsim(1, path.to.zone = "path/to/zone", expfile_name = "maize.apsimx")
runapsim <- function(i, path.to.zone, expfile_name) {
  # Set working directory to the EXTE folder for this index
  setwd(paste(path.to.zone, paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
  
  # Run APSIM and capture harvest report
  tryCatch({
    harvest_data <- apsimx::apsimx(expfile_name, value = "HarvestReport")
    experiment_name <- tools::file_path_sans_ext(expfile_name)
    
    # Save harvest report as Parquet file
    write_parquet(harvest_data, paste0("HarvestReport_", experiment_name, "_", i, ".parquet"))
    
    gc() # free memory
  }, error = function(e) {
    # Log error if simulation fails
    message("Simulation failed for ", path.to.zone, "EXTE ", i, ": ", e$message)
  })
}

#' Execute APSIM simulations across zones and varieties
#'
#' Runs APSIM experiments for all EXTE folders in a given zone and variety.
#' Handles directory setup, parallel execution, and logging of progress.
#'
#' @param country Character. Country code (e.g., "ZM").
#' @param useCaseName Character. Name of the use case/project.
#' @param Crop Character. Crop name (e.g., "maize").
#' @param AOI Logical. Whether to use Area of Interest workflow (default TRUE).
#' @param expfile_name Character. APSIMX experimental file name.
#' @param varietyid Character. Cultivar identifier.
#' @param zone Character. Administrative zone name.
#' @param level2 Character or NA. Optional administrative level 2 identifier.
#' @param project_root Character. Root directory of the project.
#'
#' @return None. Side effects: runs APSIM simulations and writes harvest reports.
#' @export
#'
#' @examples
#' apsim.exec("ZM", "MaizeStudy", "maize", expfile_name = "maize.apsimx",
#'            varietyid = "A_103", zone = "ZoneA", project_root = "/project")
apsim.exec <- function(country, useCaseName, Crop, AOI = TRUE, expfile_name,
                       varietyid, zone, level2 = NA, project_root = NA) {  
  
  # Define path to variety folder depending on AOI or fieldData workflow
  if (AOI == TRUE) {
    path.to.varietyid <- paste(project_root, "/useCases/Data/CropModel_Approach/", Crop,
                               "/useCase_", country, "_", useCaseName, "/transform/APSIM/AOI/", varietyid, sep = "")
  } else {
    path.to.varietyid <- paste(project_root, "/useCases/Data/CropModel_Approach/", Crop,
                               "/useCase_", country, "_", useCaseName, "/transform/APSIM/fieldData/", varietyid, sep = "")
  }
  
  # Build zone path depending on level1/level2 inputs
  if (!is.na(level2) & !is.na(zone)) {
    path.to.zone <- paste(path.to.varietyid, zone, level2, sep = "/")
    path.to.zone <- gsub(" ", "_", path.to.zone)  # remove spaces
  } else if (is.na(level2) & !is.na(zone)) {
    path.to.zone <- paste(path.to.varietyid, zone, sep = "/")
    path.to.zone <- gsub(" ", "_", path.to.zone)
  } else if (!is.na(level2) & is.na(zone)) {
    print("You need to define a zone (level 1) before running for level 2. Process stopped")
    return(NULL)
  } else {
    path.to.zone <- path.to.varietyid
  }
  
  # Check if input files exist
  if (!dir.exists(file.path(path.to.zone))) {
    print("You need to create input files (weather, soil, experimental data) before running. Process stopped")
    return(NULL)
  }
  
  setwd(path.to.zone)
  
  # List EXTE folders (ignore notebooks)
  folders <- list.dirs(".", full.names = FALSE, recursive = FALSE)
  folders <- grep(folders, pattern = ".ipynb", value = TRUE, invert = TRUE, fixed = TRUE)
  matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
  
  # Create indices for EXTE folders
  indices <- seq_along(matching_folders)
  
  # Initialize log file
  log_file <- paste(path.to.zone, "progress_log_run.txt", sep = '/')
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  
  # Configure parallel execution
  num_cores <- availableCores() - 3
  plan(multisession, workers = num_cores)
  
  # Run APSIM simulations in parallel
  future_lapply(indices, function(i) {
    message <- paste("Progress:", i, "out of", length(indices))
    cat(message, "\n", file = log_file, append = TRUE)
    
    runapsim(i, path.to.zone = path.to.zone, expfile_name = expfile_name)
    
    message2 <- paste("Finished:", i, "out of", length(indices))
    cat(message2, "\n", file = log_file, append = TRUE)
  })
  
  # Reset to sequential plan
  plan(sequential)
  
  # Clean environment and free memory
  rm(list = ls())
  gc()
}





