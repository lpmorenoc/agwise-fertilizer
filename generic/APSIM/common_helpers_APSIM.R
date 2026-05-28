# ==============================================================================
# APSIM Common Helper Functions
# ==============================================================================

#' Load or install an R package
#'
#' Checks if a package is installed in the local environment. If it is missing, 
#' it installs it automatically. Finally, it loads the package quietly.
#'
#' @param pkg Character. The name of the package to load or install.
#' @return None. Side effect: loads the package into the current environment.
load_or_install <- function(pkg) {
  # Check if the package is available in the current namespace
  if (!requireNamespace(pkg, quietly = T)) {
    install.packages(pkg)
  }
  # Load the package suppressing startup messages to keep the console clean
  suppressPackageStartupMessages(library(pkg, character.only = T))
}

### Load required packages
packages_required <- c(
  "chirps", "tidyverse","sf","furrr","future", "future.apply","parallel",
  "sp","apsimx","geodata", "terra", "countrycode","jsonlite")

# Apply the load_or_install function across all required packages silently
invisible(lapply(packages_required, load_or_install))


#' Create external data path for APSIM outputs
#'
#' Constructs the directory path where transformed APSIM data and simulation 
#' results will be stored. It routes the path to an "AOI" or "fieldData" 
#' subfolder based on the workflow type. Creates the directory if it is missing.
#'
#' @param project_root Character. Root directory of the project.
#' @param country Character. Name of the country.
#' @param useCaseName Character. Name of the specific use case.
#' @param Crop Character. Name of the crop being modeled.
#' @param varietyid Character. Identifier for the specific crop variety.
#' @param AOI Logical. TRUE if data is for Area of Interest, FALSE for trial sites (default: FALSE).
#'
#' @return Character. The constructed directory path.
create_extdata_path_APSIM <- function(project_root, country, useCaseName, Crop, 
                                      varietyid, AOI = F) {
  # Determine the appropriate subfolder based on the workflow
  subfolder <- ifelse(AOI, "AOI", "fieldData")
  
  # Construct the full path string
  path <- file.path(project_root, paste0(
    "useCases/Data/CropModel_Approach/", Crop, "/useCase_", country, "_", 
    useCaseName),"transform/APSIM", subfolder, varietyid)
  
  # Create the directory structure recursively if it does not already exist
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  return(path)
}


#' Create template data path for APSIM
#'
#' Constructs the path pointing to the "Landing" folder containing necessary 
#' APSIM template files (e.g., base weather and soil templates).
#'
#' @param project_root Character. Root directory of the project.
#' @param country Character. Name of the country.
#' @param useCaseName Character. Name of the specific use case.
#' @param Crop Character. Name of the crop being modeled.
#'
#' @return Character. The constructed template directory path.
#' @export
create_temdata_path_APSIM <- function(project_root, country, useCaseName, Crop) {
  # Construct the full path string to the Landing directory
  path.to.temdata <- paste0(project_root,
                            "/useCases/Data/CropModel_Approach/", Crop, "/useCase_", country, "_", 
                            useCaseName,"/Landing/APSIM")
  
  # Halt execution if the template directory is missing, as downstream processes rely on it
  if (!dir.exists(path.to.temdata)) {
    stop("Directory with APSIM Template Data (soil and weather files) does ", 
         "not exist, please add the template files. Process will stop.")
  }
  
  return(path.to.temdata)
}


#' Define output path for a specific experiment (EXTE)
#'
#' Builds the nested directory structure for a specific experimental run based 
#' on geographic zoning (administrative levels 1 and 2) and an index ID.
#'
#' @param path.to.extdata Character. Base external data directory.
#' @param i Integer. The index used to generate the EXTE ID (e.g., EXTE0001).
#' @param zone Character. Administrative level 1 zone name (default: NA).
#' @param level2 Character. Administrative level 2 zone name (default: NA).
#'
#' @return Character. The sanitized output directory path for the experiment.
define_pathOUT <- function(path.to.extdata, i, zone = NA, level2 = NA) {
  
  # Sanity check: Ensure level 1 (zone) is defined before defining level 2
  if (!is.na(level2) && is.na(zone)) {
    stop(
      "You need to define a zone (administrative level 1) ",
      "to get data for level 2 (administrative level 2)."
    )
  }
  
  # Format the integer index into a 4-digit padded string (e.g., "EXTE0015")
  exte_id <- paste0(
    "EXTE",
    formatC(as.integer(i), width = 4, flag = "0")
  )
  
  # Construct the path based on available administrative levels
  pathOUT <- if (!is.na(zone) && !is.na(level2)) {
    file.path(path.to.extdata, zone, level2, exte_id)
  } else if (!is.na(zone)) {
    file.path(path.to.extdata, zone, exte_id)
  } else {
    file.path(path.to.extdata, exte_id)
  }
  
  # Replace whitespaces with underscores, as APSIM struggles with spaces in file paths
  pathOUT <- gsub(" ", "_", pathOUT)  
  
  # Create the target directory if it does not already exist
  if (!dir.exists(pathOUT)) {
    dir.create(pathOUT, recursive = TRUE)
  }
  
  return(pathOUT)
}


#' Configure and plan multisession parallel processing
#'
#' Dynamically detects the available system RAM (accounting for container limits 
#' like cgroups) and calculates a safe number of workers for parallel execution 
#' to prevent out-of-memory crashes.
#'
#' @param per_worker_gb Numeric. The estimated RAM requirement (in GB) per parallel worker.
#'
#' @return None. Side effect: activates a future::plan multisession strategy.
plan_multisession <- function(per_worker_gb) {
  
  # Step 1: Detect RAM limits (container-aware setup)
  ram_limit_file <- "/sys/fs/cgroup/memory/memory.limit_in_bytes"
  
  if (file.exists(ram_limit_file)) {
    ram_limit_bytes <- as.numeric(readLines(ram_limit_file))
    # If the limit is real (not an artificially massive default value), calculate GBs
    if (!is.na(ram_limit_bytes) && ram_limit_bytes < 2 ^ 60) {
      available_ram_gb <- ram_limit_bytes / 1024 ^ 3
    } else {
      # Fallback to system /proc/meminfo if cgroup limit is unreliable
      available_ram_gb <- as.numeric(system("grep MemAvailable /proc/meminfo | awk '{print $2}'", intern=TRUE)) / 1024 / 1024
    }
  } else {
    # Fallback to standard /proc/meminfo for non-containerized Linux systems
    available_ram_gb <- as.numeric(system("grep MemAvailable /proc/meminfo | awk '{print $2}'", intern=TRUE)) / 1024 / 1024
  }
  
  # Step 2: Compute the safe number of workers
  # Divide available RAM by expected RAM per worker
  workers <- floor(available_ram_gb / per_worker_gb)
  
  # Cap the workers to ensure at least 3 CPU cores are left free for system stability
  workers <- min(workers, availableCores() - 3)
  
  # Ensure there is at least 1 worker to execute the tasks
  workers <- max(workers, 1)
  
  # Step 3: Activate the multisession parallel plan
  suppressWarnings(plan(multisession, workers = workers))
  
  # Notify the user of the configuration being used
  message("Parallel plan: ", workers, " workers, estimated per-worker RAM: ", per_worker_gb, " GB")
}