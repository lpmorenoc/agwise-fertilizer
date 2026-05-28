# Merge output of the APSIM simulations for diverse locations for MONOCROPS

#################################################################################################################
## sourcing required packages
#################################################################################################################
packages_required <- c("arrow","mgsub","tidyverse","future.apply","future","furrr", "tools")

# Check the local library to see which packages are missing
installed_packages <- packages_required %in% rownames(installed.packages())

# Install any missing packages
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])
}

# Load all required packages silently
invisible(lapply(packages_required, library, character.only = TRUE))

#################################################################################################################
## Helper Functions
#################################################################################################################

# Extracts only the latitude and longitude from the header of an APSIM weather (.met) file
get_lat_lon <- function(filepath) {
  # Read only the first 20 lines to optimize speed, as coordinates are in the header
  lines <- readLines(filepath, n = 20)  
  
  # Search for the lines starting with "latitude" and "longitude"
  lat_line <- grep("^latitude", lines, value = TRUE)
  long_line <- grep("^longitude", lines, value = TRUE)
  
  # Split the string at the "=" sign, extract the numeric value, and trim whitespace
  lat <- as.numeric(trimws(strsplit(lat_line, "=")[[1]][2]))
  long <- as.numeric(trimws(strsplit(long_line, "=")[[1]][2]))
  
  return(list(lat = lat, lon = long))
}

# Reads an entire APSIM weather (.met) file and formats it into a data frame
read_APSIM_weather_file <- function(filepath) {
  lines <- readLines(filepath)
  
  # Extract latitude and longitude from the header
  lat_line <- grep("^latitude", lines, value = TRUE)
  long_line <- grep("^longitude", lines, value = TRUE)
  
  if (length(lat_line) == 0 || length(long_line) == 0) {
    stop("Could not find latitude/longitude in file")
  }
  
  lat <- as.numeric(trimws(strsplit(lat_line, "=")[[1]][2]))
  long <- as.numeric(trimws(strsplit(long_line, "=")[[1]][2]))
  
  # Locate the row defining the column names (starts with year and day)
  header_idx <- grep("^year\\s+day", lines)
  if (length(header_idx) == 0) {
    stop("Could not find weather data header in file")
  }
  
  # Clean and extract the column names
  colnames <- strsplit(trimws(lines[header_idx]), "\\s+")[[1]]
  
  # Isolate the actual data lines (skipping the header and the units row immediately below it)
  data_lines <- lines[(header_idx + 2):length(lines)]
  
  # Parse the text block into a data frame
  df <- read.table(
    text = paste(data_lines, collapse = "\n"),
    header = FALSE
  )
  
  # Apply the extracted column names
  names(df) <- colnames
  
  # Convert the APSIM year and Julian day into a standard R Date format
  df$date <- as.Date(paste(df$year, df$day), format = "%Y %j")
  df$latitude <- lat
  df$longitude <- long
  
  return(df)
}

#################################################################################################################
## Main Merging Function
#################################################################################################################

#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param expfile_name name of the experimental file 
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param varietyids ids of the varieties based on the cultivar file of APSIM (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone_folder When TRUE the output folders are organized by administrative level 1.
#' @param level2_folder When TRUE the output folders are organized by administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#' @param project_root Root directory path for the project
#'        
#' @return merged results from APSIM in parquet format (saved to disk)
#'
#' @examples merge_APSIM_output(country="Rwanda", useCaseName="RAB",Crop="Maize",varietyids=c("Early"))

merge_APSIM_output <- function(country, useCaseName, Crop, expfile_name, AOI = FALSE, season = NULL, varietyids, zone_folder = TRUE, level2_folder = FALSE, project_root = NA) {
  
  # Set up parallel processing to handle large volumes of simulation outputs
  num_cores <- availableCores() - 3 # Leave 3 cores free for background system tasks
  plan(multisession, workers = num_cores)
  all_results <- NULL
  
  # Iterate through each specified crop variety
  for (varietyid in varietyids) {
    
    # Determine the directory path based on whether the data is for a target Area of Interest (AOI) or trial sites
    if (AOI == TRUE) {
      if (is.null(season)) {
        stop("With AOI=TRUE, season cannot be null. Please provide a season number.")
      }
      path.to.extdata <- paste(project_root,"/useCases/Data/CropModel_Approach/", Crop,"/useCase_",country, "_", useCaseName, "/transform/APSIM/AOI/", varietyid, sep = "")
    } else {
      path.to.extdata <- paste(project_root,"/useCases/Data/CropModel_Approach/", Crop,"/useCase_", country, "_", useCaseName, "/transform/APSIM/fieldData/", varietyid, sep = "")
    }
    
    # Check if the folder for the current variety exists; if not, skip to the next variety
    if (!dir.exists(file.path(path.to.extdata))) {
      message("Experiments not found for varietyid: ", varietyid, ". Process stopped for this varietyid.")
      next
    }
    setwd(path.to.extdata)
    
    # Construct the regex pattern to find the specific HarvestReport parquet files
    clean_expfile_name <- tools::file_path_sans_ext(expfile_name)
    fixed_part <- paste0("^HarvestReport_", clean_expfile_name)
    pattern <- paste0(fixed_part, "_.*\\.parquet$")
    
    # a: List of all HarvestReport parquet files
    a <- list.files(path = path.to.extdata,
                    pattern = pattern,
                    include.dirs = TRUE,
                    full.names = TRUE,
                    recursive = TRUE)
    
    # b: List of all weather (.met) files in the same directory structure
    b <- list.files(path = path.to.extdata, pattern = "^wth.*\\.met$", include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
    
    # Process files in parallel and bind them into a single data frame
    results <- future_imap_dfr(a, function(.x, idx) {
      tryCatch({
        # Read the simulation output parquet file
        file <- read_parquet(.x)
        file$file_name <- .x
        
        # Find the weather file that belongs to the same directory as the current parquet file
        matching_weather <- b[dirname(b) == dirname(.x)][1]
        
        # Extract and attach coordinates if a matching weather file is found
        if (!is.na(matching_weather)) {
          coords <- get_lat_lon(matching_weather)
          file$latitude <- coords$lat
          file$longitude <- coords$lon
        } else {
          file$latitude <- NA
          file$longitude <- NA
        }
        
        # Extract geographic/administrative zoning information from the file path structure
        if (level2_folder == TRUE & zone_folder == TRUE) {
          test <- mgsub(.x, c(path.to.extdata, "/EXTE.*"), c("", ""))
          test <- strsplit(test, "/")[[1]]
          test <- test[test != ""]
          file$zone <- test[1]
          file$Loc <- file$zone
          file$level2 <- test[2]
        } else if (level2_folder == FALSE & zone_folder == TRUE) {
          file$zone <- mgsub(.x, c(path.to.extdata, "/", "/EXTE.*"), c("", "", ""))
          file$Loc <- file$zone
          file$level2 <- NA
        } else if (level2_folder == TRUE & zone_folder == FALSE) {
          stop("You need to define first a zone (administrative level 1) to be able to run the model for level 2 (administrative level 2). Process stopped")
        } else {
          file$zone <- NA
          file$level2 <- NA
          file$Loc <- NA
        }
        
        # Tag the data with the current variety ID
        file$Variety <- varietyid
        
        # Return the processed data frame chunk
        file
      }, error = function(e) {
        # Catch and print errors without crashing the entire parallel mapping process
        cat("Error processing file:", .x, "\n", e$message, "\n")
        NULL
      })
    })
    
    # Append the results of the current variety to the master data frame
    all_results <- bind_rows(all_results, results)
  }
  
  # Determine the final output directory based on AOI flag
  if (AOI == TRUE) {
    dir_path <- paste0(project_root,"/useCases/Data/CropModel_Approach/", Crop,"/useCase_",country, "_", useCaseName, "/result/APSIM/AOI/")
  } else {
    dir_path <- paste0(project_root,"/useCases/Data/CropModel_Approach/", Crop,"/useCase_",country, "_", useCaseName, "/result/APSIM/fieldData/")
  }
  
  # Create the output directory if it doesn't already exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Write the fully merged dataset to disk as a parquet file
  # Note: Both branches currently do the exact same thing; consider appending `season` here if AOI == TRUE
  if (AOI) {
    write_parquet(
      all_results,
      sink = paste0(
        dir_path,
        clean_expfile_name, ".parquet"
      )
    )
  } else {
    write_parquet(
      all_results,
      sink = paste0(
        dir_path,
        clean_expfile_name, ".parquet"
      )
    )
  }
}