# Produce individual EXTE scatter plots and maps for best Sow Date and highest yields per location for MONOCROPS

# Sourcing required packages -------------------------------------------
packages_required <- c("tidyverse", "ggridges", "patchwork", "Rmisc", 
                       "terra", "cowplot", "foreach", "doParallel", "future",
                       "future.apply", "sf", "arrow", "geodata", "tools")

# Check which of the required packages are missing from the local R environment
installed_packages <- packages_required %in% rownames(installed.packages())

# Install only the missing packages
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])
}

# Load all required packages silently (suppressing warnings and startup text)
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

#' Generate APSIM Plots for Crop Simulation Results
#'
#' This function processes APSIM simulation results and produces plots to visualize
#' crop yields by variety, sowing date, and location. It can also save EXTE plots 
#' for individual simulation files if requested.
#'
#' @param country Character. Country name used to retrieve boundary data.
#' @param variety Character. Crop variety to filter results by.
#' @param useCaseName Character. Label describing the use case or scenario.
#' @param Crop Character. Crop name (e.g., "Maize").
#' @param expfile_name Character. APSIMX experimental file name (used to locate results).
#' @param produce_EXTE_plots Logical. If TRUE, scatter plots for individual EXTE simulation files are produced and saved.
#' @param yield_column Character or NULL. Optional column name to treat as yield (renamed to 'Yield').
#' @param project_root Character or NA. Root directory of the project.
#'
#' @return None. Side effects: saves PNG plots for best sowing dates and highest yields per location.
apsim.plots <- function(country, variety, useCaseName, Crop, expfile_name, 
                        produce_EXTE_plots = FALSE, yield_column = NULL,
                        project_root = NA) {
  
  # Strip the file extension (e.g., ".apsimx") to use the clean name for file paths
  clean_expfile_name <- tools::file_path_sans_ext(expfile_name)
  
  # Construct the file path and read the compiled APSIM simulation results from the parquet file
  results <- read_parquet(paste(project_root, "/useCases/Data/CropModel_Approach/", Crop,
                                "/useCase_", country, "_", useCaseName,
                                "/result/APSIM/AOI/", clean_expfile_name, ".parquet", sep = ""))
  
  # Standardize the target yield column
  # If a specific column is provided and exists, drop the existing 'Yield' column and rename the new one to 'Yield'
  if (!is.null(yield_column) && (yield_column %in% names(results))) {
    results$Yield <- NULL
    print(paste(yield_column, "selected as Yield."))
    results <- results %>% dplyr::rename(Yield = !!yield_column)
  }
  
  # Filter the dataset down to the specific crop variety
  # Group by the source simulation file and ensure there are enough data points (at least 5) to warrant plotting
  results <- results %>%
    filter(Variety == variety) %>%
    group_by(file_name) %>%
    filter(n() >= 5) %>%
    ungroup()
  
  # Extract a unique list of simulation files remaining after the filter
  unique_files <- unique(results$file_name)
  
  # Standardize Sowing Date formatting
  results <- results %>%
    mutate(
      # Append a dummy year (2020) to parse the day-month string into a valid Date object for chronological sorting
      SowDate_date = as.Date(paste0(SowDate, "-2020"), format = "%d-%b-%Y"),
      # Create a factor sorted chronologically rather than alphabetically to ensure plot axes are correctly ordered
      SowDate_factor = factor(SowDate, levels = unique(SowDate[order(SowDate_date)]))
    )
  
  # Conditionally produce and save scatter plots for each individual EXTE simulation file
  if (produce_EXTE_plots) {  
    for (fn in unique_files) {
      
      # Build the scatter plot mapping Yield against Sowing Date
      p <- results %>%
        filter(file_name == fn) %>%
        ggplot(aes(x = SowDate_factor, y = Yield)) +
        geom_point(na.rm = TRUE) +
        ggtitle({
          # Parse the file path string to dynamically extract the location and experiment name for the title
          path_parts <- str_split(fn, "/")[[1]]
          n <- length(path_parts)
          location <- path_parts[n - 2]
          experiment <- path_parts[n - 1]
          paste0("Yield for ", location, " (", experiment, ")")
        }) +
        # Rotate x-axis labels 90 degrees to prevent text overlap
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
      # Re-extract path parts to construct the specific output filename
      path_parts <- str_split(fn, "/")[[1]]
      n <- length(path_parts)
      location <- path_parts[n - 2]
      EXTE <- path_parts[n - 1]
      outfile <- paste0("yield_", clean_expfile_name, "_", location, "_", EXTE, ".png")
      
      # Transform the input file directory path into the correct output result path
      outdir <- dirname(fn)
      parts <- str_split(outdir, "/")[[1]]
      # Strip the last 3 subdirectories to move up the folder tree
      new_parts <- parts[1:(length(parts) - 3)]
      outdir <- paste0(paste(new_parts, collapse = "/"), "/")
      # Swap 'transform' folder for 'result' folder in the path string
      outdir <- sub("/transform/", "/result/", outdir)
      outdir <- paste0(outdir, "EXTE_", clean_expfile_name)
      
      # Create the destination directory structure if it does not exist
      if (!dir.exists(outdir)) {
        dir.create(outdir, recursive = TRUE)
      }
      
      # Save the individual scatter plot to disk
      ggsave(filename = file.path(outdir, outfile), plot = p, width = 8, height = 5)
    }
  }
  
  # Standardize capitalization for coordinate columns to ensure downstream consistency
  results <- results %>% 
    dplyr::rename(Longitude = longitude, Latitude = latitude)
  
  # Create a composite key linking Longitude and Latitude to group specific spatial points uniquely
  final <- mutate(results, lonlat = paste0(Longitude, "_", Latitude))
  
  # Note: 'p_Win' extracts the top 10 yields per location, but is currently unused in the plot generation below.
  p_Win <- final %>% 
    group_by(Longitude, Latitude) %>%
    arrange(desc(Yield)) %>% 
    slice(1:10) %>%
    as.data.frame()
  
  # Extract only the single highest yielding row (best sowing date) for each spatial coordinate
  pd <- final %>%
    group_by(Longitude, Latitude) %>%
    slice(which.max(Yield)) %>%
    as.data.frame()
  
  # Backup the country string name before overwriting the 'country' variable with spatial data
  country_ori <- country
  
  # Download (if necessary) and load administrative boundary spatial data (level 1) using the geodata package
  country <- geodata::gadm(country = country, level = 1, path = tempdir())
  # Convert the spatial SpatVector object into a Simple Features (sf) object for ggplot2 compatibility
  country <- st_as_sf(country)
  
  # Calculate bounding box limits with a 0.1 degree buffer to frame the map correctly
  xlimmin <- min(pd$Longitude) - 0.1
  xlimmax <- max(pd$Longitude) + 0.1
  ylimmin <- min(pd$Latitude) - 0.1
  ylimmax <- max(pd$Latitude) + 0.1
  
  # Generate Map 1: Best Sowing Date per spatial point
  p1 <- ggplot() + 
    geom_sf(data = country, fill = "white") + # Base layer: Country administrative boundaries
    geom_point(data = pd, aes(x = Longitude, y = Latitude, color = SowDate_factor), size = 2) + # Overlay: Yield data
    coord_sf(ylim = c(ylimmin, ylimmax), xlim = c(xlimmin, xlimmax), expand = FALSE) + # Restrict viewport to calculated bounding box
    labs(title = "Best Performing Sowing Date by Location", color = "Sowing Date")
  
  # Save Map 1 to disk
  ggsave(paste0(project_root, "/useCases/Data/CropModel_Approach/", Crop, "/useCase_", country_ori, "_", useCaseName, 
                "/result/APSIM/AOI/", clean_expfile_name, "_best_sowdate_by_loc.png"),
         p1, width = 8, height = 6, dpi = 300)
  print(p1) # Output to console/IDE
  
  # Generate Map 2: Highest Yield Magnitude per spatial point
  p3 <- ggplot() + 
    geom_sf(data = country, fill = "white") + # Base layer: Country administrative boundaries
    geom_point(data = pd, aes(x = Longitude, y = Latitude, color = Yield), size = 2) + # Overlay: Yield data mapped to a continuous color scale
    coord_sf(ylim = c(ylimmin, ylimmax), xlim = c(xlimmin, xlimmax), expand = FALSE) + # Restrict viewport
    labs(title = "Highest Yield by Location")
  
  # Save Map 2 to disk
  ggsave(paste0(project_root, "/useCases/Data/CropModel_Approach/", Crop, "/useCase_", country_ori, "_", useCaseName, 
                "/result/APSIM/AOI/", clean_expfile_name, "_highest_yield_by_loc.png"),
         p3, width = 8, height = 6, dpi = 300)
  print(p3) # Output to console/IDE
}