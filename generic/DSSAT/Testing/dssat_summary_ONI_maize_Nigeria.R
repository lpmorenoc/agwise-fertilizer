# Aggregated DSSAT output and ONI classification of aggregated DSSAT output

# Introduction: 
# This script allows : 
# (1) to aggregate/merge DSSAT output
# (2) To classify aggregated outputs from DSSAT simulation in terms of ONI index: Nina, Nino and Neutral year
# It provides also graphics of aggregated DSSAT output according to the ONI
# For more info regarding ONI : https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
# Authors : P.Moreno, E.Bendito Garcia, L.Leroux
# Credentials : EiA, 2024

#### Getting started #######

# 1. Sourcing required packages -------------------------------------------
packages_required <- c("DSSAT","tidyverse", "ggridges","patchwork", "Rmisc", "terra", "cowplot", "foreach","doParallel","future","future.apply")

installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
   install.packages(packages_required[!installed_packages])
  }

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

# get number of cores and set for parallel computing
num_cores <- parallel::detectCores() - 1
registerDoParallel(num_cores)

# 2. Defining required functions -------------------------------------------

oni_map <- function(data, x,y, fill, HWAH, shp, limits,varieties_grid=FALSE){
  
  # Mean plot
  if(varieties_grid ==FALSE){
    if (HWAH == TRUE){
      ggplot(data = data) +
        geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
        facet_grid(rows = vars(ENSO), switch=c('y'))+
        scale_fill_stepsn(n.breaks = 9, colours = viridis::magma(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
        theme(legend.position = "right")+ 
        geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
        coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
        xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Mean")
    } else {
      ggplot(data = data) +
        geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
        facet_grid(rows = vars(ENSO), switch=c('y'))+
        scale_fill_stepsn(n.breaks = 9, colours = viridis::magma(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
        #scale_fill_gradientn(colours = viridis::magma(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
        theme(legend.position = "right")+ 
        geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
        coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
        xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Standard Deviation")
    }
  }else{
    # Mean plot
    if (HWAH == TRUE){
      ggplot(data = data) +
        geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
        #facet_grid(cols = vars(Wheather), switch=c('x'), labeller=as_labeller(c(`A`="Niño",`B`="Neutral", `C`="Niña")))+
        facet_grid(Variety~ENSO)+
        scale_fill_gradientn(colours = viridis::viridis(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
        theme(legend.position = "right", aspect.ratio = 1, 
              axis.text.x =element_text(angle=90, hjust=1, face ="bold",size=14),
              axis.text.y = element_text(size = 14, face ="bold"),
              axis.title = element_text(size = 16, face = "bold"),
              strip.text = element_text(size = 16, face = "bold"),
              strip.background = element_blank(),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 16, face = "bold"),
              plot.title = element_text(hjust = 0.5, size=16,face ="bold"))+ 

        geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
        coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
        xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Mean") 
    } else {
      ggplot(data = data) +
        geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
        #facet_grid(cols = vars(Wheather), switch=c('x'), labeller=as_labeller(c(`A`="Niño",`B`="Neutral", `C`="Niña")))+
        facet_grid(Variety~ENSO)+
        scale_fill_gradientn(colours = viridis::magma(9),name=" Yield"~(kg~ha^{-1}), limits = limits)+ theme_bw()+
        theme(legend.position = "right", aspect.ratio = 1, 
              axis.text.x =element_text(angle=90, hjust=1, face ="bold", size =14),
              axis.text.y = element_text(size = 14, face ="bold"),
              axis.title = element_text(size = 16, face = "bold"),
              strip.text = element_text(size = 16, face = "bold"),
              strip.background = element_blank(),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 16, face = "bold"),
              plot.title = element_text(hjust = 0.5, size=16,face ="bold"))+ 
        geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
        coord_sf(expand = FALSE, xlim=c(min(data$Long), max(data$Long)), ylim=c(min(data$Lat), max(data$Lat)))+
        xlab("Longitude")+ ylab("Latitude") + ggtitle(label="Standard Deviation") 
    }
  }
  
}


### Create a function to determine the category
get_oni_category <- function(anom_values) {
  anom_values <- round(anom_values,1)
  n <- length(anom_values)
  categories <- rep("Neutral", n)
  
  i <- 1
  while (i <= n) {
    if (anom_values[i] <= -0.5) {
      count <- 0
      j <- i
      while (j <= n && anom_values[j] <= -0.5) {
        count <- count + 1
        j <- j + 1
      }
      if (count >= 5) {
        categories[i:(i+count-1)] <- "Niña"
        i <- i + count
      } else {
        i <- i + 1
      }
    } else if (anom_values[i] >= 0.5) {
      count <- 0
      j <- i
      while (j <= n && anom_values[j] >= 0.5) {
        count <- count + 1
        j <- j + 1
      }
      if (count >= 5) {
        categories[i:(i+count-1)] <- "Niño"
        i <- i + count
      } else {
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }
  return(categories)
}

#### Compute the mode from caterogical ONI over the cropping season 

calculate_mode <- function(x) {
  unique_x <- unique(x)
  tab <- tabulate(match(x, unique_x))
  mode_value <- unique_x[which.max(tab)]
  return(mode_value)
}
# 3. Aggregated DSSAT output  -------------------------------------------
#(To check if function from L. Leroux is more efficient)
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param varietyids ids of the varieties based on the cultivar file of DSSAT (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone_folder When TRUE the output folders are organized by administrative level 1.
#' @param level2_foler When TRUE the output folders are organized by administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created
#'        
#' @return merged results from DSSAT in RDS format
#'
#' @examples merge_DSSAT_output(country="Rwanda", useCaseName="RAB",Crop="Maize",varietyids=c("890011","890012"), zone_folder=T, level2_folder=F)

source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/merge_DSSAT_output.R")

# 4. Get ONI Index  -------------------------------------------

  #' @description Function that will classify each DSSAT simulation based on ONI index
  #' @param country country name
  #' @param useCaseName use case name
  #' @param Crop targeted crop
  #' @param AOI TRUE if the data is required for target area, and FALSE if it is for trial sites, default =TRUE
  #' @param season integer, cropping season concerned, default = 1 
  #' @param Plot, provide somes plots of the output, default = TRUE
  #' @param short_variety variety ID with short growing period duration
  #' @param medium_variety variety ID with medium growing period duration 
  #' @param long_variety variety ID with long growing period duration
  #'
  #' @return A table with the aggregated DSSAT simulation classified based on ONI index - Output table is written out in "~/agwise-potentialyield/dataops/potentialyield/Data/useCaseName/Crop/result/DSSAT/Extent/useCase_country_useCaseName_crop_Extent_season_X_ONI.RDS")
  #'
  #' @examples get_ONI(country= "Kenya", useCaseName = "KALRO", Crop="Maize", AOI=T, season=1, Plot=TRUE, short_variety="900111", medium_variety="900112",long_variety="900113")
  #' 

get_ONI_Nigeria <- function(country, useCaseName, Crop, AOI=TRUE, season,short_variety, medium_variety, long_variety,varietyid){

  ## 4.1. Creating a directory to store output table and graphics ####
  if (AOI == TRUE){
    pathOut <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName,"/",Crop,"/result/DSSAT/AOI/", sep="")
  } else {
    pathOut <- paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName,"/",Crop,"/result/DSSAT/fieldData/",  sep="")
  }
  
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## 4.2. Read the input data ####
  ### 4.2.1. Get the ONI data ####
  # Check if the ONI table is already available
  
  url <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
  destfile <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/"
  file_name <- "oni.txt"
  
  # Download it if doesn't exist
  if (!file.exists(paste(destfile, file_name, sep = ""))){
    download.file(url, paste(destfile, file_name, sep = ""), mode="wb")
  } else {
  # Update the ONI version with the last version
    unlink("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/oni.txt")
    download.file(url, paste(destfile, file_name, sep = ""), mode="wb")
  }
  
  # Open the ONI data
  oni <- read_table(paste0(destfile,file_name))
  
  # Reshape the ONI data
  month_mapping <- c(
    'DJF'= 'January',
    'JFM'= 'February',
    'FMA'= 'March',
    'MAM'= 'April',
    'AMJ'= 'May',
    'MJJ'= 'June',
    'JJA'= 'July',
    'JAS'= 'August',
    'ASO'= 'September',
    'SON'= 'October',
    'OND'= 'November',
    'NDJ'= 'December'
  )
  oni$month <- month_mapping[oni$SEAS]
  oni$date <- as.Date(paste0(oni$YR, "-", oni$month, "-01"), format="%Y-%B-%d")
  oni$year <-year(oni$date)
  oni$num_month <-month(oni$date)
  
  
  oni <- oni %>% arrange(year, num_month)
  

  
  # Apply the function to the oni data frame
  oni <- oni %>%
    mutate(oni_category = get_oni_category(ANOM))
  
  # Print the data frame to check the result
  path_to_save = "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/"
  write.table(oni, paste0(path_to_save,"oni_categorical.txt"), row.names = FALSE)
  
  ### 4.2.2. Get the aggregated DSSAT output and reshape the data ####
  # Open the aggregated DSSAT output
  if(AOI ==TRUE){
   dssat_path <- paste0(pathOut,"useCase_", country, "_" ,useCaseName,"_",Crop,"_AOI_season_",season,"_",varietyid,".RDS", sep="")
  }else{
  dssat_path <- paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",season,".RDS", sep="")
  }
  
   # Check if the object all_results with newly aggregated data is still in memory to be put into the path if not read the saved object        
  #ifelse(file.exists(dssat_path)==FALSE, dssat <- all_results, dssat <- readRDS(dssat_path))
  dssat <- readRDS(dssat_path)
  if(Crop %in% c('Cassava','Potato')) {
    dssat <- dssat %>%
      mutate(final_date = HDAT)
  }else{
    dssat <- dssat %>%
      mutate(final_date = as.Date(ifelse(!is.na(MDAT), as.character(MDAT), as.character(HDAT))))
    }
  ## 4.3. Get the mode ONI over the cropping season for aggregated DSSAT output ####
  # Ranges of date of analysis
    date_ranges <- data.frame (
      initial_date = dssat$PDAT, # Planting Date
      final_date = dssat$final_date    # Harvesting Date
    )

																	 
  date_ranges <- unique(date_ranges)


  num_cores <- availableCores() -3
  plan(multisession, workers = num_cores)
  
  # # Use future_lapply for parallel processing
  # medoni <- future_lapply(1:nrow(date_ranges), function(i) {
  #   filtered_data <- oni %>%
  #     filter(date >= as.Date(date_ranges$initial_date[i]) & date <= as.Date(date_ranges$final_date[i]))
  #   vars <- filtered_data$oni_category
  #   if(i %% 10000 == 0) {
  #     print(i)
  #   }
  #   calculate_mode(vars)
  # })
  # medoni <- do.call(rbind, medoni)
  # 
  # # Convert medoni to a dataframe with a specific column name
  # medoni_df <- tibble(mode_variable = medoni)
  # 
  # date_ranges_oni <- bind_cols(date_ranges,medoni_df)
  
  chunk_size <- 50  # Set your desired chunk size
  num_chunks <- ceiling(nrow(date_ranges) / chunk_size)
  
  # Function to process each chunk
  process_chunk <- function(chunk_index) {
    start_index <- (chunk_index - 1) * chunk_size + 1
    end_index <- min(chunk_index * chunk_size, nrow(date_ranges))
    
    chunk <- date_ranges[start_index:end_index, ]
    
    medoni_chunk <- future_lapply(1:nrow(chunk), function(i) {
      filtered_data <- oni %>%
        filter(date >= as.Date(chunk$initial_date[i]) & date <= as.Date(chunk$final_date[i]))
      vars <- filtered_data$oni_category
      if(i %% 1000 == 0) {
        print(paste("Processing row", i, "of chunk", chunk_index))
      }
      calculate_mode(vars)
    })
    
    medoni_chunk <- do.call(rbind, medoni_chunk)
    chunk_df <- tibble(mode_variable = medoni_chunk)
    
    return(bind_cols(chunk, chunk_df))
  }
  
  # Step 3: Process each chunk sequentially and combine results
  all_results <- lapply(1:num_chunks, function(chunk_index) {
    process_chunk(chunk_index)
  })
  
  # Step 4: Combine all results into a single data frame
  date_ranges_oni <- do.call(bind_rows, all_results)
  
  
  
  # Merge with DSSAT output
  dssat_oni <- merge(dssat, date_ranges_oni, by.x = c('PDAT', 'final_date'), by.y = c('initial_date', 'final_date'))
  rm(dssat)

  
  #colnames(dssat_oni) <- c(colnames(dssat_oni)[-27],'med_variable')
  
  ## 4.4. Classify tbe ONI into three classes (now using the categorical classification directly) ####
  # med ONI > 0.5, Nino, med ONI < -0.5 Nina, -0.5 > ONI > 0.5 Neutral 
# 
#   dssat_oni$ENSO <- ifelse(dssat_oni$med_variable > 0.5,"Niño",
#                              ifelse(dssat_oni$med_variable< -0.5,"Niña", "Neutral"))
  
  dssat_oni$ENSO  <- dssat_oni$mode_variable

  
  dssat_oni <- dssat_oni %>%
    mutate(year = year(PDAT),
           num_month = month(PDAT))
  
  # Replace ENSO na values with the oni_category at planting
  dssat_oni <- dssat_oni %>%
    left_join(oni %>% select(year, num_month, oni_category), by = c("year", "num_month")) %>%
    mutate(ENSO = ifelse(is.na(ENSO), oni_category, ENSO)) %>%
    select(-year, -num_month, -oni_category)  
  
  
  
  #Change the name of the varieties by their growing duration
  # Apply conditional transformation using case_when
  dssat_oni <- dssat_oni %>%
    mutate(Variety = case_when(
      Variety ==short_variety ~ "Short",
      Variety ==medium_variety ~ "Medium",
      Variety ==long_variety ~ "Long"
    ))
				
  dssat_oni$TNAM <- reorder(dssat_oni$TNAM, dssat_oni$TRNO)											   
											
  # Save the aggregated DSSAT output with ONI information
  if(AOI ==TRUE){
    saveRDS(dssat_oni,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",season,"_",varietyid,"_ONI.RDS" ))
  }else{
    saveRDS(dssat_oni,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",season,"_ONI.RDS" ))
  }
  

}

# country="Kenya"
# useCaseName="KALRO"
# Crop = "Maize"
# Extent = "AOI"
# season = 1
# Plot = TRUE
# 
# 
# get_ONI(country, useCaseName, Crop, Extent, season, Plot)
# merge_DSSAT_output(country, useCaseName, Crop, Extent, season)
  # Meand and SEM plot

 

