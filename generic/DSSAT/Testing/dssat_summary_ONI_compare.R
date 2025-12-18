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
packages_required <- c("DSSAT","tidyverse", "ggridges","patchwork", "Rmisc", "terra", "cowplot", "foreach","doParallel")

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

get_median_variable <- function(initial_date, final_date, variable, data) {
  filtered_data <- data %>%
    filter(date >= as.Date(initial_date) & date <= as.Date(final_date))
  
  med_variable <- median(filtered_data[[variable]], na.rm = TRUE)
  
  return(data.frame(initial_date = initial_date, final_date = final_date, med_variable = med_variable))
}



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

get_ONI <- function(country, useCaseName, Crop, AOI=TRUE, season, Plot=TRUE, short_variety, medium_variety, long_variety){

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
  

  destfile <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/"
  file_name <- "oni_categorical.txt"

  # Open the ONI data
  #oni <- read_table(paste0(destfile,file_name))
  lines <- readLines(paste0(destfile,file_name))
  
  # Remove quotation marks from the text
  cleaned_lines <- gsub("\"", "", lines)
  
  # Read the cleaned data using read.table with a custom separator
  oni <- read.table(text = cleaned_lines, header = TRUE, sep = "", fill = TRUE)
  oni$date <- as.Date(oni$date)

  
  ### 4.2.2. Get the aggregated DSSAT output and reshape the data ####
  # Open the aggregated DSSAT output
  if(AOI ==TRUE){
   dssat_path <- paste0(pathOut,"useCase_", country, "_" ,useCaseName,"_",Crop,"_AOI_season_",season,"_weather_growth_phases_ONI.RDS", sep="")
  }else{
  dssat_path <- paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",season,"_weather_growth_phases_ONI.RDS", sep="")
  }
  
   # Check if the object all_results with newly aggregated data is still in memory to be put into the path if not read the saved object        
  #ifelse(file.exists(dssat_path)==FALSE, dssat <- all_results, dssat <- readRDS(dssat_path))
  dssat <- readRDS(dssat_path)

  ## 4.3. Get the median ONI over the cropping season for aggregated DSSAT output ####
  # Ranges of date of analysis
  date_ranges <- data.frame (
    initial_date = dssat$PDAT, # Planting Date
    final_date = dssat$HDAT    # Harvesting Date
  )
																	 
  date_ranges <- unique(date_ranges)

  date_ranges_mat <- data.frame (
    initial_date = dssat$PDAT, # Planting Date
    final_date = dssat$MDAT    # Maturity Date
  )
  
  date_ranges_mat <- unique(date_ranges_mat)

  # Compute the median ONI over the cropping season #(takes alot of time)
 # med_oni <- pmap_dfr(date_ranges, get_median_variable, variable="ANOM", data=oni)

 # # Update the aggregated DSSAT output
 # dssat_oni <- bind_cols(dssat,med_oni)
  calculate_mode <- function(x) {
    unique_x <- unique(x)
    tab <- tabulate(match(x, unique_x))
    mode_value <- unique_x[which.max(tab)]
    return(mode_value)
  }
  
  medoni <- foreach::foreach(i=1:nrow(date_ranges)) %dopar% {
    filtered_data <- oni %>%
      filter(date >= as.Date(date_ranges$initial_date[i]) & date<= as.Date(date_ranges$final_date[i]))
    vars <- filtered_data$oni_category
    if(i%%10000==0){
      print(i)}
    calculate_mode(vars)}
  medoni <- do.call(rbind, medoni)
  
  # Convert medoni to a dataframe with a specific column name
  medoni_df <- tibble(mod_variable = medoni)

  
  date_ranges_oni <- bind_cols(date_ranges,medoni_df)
  
  medoni_mat <- foreach::foreach(i=1:nrow(date_ranges_mat)) %dopar% {
    filtered_data <- oni %>%
      filter(date >= as.Date(date_ranges_mat$initial_date[i]) & date<= as.Date(date_ranges_mat$final_date[i]))
    vars <- filtered_data$oni_category
    if(i%%10000==0){
      print(i)}
    calculate_mode(vars)}
  medoni_mat <- do.call(rbind, medoni_mat)

  # Convert medoni to a dataframe with a specific column name
  #medoni_df <- tibble(mod_variable = medoni)
  medoni_df_mat <- tibble(mod_variable_mat = medoni_mat)
  
  date_ranges_oni_mat <- bind_cols(date_ranges_mat,medoni_df_mat)
  
  # Merge with DSSAT output
  dssat_oni <- merge(dssat, date_ranges_oni, by.x = c('PDAT', 'HDAT'), by.y = c('initial_date', 'final_date'))
  dssat_oni <- merge(dssat_oni, date_ranges_oni_mat, by.x = c('PDAT', 'MDAT'), by.y = c('initial_date', 'final_date'))
  #colnames(dssat_oni) <- c(colnames(dssat_oni)[-27],'med_variable')
  

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
    saveRDS(dssat_oni,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_AOI_season_",season,"_weather_growth_phases_ONI_cat.RDS" ))
  }else{
    saveRDS(dssat_oni,paste0(pathOut,"useCase_", country, "_",useCaseName,"_",Crop,"_fieldData_season_",season,"_weather_growth_phases_ONI_cat.RDS" ))
  }
  
  ## 4.5. Basic plots ####
  if (Plot == TRUE){
    ### 4.5.1. Global plot ####
    #dssat_oni <- na.omit(dssat_oni)
    # Define a manual scale
    val <- c("Neutral" = "gold",
             "Niño" = "tomato1",
             "Niña" = "royalblue2")
    
    dssat_oni  <- dssat_oni %>%
      mutate(
        mod_variable_mat = ifelse(mod_variable_mat == "neutral", "Neutral", 
                                  ifelse(mod_variable_mat == "nino","Niño",
                                         ifelse(mod_variable_mat =="nina","Niña",mod_variable_mat))),
        mod_variable = ifelse(mod_variable == "neutral", "Neutral", 
                                  ifelse(mod_variable == "nino","Niño",
                                         ifelse(mod_variable =="nina","Niña",mod_variable)))
      )
    
			 
  # Meand and SDM plot new ENSO classification (mode considering historical classification)
  pd <- position_dodge(0.2)
  dssat_oni %>%
    summarySE(measurevar="HWAH", groupvars=c("TNAM", "mod_variable"))%>%
  ggplot(aes(x = TNAM, 
             y = HWAH, 
             group=mod_variable, 
             color=mod_variable)) +
    geom_point(position = pd, size = 3) +
    geom_line(position = pd,linewidth = 1) +
    geom_errorbar(aes(ymin = HWAH - sd, ymax = HWAH + sd), width = .1, position= pd)+
    theme_bw()+
    scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
    ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))+
    labs(color = "New ENSO")

  

  # Meand and SDM plot original ENSO classification
  pd <- position_dodge(0.2)
  dssat_oni %>%
    summarySE(measurevar="HWAH", groupvars=c("TNAM", "ENSO"))%>%
    ggplot(aes(x = TNAM, 
               y = HWAH, 
               group=ENSO, 
               color=ENSO)) +
    geom_point(position = pd, size = 3) +
    geom_line(position = pd,linewidth = 1) +
    geom_errorbar(aes(ymin = HWAH - sd, ymax = HWAH + sd), width = .1, position= pd)+
    theme_bw()+
    scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
    ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))  
  
  
  # Meand and SDM plot new ENSO classification (until maturity)
  pd <- position_dodge(0.2)
  dssat_oni %>%
    summarySE(measurevar="HWAH", groupvars=c("TNAM", "mod_variable_mat"))%>%
    ggplot(aes(x = TNAM, 
               y = HWAH, 
               group=mod_variable_mat, 
               color=mod_variable_mat)) +
    geom_point(position = pd, size = 3) +
    geom_line(position = pd,linewidth = 1) +
    geom_errorbar(aes(ymin = HWAH - sd, ymax = HWAH + sd), width = .1, position= pd)+
    theme_bw()+
    scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
    ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))+
    labs(color = "New ENSO")
  
  
  
  # Heat map (original ENSO)
  p1 <- dssat_oni %>%
    summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
  ggplot(aes(x=TNAM, y=ENSO, fill=HWAH))+
     geom_tile(color="white", linewidth=0.1)+
     theme_bw()+
     theme(axis.text.x =element_text(angle=45, hjust=1))+
     scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
     coord_equal()+
    ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
    ggtitle("Mean yield")
  
 p2 <-  dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
    ggplot(aes(x=TNAM, y=ENSO, fill=sd))+
    geom_tile(color="white", linewidth=0.1)+
    theme_bw()+
    theme(axis.text.x =element_text(angle=45, hjust=1))+
    scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
    coord_equal()+
    ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
    ggtitle("Standard Deviation")
 p1+p2+plot_layout(ncol = 1) 
 
 
 # Heat map (new ENSO)
 p1 <- dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "mod_variable"))%>%
   ggplot(aes(x=TNAM, y=mod_variable, fill=HWAH))+
   geom_tile(color="white", linewidth=0.1)+
   theme_bw()+
   theme(axis.text.x =element_text(angle=45, hjust=1))+
   scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
   coord_equal()+
   ylab(expression(bold("New ENSO")))+xlab(expression(bold("Planting time")))+
   ggtitle("Mean yield")
 
 p2 <-  dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "mod_variable"))%>%
   ggplot(aes(x=TNAM, y=mod_variable, fill=sd))+
   geom_tile(color="white", linewidth=0.1)+
   theme_bw()+
   theme(axis.text.x =element_text(angle=45, hjust=1))+
   scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
   coord_equal()+
   ylab(expression(bold("New ENSO")))+xlab(expression(bold("Planting time")))+
   ggtitle("Standard Deviation")
 p1+p2+plot_layout(ncol = 1) 

 
 ### 4.5.2. Plot by variety ####
 # Mean and SEM plot (old ENSO)
 pd <- position_dodge(0.2)
 dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x = TNAM, 
              y = HWAH, 
              group=ENSO, 
              color=ENSO)) +
   facet_grid(rows = vars(Variety))+
   geom_point(position = pd, size = 3) +
   geom_line(position = pd,linewidth = 1) +
   geom_errorbar(aes(ymin = HWAH - se, ymax = HWAH + se), width = .1, position= pd)+
   theme_bw()+
   scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
   ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))
 # Mean and SEM plot (new ENSO)
 dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "mod_variable"))%>%
   ggplot(aes(x = TNAM, 
              y = HWAH, 
              group=mod_variable, 
              color=mod_variable)) +
   facet_grid(rows = vars(Variety))+
   geom_point(position = pd, size = 3) +
   geom_line(position = pd,linewidth = 1) +
   geom_errorbar(aes(ymin = HWAH - se, ymax = HWAH + se), width = .1, position= pd)+
   theme_bw()+
   scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
   ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))+
   labs(color = "New ENSO")

 # Mean and SEM plot (new ENSO, maturity)
 dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "mod_variable_mat"))%>%
   ggplot(aes(x = TNAM, 
              y = HWAH, 
              group=mod_variable_mat, 
              color=mod_variable_mat)) +
   facet_grid(rows = vars(Variety))+
   geom_point(position = pd, size = 3) +
   geom_line(position = pd,linewidth = 1) +
   geom_errorbar(aes(ymin = HWAH - se, ymax = HWAH + se), width = .1, position= pd)+
   theme_bw()+
   scale_color_manual(values=val)+ theme(axis.text.x =element_text(angle=45, hjust=1))+
   ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab(expression(bold("Planting time")))+
   labs(color = "New ENSO")
 
 # Heat map (old ENSO)
 p1 <- dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x=TNAM, y=ENSO, fill=HWAH))+
   facet_grid(rows = vars(Variety))+
   geom_tile(color="white", linewidth=0.1)+
   theme_bw()+
   theme(axis.text.x =element_text(angle=45, hjust=1))+
   scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
   coord_equal()+
   ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
   ggtitle("Mean yield")
 
 p2 <-  dssat_oni %>%
   summarySE(measurevar="HWAH", groupvars=c("Variety","TNAM", "ENSO"))%>%
   ggplot(aes(x=TNAM, y=ENSO, fill=sd))+
   facet_grid(rows = vars(Variety))+
   geom_tile(color="white", linewidth=0.1)+
   theme_bw()+
   theme(axis.text.x =element_text(angle=45, hjust=1))+
   scale_fill_viridis_c(name="Yield"~(kg~ha^{-1}))+
   coord_equal()+
   ylab(expression(bold("ENSO")))+xlab(expression(bold("Planting time")))+
   ggtitle("Standard Deviation")
 p1+p2+plot_layout(ncol = 2) 

 
 # Boxplot average vegetative rain (new ENSO)
 dssat_oni %>%
   ggplot(aes(x = TNAM, 
              y = veg_rain_avg, 
              fill=mod_variable)) +
   geom_boxplot()+theme_bw()+
   labs(fill="New ENSO_mat", x="Planting time",y="Daily average vegetative rain")+
   scale_fill_manual(values=val)+
   theme(axis.text.x =element_text(angle=45, hjust=1),
         legend.title = element_text(face="bold"))
 
 # Boxplot average vegetative rain (old ENSO)
 dssat_oni %>%
   ggplot(aes(x = TNAM, 
              y = veg_rain_avg, 
              fill=ENSO)) +
   geom_boxplot()+theme_bw()+
   labs(fill="Old ENSO", x="Planting time",y="Daily average vegetative rain")+
   scale_fill_manual(values=val)+
   theme(axis.text.x =element_text(angle=45, hjust=1),
         legend.title = element_text(face="bold"))

 # Boxplot average reproductive rain (new ENSO)
 dssat_oni %>%
   ggplot(aes(x = TNAM, 
              y = rep_rain_avg, 
              fill=mod_variable)) +
   geom_boxplot()+theme_bw()+
   labs(fill="New ENSO", x="Planting time",y="Daily average reproductive rain")+
   scale_fill_manual(values=val)+
   theme(axis.text.x =element_text(angle=45, hjust=1),
         legend.title = element_text(face="bold"))
 
 # Boxplot average reproductive temperature (old ENSO) (no effect)
 dssat_oni %>%
   ggplot(aes(x = TNAM, 
              y = rep_tmean_avg, 
              fill=ENSO)) +
   geom_boxplot()+theme_bw()+
   labs(fill="Old ENSO", x="Planting time",y="Daily average reproductive rain")+
   scale_fill_manual(values=val)+
   theme(axis.text.x =element_text(angle=45, hjust=1),
         legend.title = element_text(face="bold"))
 

 # Boxplot average vegetative temperature (new ENSO) (no effect)
 dssat_oni %>%
   ggplot(aes(x = TNAM, 
              y = rep_tmean_avg, 
              fill=mod_variable)) +
   geom_boxplot()+theme_bw()+
   labs(fill="Old ENSO", x="Planting time",y="Daily average reproductive rain")+
   scale_fill_manual(values=val)+
   theme(axis.text.x =element_text(angle=45, hjust=1),
         legend.title = element_text(face="bold"))
 
 
 
 
 
 
 
 
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

 

