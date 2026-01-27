##Siya_code

library(stringr)
library(dplyr)

# Define the base folder path
base_folder <- "~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Ethiopia_PotentialYield/Wheat/transform/DSSAT/AOI/TI2020/Somali"

# Initialize counters
coordinate_counter <- 1
weather_counter <- 1
soil_counter <- 1

# Function to update soil file content
update_soil_file <- function(file_path, new_code) {
  soil_content <- readLines(file_path)
  soil_content <- str_replace(soil_content, "TRAN\\d{5}", new_code)
  writeLines(soil_content, file_path)
}

# Process the folders
district_folders <- list.dirs(base_folder, full.names = TRUE, recursive = FALSE)

for (district_folder in district_folders) {
  # Get coordinate folders in the district
  coordinate_folders <- list.dirs(district_folder, full.names = TRUE, recursive = FALSE)
  
  for (coordinate_folder in coordinate_folders) {
    # Generate new coordinate folder name
    new_coordinate_name <- sprintf("EXTE%04d", coordinate_counter)
    new_coordinate_path <- file.path(base_folder, new_coordinate_name)
    file.rename(coordinate_folder, new_coordinate_path)
    coordinate_counter <- coordinate_counter + 1
    
    # Process files in the coordinate folder
    weather_files <- list.files(new_coordinate_path, pattern = "\\.WTH$", full.names = TRUE)
    soil_files <- list.files(new_coordinate_path, pattern = "\\.SOL$", full.names = TRUE)
    
    # Rename weather files
    for (weather_file in weather_files) {
      new_weather_name <- sprintf("EXTE%04d.WTH", weather_counter)
      new_weather_path <- file.path(new_coordinate_path, new_weather_name)
      file.rename(weather_file, new_weather_path)
      weather_counter <- weather_counter + 1
    }
    
    # Update soil file content
    for (soil_file in soil_files) {
      new_code <- sprintf("TRAN%05d", soil_counter)
      update_soil_file(soil_file, new_code)
      soil_counter <- soil_counter + 1
    }
  }
}

cat("Processing complete. Files and folders renamed successfully.\n")








# path <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Ethiopia_PotentialYield/Wheat/transform/DSSAT/AOI/TI20OR/Oromi'
# out_path <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Ethiopia_PotentialYield/Wheat/transform/DSSAT/AOI/TI2021/'
# 
# # Get provinces
# prov_path <- list.dirs(path,recursive = FALSE)
# prov_p <- list.files(path,recursive = TRUE)
# 
# 
# for (p in 1:length(prov_path)){
# new_name <- 0
#   
# # Get EXTE files from the district by  province
# distr_path <- list.dirs(prov_path[p],recursive = TRUE)
# # out_path_p <- paste0(out_path,'/', prov_p[p])
# for (d in 1:length(distr_path)){
#   # lwv <- list.files(distr_path[d],recursive = TRUE)
#   lwv <- list.files(distr_path[d],recursive = TRUE,pattern = '.SOL')
#   lww <- list.files(distr_path[d],recursive = TRUE,pattern = '.WTH')
#   
#   for (k in 1:length(lwv)){
#     sol <- DSSAT::read_sol(paste0(distr_path[d],'/',lwv[k]))
#     wth <- DSSAT::read_wth(paste0(distr_path[d],'/',lww[k]))
#     # wth <- list.files(paste0(distr_path[d],'/',lwv[k]), pattern = '.WTH')
#     # wth <- DSSAT::read_wth(paste0(distr_path[d],'/',lwv[k],'/',wth))
#     new_name <- new_name + 1
#     new <- paste0('TRAN', formatC(width = 5, (as.integer(new_name)), flag = "0"))
#     w_new <- paste0('WHTE', formatC(width = 4, (as.integer(new_name)), flag = "0"))
#     
#     exte_new <- paste0('EXTE', formatC(width = 4, (as.integer(new_name)), flag = "0"))
#     
#     sol$PEDON <- new
#     if (!dir.exists(paste0(out_path))){dir.create(paste0(out_path))}
#     if (!dir.exists(paste0(out_path,'/',exte_new))){dir.create(paste0(out_path,'/',exte_new))}
#     DSSAT::write_sol(sol, file  = paste0(out_path,'/',exte_new,'/','SOIL.SOL'), title = 'General DSSAT Soil Input File', append = FALSE)
#     DSSAT::write_wth(wth, file  = paste0(out_path,'/',exte_new,'/',w_new,'.WTH'))
#     
#   }
#   
# }
# # print(distr_path[d])
# # print(k)
# }



# 
# 
# # Rename
# path <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zambia_Solidaridad/Soybean/transform/DSSAT/AOI/999911/Central'
# out_path <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zambia_Solidaridad/Soybean/transform/DSSAT/AOI/999911/correct_Central'
# 
# # Get provinces
# prov_path <- list.dirs(path,recursive = FALSE)
# prov_p <- list.files(path,recursive = FALSE)
# 
# for (p in 1:length(prov_path)){
#   new_name <- 0
#   out_path_p <- paste0(out_path,'/', prov_p[p])
#   for (d in 1:length(path)){
#     lwv <- list.files(path[d])
#     
#     for (k in 1:length(lwv)){
#       sol <- DSSAT::read_sol(paste0(path[d],'/',lwv[k],'/SOIL.SOL'))
#       wth <- list.files(paste0(path[d],'/',lwv[k]), pattern = '.WTH')
#       wth <- DSSAT::read_wth(paste0(path[d],'/',lwv[k],'/',wth))
#       new_name <- new_name + 1
#       new <- paste0('TRAN', formatC(width = 5, (as.integer(new_name)), flag = "0"))
#       w_new <- paste0('WHTE', formatC(width = 4, (as.integer(new_name)), flag = "0"))
#       
#       exte_new <- paste0('EXTE', formatC(width = 4, (as.integer(new_name)), flag = "0"))
#       
#       sol$PEDON <- new
#       if (!dir.exists(paste0(out_path_p))){dir.create(paste0(out_path_p))}
#       if (!dir.exists(paste0(out_path_p,'/',exte_new))){dir.create(paste0(out_path_p,'/',exte_new))}
#       DSSAT::write_sol(sol, file  = paste0(out_path_p,'/',exte_new,'/','SOIL.SOL'), title = 'General DSSAT Soil Input File', append = FALSE)
#       DSSAT::write_wth(wth, file  = paste0(out_path_p,'/',exte_new,'/',w_new,'.WTH'))
#       
#     }
#     
#   }
#   print(path[d])
#   print(k)
# }

sa






