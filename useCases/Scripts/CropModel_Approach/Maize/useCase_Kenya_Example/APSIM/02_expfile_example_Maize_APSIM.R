# Create soil and weather data in APSIM format for AOI data

################################################################################
## USER SETTINGS — Edit the parameters below to configure the analysis        ##
################################################################################

## Define experiment ##
# Experimental file name
filex_temp<- "MaizeFactorialMonocropMultipleApplications.apsimx"; cropping_system <- "monocrop"

# expfile_name <- "MaizePeanutIntercrop.apsimx"; cropping_system <- "intercrop"
# expfile_name <- "MaizeSoybeanIntercrop.apsimx"; cropping_system <- "intercrop"

# expfile_name <- "MaizePeanutRotation.apsimx"; cropping_system <- "rotation"
# expfile_name <- "MaizeSoybeanRotation.apsimx"; cropping_system <- "rotation"

project_root <- "/home/jovyan/patricia_repos/agwise-fertilizer"
datasourcing_path <- "~/agwise-datasourcing/dataops/datasourcing"
country <- "Kenya"
useCaseName <- "Example"

# Crops
main_Crop <- "Maize"
#sec_Crop <- "Peanut"
#Crops <- c(main_Crop, sec_Crop)
Crop <- main_Crop

# Main and secondary crop cultivars
main_Crop_varietyid <- "Early"
# main_Crop_varietyid <- "Dekalb_XL82"  # Maize cultivar
# main_Crop_varietyid <- "Katumani"  # Maize cultivar
#sec_Crop_varietyid <- "Florunner"  # Peanut cultivar
# sec_Crop_varietyid <- "Soya791"  # Soybean cultivar
#varietyids <- c(main_Crop_varietyid, sec_Crop_varietyid)
varietyid <- main_Crop_varietyid

# APSIM will not run if some conditions are not met. It will be necessary to 
# adapt the soil or the crop parameters
fix_crop_or_soil_parm <- "soil"  # Adapt soil parameters to crop or viceversa

# Simulation range. This depends on data availability
clck = c("2000-02-01T00:00:00", "2020-12-31T00:00:00")

# Additional reporting variables
rep <- c("[Maize].Grain.Total.Wt*10 as Yield", 
         "[Maize].SowingDate")

## Use this to visualize APSIMX experimental file
# library(listviewer)
# apsimx::view_apsimx(expfile_name,
#                     src.dir = "~/agwise-cropping-innovation/Data/useCase_Rwanda_RAB/Maize/Landing/APSIM/")

countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)
prov <- "Kisumu"
level2 <- NA
AOI <- TRUE
pathIn_zone <- T
create_RS_schedule <- T
Forecast <- F
Soil_source <- "ISRIC"
fertilizer <- TRUE
rs_schedule_df <- NULL
fc_year <- NA
#fertilizer_param <- c(100,25)
fertilizer_param <- NULL


# Input file (.csv) for fertilizer, planting dates, varieties, ...
temp_file <- "Fertilizer_recommendation_template_V3.csv"


################################################################################
## DO NOT EDIT BELOW THIS LINE — Core processing code                         ##
################################################################################

# Source script and select function
script_to_source <- switch(cropping_system,
                           monocrop = "/home/jovyan/patricia_repos/agwise-fertilizer/generic/APSIM/02_MONOCROP_apsim.R",
                           intercrop = "/home/jovyan/patricia_repos/agwise-fertilizer/generic/APSIM/02_INTERCROP_apsim.R", #I need to edit these source files
                           rotation = "/home/jovyan/patricia_repos/agwise-fertilizer/generic/APSIM/02_ROTATION_apsim.R")  #I need to edit these source files

source(script_to_source)

path.to.temdata <- create_temdata_path_APSIM(project_root, country, useCaseName, Crop)
template_df <- read.csv(file.path(path.to.temdata, temp_file))

produce_apsim_file <- switch(cropping_system,
                             monocrop = apsimSpatialFactorial,
                             intercrop = apsimIntercrop,
                             rotation = apsimRotation)

arguments <- switch(cropping_system,
                    monocrop = list(
                      country = country, useCaseName = useCaseName, 
                      Crop = main_Crop, project_root = project_root,
                      AOI = AOI, filex_temp = filex_temp,
                      varietyid = main_Crop_varietyid,
                      zone = NULL, level2 = level2, 
                      fertilizer = fertilizer,  
                      fertilizer_param =fertilizer_param,
                      template_df = template_df, rs_schedule_df = rs_schedule_df, 
                      Forecast = Forecast, create_RS_schedule = create_RS_schedule, 
                      fc_year = fc_year, clck = clck, rep = rep, 
                      fix_crop_or_soil_parm = fix_crop_or_soil_parm,
                      Soil_source = Soil_source, datasourcing_path = datasourcing_path
                    ),
                    intercrop = list(
                      country = country, useCaseName = useCaseName, 
                      Crops = Crops, AOI = AOI, season = season, zone = NULL,
                      level2 = level2, 
                      filex_temp = filex_temp, clck = clck, 
                      varietyids = varietyids, rep = rep, 
                      fix_crop_or_soil_parm = fix_crop_or_soil_parm
                    ),
                    rotation = list(
                      country = country, useCaseName = useCaseName, 
                      Crops = Crops, AOI = AOI, season = season, zone = NULL, 
                      level2 = level2, pathIn_zone = pathIn_zone, 
                      filex_temp = filex_temp, clck = clck, 
                      varietyids = varietyids, rep = rep, 
                      fix_crop_or_soil_parm = fix_crop_or_soil_parm
                    ))

start_time <- Sys.time()
for (z in prov) {
  args <- arguments
  args$zone <- z
  do.call(produce_apsim_file, args)
}
end_time <- Sys.time()
duration <- end_time - start_time
cat(duration)
