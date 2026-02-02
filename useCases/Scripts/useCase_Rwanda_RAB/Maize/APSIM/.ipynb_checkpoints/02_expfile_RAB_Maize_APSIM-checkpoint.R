# Create soil and weather data in APSIM format for AOI data

################################################################################
## USER SETTINGS — Edit the parameters below to configure the analysis        ##
################################################################################

## Define experiment ##
# Experimental file name
expfile_name <- "MaizeFactorialAugSep.apsimx"; cropping_system <- "monocrop"

# expfile_name <- "MaizePeanutIntercrop.apsimx"; cropping_system <- "intercrop"
# expfile_name <- "MaizeSoybeanIntercrop.apsimx"; cropping_system <- "intercrop"

# expfile_name <- "MaizePeanutRotation.apsimx"; cropping_system <- "rotation"
# expfile_name <- "MaizeSoybeanRotation.apsimx"; cropping_system <- "rotation"

country <- "Rwanda"
useCaseName <- "RAB"

# Crops
main_Crop <- "Maize"
sec_Crop <- "Peanut"
# sec_Crop <- "Soybean"
Crops <- c(main_Crop, sec_Crop)

# Main and secondary crop cultivars
main_Crop_varietyid <- "Early"
# main_Crop_varietyid <- "Dekalb_XL82"  # Maize cultivar
# main_Crop_varietyid <- "Katumani"  # Maize cultivar
# sec_Crop_varietyid <- "Florunner"  # Peanut cultivar
# sec_Crop_varietyid <- "Soya791"  # Soybean cultivar
varietyids <- c(main_Crop_varietyid, sec_Crop_varietyid)

# APSIM will not run if some conditions are not met. It will be necessary to 
# adapt the soil or the crop parameters
fix_crop_or_soil_parm <- "soil"  # Adapt soil parameters to crop or viceversa

# Simulation range. This depends on data availability
clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00")

# Additional reporting variables
rep <- c("[Maize].Grain.Total.Wt*10 as Yield", 
         "[Maize].SowingDate")

## Use this to visualize APSIMX experimental file
# library(listviewer)
# apsimx::view_apsimx(expfile_name,
#                     src.dir = "~/agwise-cropping-innovation/Data/useCase_Rwanda_RAB/Maize/Landing/APSIM/")

countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)
level2 <- NA
AOI <- TRUE
pathIn_zone <- T
season <- 1


################################################################################
## DO NOT EDIT BELOW THIS LINE — Core processing code                         ##
################################################################################

# Source script and select function
script_to_source <- switch(cropping_system,
                           monocrop = "~/agwise-cropping-innovation/Scripts/APSIM/generic/02_MONOCROP_apsim.R",
                           intercrop = "~/agwise-cropping-innovation/Scripts/APSIM/generic/02_INTERCROP_apsim.R",
                           rotation = "~/agwise-cropping-innovation/Scripts/APSIM/generic/02_ROTATION_apsim.R")

source(script_to_source)

produce_apsim_file <- switch(cropping_system,
                             monocrop = apsimSpatialFactorial,
                             intercrop = apsimIntercrop,
                             rotation = apsimRotation)

arguments <- switch(cropping_system,
                    monocrop = list(
                      country = country, useCaseName = useCaseName, 
                      Crop = main_Crop, AOI = AOI, season = season, zone = NULL,
                      level2 = level2, pathIn_zone = pathIn_zone,
                      expfile_name = expfile_name, clck = clck, 
                      varietyid = main_Crop_varietyid, rep = rep, 
                      fix_crop_or_soil_parm = fix_crop_or_soil_parm
                    ),
                    intercrop = list(
                      country = country, useCaseName = useCaseName, 
                      Crops = Crops, AOI = AOI, season = season, zone = NULL,
                      level2 = level2, pathIn_zone = pathIn_zone, 
                      expfile_name = expfile_name, clck = clck, 
                      varietyids = varietyids, rep = rep, 
                      fix_crop_or_soil_parm = fix_crop_or_soil_parm
                    ),
                    rotation = list(
                      country = country, useCaseName = useCaseName, 
                      Crops = Crops, AOI = AOI, season = season, zone = NULL, 
                      level2 = level2, pathIn_zone = pathIn_zone, 
                      expfile_name = expfile_name, clck = clck, 
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
