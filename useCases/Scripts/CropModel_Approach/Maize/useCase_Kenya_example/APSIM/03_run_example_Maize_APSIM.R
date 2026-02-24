# This script runs APSIM simulations for the defined experimental file


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
sec_Crop_varietyid <- "Florunner"  # Peanut cultivar
# sec_Crop_varietyid <- "Soya791"  # Soybean cultivar
varietyids <- c(main_Crop_varietyid, sec_Crop_varietyid)

## Use this to visualize APSIMX experimental file
# apsimx::view_apsimx(expfile_name, 
#                     src.dir = "/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Maize/transform/APSIM/Early/Amajyaruguru/EXTE0001/")

countryShp <- geodata::gadm(country, level = 2, path='.')
prov <- unique(countryShp$NAME_1)
level2 <- NA
AOI <- TRUE
pathIn_zone <- T
season <- 1


################################################################################
## DO NOT EDIT BELOW THIS LINE — Core processing code                         ##
################################################################################

##################################
## Run APSIMX experimental file ##
##################################

# Source script and select function
script_to_source <- switch(cropping_system,
                           monocrop = "/home/jovyan/agwise-cropping-innovation/Scripts/APSIM/generic/03_MONOCROP_RunSim.R",
                           intercrop = "/home/jovyan/agwise-cropping-innovation/Scripts/APSIM/generic/03_INTERCROP_RunSim.R",
                           rotation = "/home/jovyan/agwise-cropping-innovation/Scripts/APSIM/generic/03_ROTATION_RunSim.R")

source(script_to_source)

arguments <- switch(cropping_system,
                    monocrop = list(
                      country = country, useCaseName = useCaseName, 
                      Crop = main_Crop, AOI = AOI, expfile_name = expfile_name,
                      varietyid = main_Crop_varietyid, zone=NULL,
                      level2 = level2
                    ),
                    intercrop = list(
                      country = country, useCaseName = useCaseName, 
                      Crops = Crops, AOI = AOI, expfile_name = expfile_name,
                      varietyids = varietyids, zone = NULL, level2 = level2
                    ),
                    rotation = list(
                      country = country, useCaseName = useCaseName, 
                      Crops = Crops, AOI = AOI, expfile_name = expfile_name,
                      varietyids = varietyids, zone = NULL, level2 = level2
                    ))

start_time <- Sys.time()

# Loop through zones
for (z in prov) {
  args <- arguments
  args$zone <- z
  do.call(apsim.exec, args)
}

end_time <- Sys.time()
duration <- end_time - start_time
cat("Time for simulations:", duration, "min")

##########################
## Merge APSIMX outputs ##
##########################

# Source script and select function
script_to_source <- switch(cropping_system,
                           monocrop = "/home/jovyan/agwise-cropping-innovation/Scripts/APSIM/generic//04_MONOCROP_merge_APSIM_output.R",
                           intercrop = "/home/jovyan/agwise-cropping-innovation/Scripts/APSIM/generic/04_INTERCROP_merge_APSIM_output.R",
                           rotation = "/home/jovyan/agwise-cropping-innovation/Scripts/APSIM/generic/04_ROTATION_merge_APSIM_output.R")

source(script_to_source)

merge_outputs <- switch(cropping_system,
                        monocrop = merge_APSIM_output,
                        intercrop = merge_INTERCROP_APSIM_output,
                        rotation = merge_ROTATION_APSIM_output)

arguments <- switch(cropping_system,
                    monocrop = list(
                      country = country, useCaseName = useCaseName, 
                      Crop = main_Crop, expfile_name = expfile_name, AOI = AOI,
                      season = season, varietyids = main_Crop_varietyid
                    ),
                    intercrop = list(
                      country = country, useCaseName = useCaseName, 
                      Crops = Crops, expfile_name = expfile_name, AOI = AOI,
                      season = season, varietyids = varietyids
                    ),
                    rotation = list(
                      country = country, useCaseName = useCaseName, 
                      Crops = Crops, expfile_name = expfile_name, AOI = AOI,
                      season = season, varietyids = varietyids
                    ))

do.call(merge_outputs, arguments)

cat("Simulations merged and saved.")