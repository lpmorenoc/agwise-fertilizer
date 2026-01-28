library(raster)

library(dplyr)

library(xlsx)

# 1. Start with Ethiopia

getData('ISO3')

# Get country shape file
getData('GADM', country = 'RWA', level = 2, path = "~/agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RAB/Potato/result")

# Read back the layers
locs <- readRDS('~/agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RAB/Potato/result/gadm36_RWA_2_sp.rds')

# Get field locations with planting and harvest dates

pts <- readRDS('~/agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RAB/Potato/result/compiled_fieldData.RDS')


#pts <- na.omit(pts)

coordinates(pts) <- ~ lon + lat


proj4string(pts) <- proj4string(locs) # To assign the CRS (IF IT IS KNOWN!!!)

str(pts) # Note the presence of proj4string info

# Extract name_1 and name2 for the field data
locp <- extract(locs , pts)

names(locp)

field_provs <- locp[, c('GID_0', 'NAME_0', "NAME_1", "NAME_2")]

pts <- readRDS('~/agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RAB/Potato/result/compiled_fieldData.RDS')

field_provs <-  cbind(field_provs, pts)

saveRDS(field_provs, file = "~/agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RAB/Potato/result/re_compiled_fieldData.RDS")
