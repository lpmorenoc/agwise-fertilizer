# A script for copying CUL files to EXTE folders

#' @file full path name for CUL file to be copied

#' @dest destination where the CUL will be copied to. 

#' created by Siyabusa, Patricia and Andrew

cul_file <- function(file, dest){

  test <- list.files(dest, pattern = "EXTE", recursive = TRUE)
  # test <- list.files(dest, pattern = "EXTE", recursive = TRUE)[1:8]
  
  test <- list.dirs(dest, recursive = TRUE, full.names = TRUE)
  
  testx <- grep("EXTE", test)

  testx <- test[testx]

  testx[1:5]
  
  for (l in 1:length(testx)){
    
    file.copy(file, testx[l])
    
    }

  paste0("Copied ", file, " file into ", length(testx), " experiment directories successfuly")
  }

# Maize
#file <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Mozambique_Solidaridad/Maize/Landing/DSSAT/MZCER048.CUL'

#dest <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Mozambique_Solidaridad/Maize/transform/DSSAT/AOI'


# Soybean
file <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zambia_Solidaridad/Soybean/Landing/DSSAT/SBGRO048.CUL'
dest <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zambia_Solidaridad/Soybean/transform/DSSAT/AOI'
cul_file(file, dest)
