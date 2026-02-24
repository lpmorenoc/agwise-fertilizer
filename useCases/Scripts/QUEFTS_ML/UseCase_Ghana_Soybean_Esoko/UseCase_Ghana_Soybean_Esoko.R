setwd("~/agwise_data_sourcing") # working directory

# install necessary libraries - per the README
install.packages(c("reticulate", "yaml"), repos = "https://cloud.r-project.org")
library(reticulate)
library(yaml)


## add the create environment in conda
Sys.setenv(RETICULATE_CONDA = system("which conda", intern = TRUE))
use_condaenv("agwise", required = TRUE)

## check the configuration
py_config()
py

## YAML Configurations for Ghana

# GEE project here
cfg$GENERAL_SETTINGS$ee_project_name <- "gacheri"

# Output folder (keeps outputs under runs/)
cfg$GENERAL_SETTINGS$output_path <- "runs"

# 3) Ghana whole country
cfg$DATA_DOWNLOAD$ADM0_NAME <- "Ghana"
cfg$DATA_DOWNLOAD$ADM1_NAME <- NULL
cfg$DATA_DOWNLOAD$ADM2_NAME <- NULL
cfg$DATA_DOWNLOAD$coordinate <- NULL
cfg$DATA_DOWNLOAD$adm_level <- "ADM0"

# Try 500m
cfg$DATA_DOWNLOAD$scale <- 5000

out_yaml <- "runs/configs/dem_Ghana_ADM0_500.yaml"
dir.create(dirname(out_yaml), recursive = TRUE, showWarnings = FALSE)
write_yaml(cfg, out_yaml)

out_yaml

# RUN the DEM script
log_file <- "runs/logs/dem_Ghana_ADM0_5000.log"
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

status <- system2(py,
                  args = c("download_dem.py", "-config", out_yaml),
                  stdout = log_file,
                  stderr = log_file)

status
cat(readLines(log_file), sep = "\n")






# log_file <- "runs/logs/dem_Ghana_ADM0_500.log"
# dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
# 
# status <- system2(py,
#                   args = c("download_dem.py", "-config", out_yaml),
#                   stdout = log_file,
#                   stderr = log_file)
# 
# cat(readLines(log_file), sep = "\n")
# print(status)
# 
# 
# 
# 
# 
# # 4) Run the python script
# out <- system2(py, args = c("download_dem.py", "-config", out_yaml),
#                stdout = TRUE, stderr = TRUE)
# cat(paste(out, collapse = "\n"))
# 
# 
# # Data sourcing - geospatial.R
# #  Rw ag data from carob. R
# #  Cleaning.R
# #  Training.R
# #  Blups.R
# #  predictiob.R
# #  Optimisation.R