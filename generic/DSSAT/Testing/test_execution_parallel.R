#################################################################################################################
## Run the DSSAT model
#################################################################################################################
source("~/agwise-potentialyield/dataops/potentialyield/Script/generic/DSSAT/dssat_exec.R")

#From parameter country to parameter varieties need to be modified.
#The rest should not be modified except for line 26 (prov or number of provinces to simulate)
country = "Rwanda"
useCaseName = "RAB"
Crop = "Wheat"
AOI = TRUE
TRT=1:2
level2=NA
varieties <- c("IB1101","IB1102")

#Define an initial variety to identify the number of zones (provinces)
varietyid <- varieties[1]

if(AOI == TRUE){
  path.to.extdata_ini <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/AOI/", sep="")
}else{
  path.to.extdata_ini <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/transform/DSSAT/fieldData/", sep="")
}

prov <- list.files(paste0(path.to.extdata_ini,varietyid))

prov <-prov[1:2]

start_time <- Sys.time()

plan(multisession, workers = 11)
# Define the list of combinations of varieties and provinces
combinations <- expand.grid(varieties = varieties, prov = prov)

#Define log file 
log_file <- (paste0(path.to.extdata_ini,"progress_log_run.txt"))


if(file.exists(log_file)){file.remove(log_file)}
# Use future_lapply to parallelize the outer loop
results <- future_lapply(seq_len(nrow(combinations)), function(idx) {
  variety <- combinations$varieties[idx]
  province <- combinations$prov[idx]
  
  progress_message <- paste("Progress",variety,province, i, "out of", nrow(combinations))
  cat(progress_message, "\n", file = log_file, append = TRUE)
  
  dssat.exec(
    country = country,
    useCaseName = useCaseName,
    Crop = Crop,
    AOI = AOI,
    TRT = TRT,
    varietyid = variety,
    zone = province,
    level2 = level2
  )
  finish_message <- paste("Finished",variety,province, i, "out of", nrow(combinations))
  cat(finish_message, "\n", file = log_file, append = TRUE)

})


# End timing
end_time <- Sys.time()

# Calculate the duration
duration <- end_time - start_time
# Print the duration
print(duration)
