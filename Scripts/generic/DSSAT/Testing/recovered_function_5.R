rundssat <-
function(i,path.to.extdata,TRT,AOI=TRUE,crop_code){
  setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))

  # Generate a DSSAT batch file using a tibble
  options(DSSAT.CSM="/opt/DSSAT/v4.8.1.40/dscsm048")
  tibble(FILEX=paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',crop_code,'X'), TRTNO=TRT, RP=1, SQ=0, OP=0, CO=0) %>%
    write_dssbatch(file_name="DSSBatch.v48")
  # Run DSSAT-CSM
  run_dssat(file_name="DSSBatch.v48",suppress_output = TRUE)
  # Change output file name
  new_file <-  paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.OUT')
  # Check if the output file already exists and remove it if it does
  if (file.exists(new_file)) {
    file.remove(new_file)
  }
  file.rename("Summary.OUT",new_file)
  gc()
}
