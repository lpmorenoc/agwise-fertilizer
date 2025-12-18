populate_fert_n_trt_factorial <-
function(file_x, fert_df, plant_dates, 
                                          file_x_original){
  dap <- map(fert_dfs, "F.dap")  # Extract application date from fertilzer
  fert_dfs <- map(fert_dfs, ~ select(.x, -F.dap))  # Remove application date
  
  fert_x <- dplyr::slice(file_x_original$`FERTILIZERS (INORGANIC)`, 0)
  f_ij <- 0
  for (i in seq_along(plant_dates)){
    for (j in seq_along(fert_dfs)){
      f_ij <- f_ij + 1
      fert_x_ij <- fert_dfs[[j]]
      
      fert_x_ij$F <- f_ij
      fert_x_ij$FDATE <- as.POSIXct(plant_dates[i] + dap[[j]])
      
      fert_x <- bind_rows(fert_x, fert_x_ij)
    }
  }
  
  file_x$`FERTILIZERS (INORGANIC)` <- fert_x
  
  trt_x_original <-  file_x_original$`TREATMENTS                        -------------FACTOR LEVELS------------`
  trt_x <- dplyr::slice(trt_x_original, 0)
  trt_ij <- 0
  for (i in seq_along(plant_dates)){
    for (j in seq_along(fert_dfs)){
      trt_ij <- trt_ij + 1
      trt_x_ij <- trt_x_original
      
      trt_x_ij <- trt_x_ij %>%
        mutate(
          N = trt_ij,
          TNAME = paste(fert_dfs[[j]]$FERNAME[1], "@", format(plant_dates[i], "%m-%d"), sep= " "),
          MF = j,
          across(c(MH, MP, IC, SM), ~ i)
      )
      
      trt_x <- bind_rows(trt_x, trt_x_ij)
    }
  }
  file_x$`TREATMENTS                        -------------FACTOR LEVELS------------` <- trt_x
  
  return(file_x)
}
