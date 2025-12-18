path <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Ethiopia_PotentialYield/Wheat/transform/DSSAT/AOI/TI2020'
out_path <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Ethiopia_PotentialYield/Wheat/transform/DSSAT/AOI/TI2021'

# Get provinces
prov_path <- list.dirs(path,recursive = FALSE)
prov_p <- list.files(prov_path,recursive = TRUE)

for (p in 1:length(prov_path)){
  new_name <- 0
  out_path_p <- paste0(out_path,'/', prov_p[p])
  for (d in 1:length(path)){
    lwv <- list.files(path[d])

    for (k in 1:length(lwv)){
      
      new_name <- new_name + 1
      EXTE <- paste0('EXTE', formatC(width = 4, (as.integer(new_name)), flag = "0"))
      
      sol <- DSSAT::read_sol(paste0(path[d],'/',lwv[k],'/',EXTE,'/SOIL.SOL'))
      wth <- list.files(paste0(path[d],'/',lwv[k]), pattern = '.WTH',recursive = TRUE)
      #for (w in 1:length(wth)){
      wth <- DSSAT::read_wth(paste0(path[d],'/',lwv[k],'/',wth[1]))
      #new_name <- new_name + 1
      new <- paste0('TRAN', formatC(width = 5, (as.integer(new_name)), flag = "0"))
      w_new <- paste0('WHTE', formatC(width = 4, (as.integer(new_name)), flag = "0"))

      exte_new <- paste0('EXTE', formatC(width = 4, (as.integer(new_name)), flag = "0"))

      sol$PEDON <- new
       if (!dir.exists(paste0(out_path_p))){dir.create(paste0(out_path_p))}
      if (!dir.exists(paste0(out_path_p,'/',exte_new))){dir.create(paste0(out_path_p,'/',exte_new))}
      DSSAT::write_sol(sol, file  = paste0(out_path_p,'/',exte_new,'/','SOIL.SOL'), title = 'General DSSAT Soil Input File', append = FALSE)
      DSSAT::write_wth(wth, file  = paste0(out_path_p,'/',exte_new,'/',w_new,'.WTH'))
      
    }

  }
  print(path[d])
  print(k)
}








