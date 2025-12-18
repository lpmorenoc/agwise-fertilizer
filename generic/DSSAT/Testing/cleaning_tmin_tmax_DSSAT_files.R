# Rename
path <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Nigeria_SSA/Maize/transform/DSSAT/AOI/'
# path <- '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Malawi_Solidaridad/Maize/transform/DSSAT/AOI/999213/'
# dir.create('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zimbabwe_Solidaridad/Soybean/transform/DSSAT/AOI_new/')
# pathw <-  '~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Zimbabwe_Solidaridad/Soybean/transform/DSSAT/AOI_new/'

# Get varieties
#prov_path <- list.dirs(path,recursive = TRUE)

prov_v <- list.files(path,recursive = TRUE, pattern = ".WTH")

for (k in 1: length(prov_v)){
wth <- DSSAT::read_wth(paste0(path,prov_v[k]))

wth$Tmax <- ifelse(wth$TMAX<wth$TMIN, wth$TMIN,wth$TMAX)

wth$Tmin <- ifelse(wth$TMAX<wth$TMIN, wth$TMAX, wth$TMIN)

wth$TMAX <- wth$Tmax

wth$TMIN <- wth$Tmin

wth$TMAX <- ifelse(wth$TMAX==wth$TMIN, wth$TMAX+1,wth$TMAX)

wth <- wth[,-c(6:7)]

fds <- strsplit(prov_v[k],"/")[[1]]

DSSAT::write_wth(wth, file  = paste0(path,prov_v[k]))

# To write to a different folder e.g pathw use the following

# dir.create(paste0(pathw, fds[1],"/", fds[2], "/", fds[3], sep = "/"))
# 
# dir.create(paste0(pathw, fds[1]))
# 
# dir.create(paste0(paste0(pathw, fds[1]),"/",fds[2]))
# 
# dir.create(paste0(paste0(paste0(pathw, fds[1]),"/",fds[2]),"/", fds[3]))
# 
# DSSAT::write_wth(wth, file  = paste0(pathw,prov_v[k]))
}







