# setwd('~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Ghana_Esoko/Maize/raw/Climate_ChangeDSSAT/') #Set working directory where the results will be downloaded into
setwd('~/Data_analytics/KPI_ClimateData_DSSAT')
base <- 'https://www.pik-potsdam.de/~chemura/IITA_EIA/DSSAT_weather/HIST'

folders <- c('GFDL', 'IPSL', 'MPIE', 'MRIE', 'UKSM')

for (f in 1:length(folders)){
	
dir.create(paste0(getwd(),'/',folders[f]))	

thepage <- readLines(paste0(base,'/',folders[f]))

m <- grep('*.WTH', thepage, value = FALSE)

pages <- thepage[m]

url <- paste0(base,'/',folders[f])

for(p in 1:length(pages)){
	
purl <- paste0(url, '/',substr(pages[p], 67,78))

download.file(purl,paste0(paste0(getwd(),'/',folders[f]),'/',substr(pages[p], 67,78)))

}}
