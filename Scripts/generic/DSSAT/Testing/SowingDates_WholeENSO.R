library(doParallel)
registerDoParallel() 
getDoParWorkers()
library(dplyr)
library(foreach)
library(doParallel)

pyield_data <- readRDS("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Malawi_Solidaridad/Soybean/result/DSSAT/AOI/useCase_Malawi_Solidaridad_Soybean_AOI_season_1_ONI.RDS")
names(pyield_data)
head(pyield_data)

pyield_data$year<-substr(pyield_data$PDAT,1,4)
pyield_data$date<-substr(pyield_data$PDAT,1,10)
pyield_data$doy<-strftime(pyield_data$PDAT,format="%j")
pyield_data$ydoy<-strftime(pyield_data$PDAT,format="%y%j")
pyield_data$XY<-paste0(pyield_data$Long,"_",pyield_data$Lat)
pyield_data<-as.data.frame(pyield_data)
head(pyield_data)

gps <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Malawi_Solidaridad/Maize/result/AOI_GPS.RDS")

gps$XY<-paste0(gps$lon,"_",gps$lat)
head(gps)

pyd <- merge(gps, pyield_data,by='XY')
names(pyd)
head(pyd)

# Merge gps and red tables together
pyd<-pyd[,-c(7:9,12:13,26,29,30,33,34)]
names(pyd)

# colnames(pyd)<-c("XY","NAME_0", "NAME_1","NAME_2","Lon","Lat","LONG","TRNO","TNAM","PDAT",
#                  "HDAT","CWAM","yield","CNAM", "GNAM","NDCH","TMAXA","TMINA","SRADA","PRCP",
#                  "ETCP","ESCP","base","WUE","variety","prov", "ENSO","year","date","doy","ydoy")

colnames(pyd)<-c("XY","NAME_0", "NAME_1","NAME_2","Lon","Lat","TNAM","PDAT",
                 "HDAT","CWAM","yield","CNAM", "GNAM","NDCH","TMAXA","TMINA","SRADA","PRCP",
                 "ETCP","ESCP","base","WUE","variety","prov", "ONI", "ENSO","year","date","doy","ydoy")

pyd_select<-pyd[,c("XY","NAME_0", "NAME_1","NAME_2","Lon","Lat","PDAT","yield","variety","prov", "ONI","ENSO","year","date","doy","ydoy")]

head(pyd_select)
#subset for neutral

max_yld <- foreach(k =  1:length(unique(pyd_select$XY))) %dopar% {
  test <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
  enso<-foreach(e =  1:length(unique(pyd_select$ENSO)[-3])) %dopar% {
    enso.e<-subset(test, test$ENSO==unique(pyd_select$ENSO)[-3][e])
    enso.e <- enso.e %>% arrange(desc(yield))
    enso.e$yld_option <- c(1:nrow(enso.e))
    enso.e <- enso.e[1,]} #Top 5 variety and sowing date choices
    # enso.e <- enso.e[1:5,]} #Top 5 variety and sowing date choices
    enso <- do.call(rbind, enso)
}
max_yld <- do.call(rbind, max_yld)
names(max_yld)
head(max_yld)
#     
#   test <- test %>% arrange(desc(yield))
#   test <- test[1:3,]
#   test$yld_option <- c(1:nrow(test))
#   }

# ymax_pk <- NULL
# for (k in 1:length(unique(pyd_select$XY))){
#   pk <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
#   # pk <-  subset(pk, pk$variety!='EX899996')
#   ymax <- order(pk$yield, decreasing = TRUE)[1:5]
#   ymax <- pk[ymax,]
#   ymax$yield_option <- c(1:5)
#   ymax_pk <- rbind(ymax_pk,ymax)
# }

# ymax_pk$doy[ymax_pk$doy==338]<- '1-7 Dec'
# ymax_pk$doy[ymax_pk$doy==339]<- '2-8 Dec'
# ymax_pk$doy[ymax_pk$doy==345]<- '7-14 Dec'

max_yld$variety[max_yld$variety=="900111"]<- 'Short'
max_yld$variety[max_yld$variety=="900112"]<- 'Medium'
max_yld$variety[max_yld$variety=="900113"]<- 'Long'

max_yld1<-max_yld[,-c(1,7:8,10,13,15)]
names(max_yld1)
colnames(max_yld1)<-c("country", "province", "district", "Lon"," Lat","variety_type", "ENSO", "year","doy","Planting_option")

head(max_yld1,10)
# ymax_el<-ymax_pk1
# ymax_el

















































# 
# 
# ##################################################################################################################
# 
# #Subset for Niña
# pyd_select<-pyd_select[pyd_select$ENSO=='Niña',]
# max_yld <- foreach(k =  1:length(unique(pyd_select$XY))) %do% 
#   {test <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
#   # test <- subset(test, test$variety !='EX899996')
#   test <- test %>% arrange(desc(yield))
#   test <- test[1:3,]
#   test$yld_option <- c(1:nrow(test))
#   }
# max_yld <- do.call(rbind, max_yld)
# 
# ymax_pk <- NULL
# for (k in 1:length(unique(pyd_select$XY))){
#   pk <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
#   # pk <-  subset(pk, pk$variety!='EX899996')
#   ymax <- order(pk$yield, decreasing = TRUE)[1:5]
#   ymax <- pk[ymax,]
#   ymax$yield_option <- c(1:5)
#   ymax_pk <- rbind(ymax_pk,ymax)
# }
# 
# # ymax_pk$doy[ymax_pk$doy==338]<- '1-7 Dec'
# # ymax_pk$doy[ymax_pk$doy==339]<- '2-8 Dec'
# # ymax_pk$doy[ymax_pk$doy==345]<- '7-14 Dec'
# 
# ymax_pk$variety[ymax_pk$variety=="900111"]<- 'Short'
# ymax_pk$variety[ymax_pk$variety=="900112"]<- 'Medium'
# ymax_pk$variety[ymax_pk$variety=="900113"]<- 'Long'
# 
# ymax_pk1<-ymax_pk[,-c(1,7:8,10,12:13,15)]
# names(ymax_pk1)
# colnames(ymax_pk1)<-c("country", "province", "district", "Lon"," Lat","variety_type", "ENSO", "doy", "Best_planting_option")
# 
# head(ymax_pk1,5)
# ymax_la<-ymax_pk1
# ymax_la
# 
# ##################################################################################################################
# 
# #Subset for Neutral
# pyd_select<-pyd_select[pyd_select$ENSO=='Neutral',]
# max_yld <- foreach(k =  1:length(unique(pyd_select$XY))) %do% 
#   {test <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
#   # test <- subset(test, test$variety !='EX899996')
#   test <- test %>% arrange(desc(yield))
#   test <- test[1:3,]
#   test$yld_option <- c(1:nrow(test))
#   }
# max_yld <- do.call(rbind, max_yld)
# 
# ymax_pk <- NULL
# for (k in 1:length(unique(pyd_select$XY))){
#   pk <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
#   # pk <-  subset(pk, pk$variety!='EX899996')
#   ymax <- order(pk$yield, decreasing = TRUE)[1:5]
#   ymax <- pk[ymax,]
#   ymax$yield_option <- c(1:5)
#   ymax_pk <- rbind(ymax_pk,ymax)
# }
# 
# # ymax_pk$doy[ymax_pk$doy==338]<- '1-7 Dec'
# # ymax_pk$doy[ymax_pk$doy==339]<- '2-8 Dec'
# # ymax_pk$doy[ymax_pk$doy==345]<- '7-14 Dec'
# 
# ymax_pk$variety[ymax_pk$variety=="900111"]<- 'Short'
# ymax_pk$variety[ymax_pk$variety=="900112"]<- 'Medium'
# ymax_pk$variety[ymax_pk$variety=="900113"]<- 'Long'
# 
# ymax_pk1<-ymax_pk[,-c(1,7:8,10,12:13,15)]
# names(ymax_pk1)
# colnames(ymax_pk1)<-c("country", "province", "district", "Lon"," Lat","variety_type", "ENSO", "doy", "Best_planting_option")
# 
# head(ymax_pk1,5)
# ymax_neu<-ymax_pk1
# ymax_neu
# 
# ##################################################################################################################
# ymax_opt1<-rbind(ymax_el,ymax_la,ymax_neu)
# # ymax_opt<-cbind(ymax_opt1,ymax_neu)
# 
# 
# 
# 
# 
