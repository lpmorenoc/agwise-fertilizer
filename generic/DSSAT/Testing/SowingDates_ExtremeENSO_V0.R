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

colnames(pyd)<-c("XY","NAME_0", "NAME_1","NAME_2","Lon","Lat","TNAM","PDAT",
                 "HDAT","CWAM","yield","CNAM", "GNAM","NDCH","TMAXA","TMINA","SRADA","PRCP",
                 "ETCP","ESCP","base","WUE","variety","prov", "ONI", "ENSO","year","date","doy","ydoy")

pyd_select<-pyd[,c("XY","NAME_0", "NAME_1","NAME_2","Lon","Lat","PDAT","yield","variety","prov", "ONI","ENSO","year","date","doy","ydoy")]

head(pyd_select)
#subset for Niño
Niño<-pyd_select %>%
  filter(ENSO=='Niño')
m<-which(Niño$ONI==max(Niño$ONI))
Niño<-unique(Niño$year)[order(unique(Niño$year),decreasing=TRUE)][1]
Niño

#subset for Niña
Niña<-pyd_select %>%
  filter(ENSO=='Niña')  
m<-which(Niña$ONI==min(Niña$ONI))
Niña<-unique(Niña$year)[order(unique(Niña$year),decreasing=TRUE)][1]
Niña

#subset for Neutral
Neutral<-pyd_select %>%
  filter(ENSO=='Neutral')
m<-which(Neutral$ONI==min(abs(Neutral$ONI)))
Neutral<-unique(Neutral$year)[order(unique(Neutral$year),decreasing=TRUE)][1]
Neutral

y_Neu<-which(pyd_select$year %in% Neutral)
y_Nino<-which(pyd_select$year %in% Niño)
y_Nina<-which(pyd_select$year %in% Niña)

enso_Neu <-pyd_select[y_Neu,]
enso_Nino <-pyd_select[y_Nino,]
enso_Nina <-pyd_select[y_Nina,]

max_yld_Neu <- enso_Neu %>%
  arrange(desc(XY),-yield)

max_yld_Nino <- enso_Nino %>%
  arrange(desc(XY),-yield)

max_yld_Nina <- enso_Nina %>%
  arrange(desc(XY),-yield)

max_yld_Nina_mod <- Reduce(rbind,							 
                           by(max_yld_Nina,
                              max_yld_Nina["XY"],
                              head,
                              n = 5))

max_yld_Nino_mod <- Reduce(rbind,							 
                           by(max_yld_Nino,
                              max_yld_Nino["XY"],
                              head,
                              n = 5))

max_yld_Neu_mod <- Reduce(rbind,							 
                           by(max_yld_Neu,
                              max_yld_Neu["XY"],
                              head,
                              n = 5))

max_opt_yld<-rbind(max_yld_Nina_mod,max_yld_Nino_mod,max_yld_Neu_mod)
max_opt_yld





################################################################################
##End##
################################################################################
# max_yld_nina<-max_yld[max_yld$max_yld=='Niño']
# max_yld<-enso_e[order(ENSO,-yield),]
# max_yld
# max_yld_opt<-enso_e[order(ENSO,-yield),]
# max_yld_opt

# max_yld <- enso_e[order(enso_e$yield,
#                                 decreasing = TRUE), ]

# select top 3 values from each group
# max_yld_Nina_mod <- Reduce(rbind,							 
#                    by(max_yld_Nina,
#                       max_yld_Nina["XY"],
#                       head,
#                       n = 3))
# 
# 
# 
# 
# 
# 
# 
# 
# # pyd_select<-pyd_select[pyd_select$ENSO=='Niño',]
# max_yld <- foreach(k =  1:length(unique(pyd_select$XY))) %dopar% {
#     test <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
#     enso<-foreach(e =  1:length(unique(pyd_select$ENSO)[-3])) %dopar% {
#     # e=0
#     # e<-e+1
#     enso.e<-subset(pyd_select, pyd_select$ENSO==unique(pyd_select$ENSO)[-3][e])
#     # enso.e<-subset(enso.e,enso.e$year==Neutral)
#     y<-which(enso.e$year %in% c(Neutral,Niña,Niño))
#     enso.e <-enso.e[y,]
#     enso.e.neutral <- enso.e %>% 
#       filter(ENSO=="Niño") %>%
#       arrange(yield)
#     enso.e$yld_option <- c(1:nrow(enso.e))
#     enso.e[1:5,]
#     }
#     # enso.e$yld_option <- c(1:nrow(enso.e)) #last to be added
#    enso <- do.call(rbind, enso)
# }
# max_yld <- do.call(rbind, max_yld)
# names(max_yld)
# head(max_yld)
# #     
# #   test <- test %>% arrange(desc(yield))
# #   test <- test[1:3,]
# #   test$yld_option <- c(1:nrow(test))
# #   }
# 
# # ymax_pk <- NULL
# # for (k in 1:length(unique(pyd_select$XY))){
# #   pk <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
# #   # pk <-  subset(pk, pk$variety!='EX899996')
# #   ymax <- order(pk$yield, decreasing = TRUE)[1:5]
# #   ymax <- pk[ymax,]
# #   ymax$yield_option <- c(1:5)
# #   ymax_pk <- rbind(ymax_pk,ymax)
# # }
# 
# # ymax_pk$doy[ymax_pk$doy==338]<- '1-7 Dec'
# # ymax_pk$doy[ymax_pk$doy==339]<- '2-8 Dec'
# # ymax_pk$doy[ymax_pk$doy==345]<- '7-14 Dec'
# 
# max_yld$variety[max_yld$variety=="900111"]<- 'Short'
# max_yld$variety[max_yld$variety=="900112"]<- 'Medium'
# max_yld$variety[max_yld$variety=="900113"]<- 'Long'
# 
# max_yld1<-max_yld[,-c(1,7:8,10,13,15)]
# names(max_yld1)
# colnames(max_yld1)<-c("country", "province", "district", "Lon"," Lat","variety_type", "ENSO", "year","doy","Planting_option")
# 
# head(max_yld1,10)
# # ymax_el<-ymax_pk1
# # ymax_el

















































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
