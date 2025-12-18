library(dplyr)
library(foreach)
library(tidyverse)

country="Malawi"
useCaseName="Solidaridad"
Crop = "Soybean"
Extent = "AOI"
Season = 1
Plot = TRUE

pyield_data <- readRDS("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Malawi_Solidaridad/Soybean/result/DSSAT/AOI/useCase_Malawi_Solidaridad_Soybean_AOI_season_1_ONI.RDS")

names(pyield_data)
head(pyield_data,3)

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

pyd$ONI<-pyd$med_variable
pyd$NAME_0<-pyd$country
pyd$yield <-pyd$HWAH
pyd$prov <-pyd$Loc

pyd_select<-pyd[,c("XY","NAME_0", "NAME_1","NAME_2","lon","lat","TRNO","TNAM","PDAT",
                   "yield","Variety","prov","ONI","ENSO","year","date","doy",
                   "ydoy")]

summary_pyd<-pyd_select %>%  group_by(XY,lon,lat,Variety,ENSO,TRNO,TNAM,doy) %>%
  summarise(median = median(yield))

#get only top 3 optimum dates per ENSO per variety per location
max_median_summary_poptions <- summary_pyd %>%  
  group_by(XY,lon,lat,Variety,ENSO) %>% 
  slice_max(median,n=5) %>%
  select(TRNO,TNAM,median, doy) %>%
  arrange(XY,ENSO,Variety, desc(median))%>%
  filter(!is.na(ENSO))

#get only one optimum date
max_median_summary <- summary_pyd %>%  
  group_by(XY,lon,lat,Variety,ENSO) %>% 
  slice_max(median, with_ties = FALSE) %>%
  select(TRNO,TNAM,median, doy) %>%
  arrange(XY, desc(median))%>%
  filter(!is.na(ENSO))

###############################

# country="Malawi"
# useCaseName="Solidaridad"
# Crop = "Soybean"
# Extent = "AOI"
# Season = 1
# Plot = TRUE

countryShp <- geodata::gadm(country, level = 1, path=paste0("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName,"/",Crop,"/result/DSSAT/", sep=""))
country_sf <- sf::st_as_sf(countryShp)

max_median_summary <- max_median_summary %>%
  mutate(Variety = case_when(Variety=="999911" ~ "Short", Variety == "999912" ~ "Medium", Variety == "999913" ~ "Long"))

year <- 2023
max_median_summary$doyy <- as.Date(as.numeric(as.character(max_median_summary$doy))-1, origin = paste0(year, "-01-01"))
# max_median_summary$doy <- format(max_median_summary$doyy, "%m-%b")
max_median_summary$Opt_date <- format(max_median_summary$doyy, "%d-%b")

max_median_summary$TRNO <-factor(max_median_summary$TRNO)
# max_median_summary <- max_median_summary[order(max_median_summary$TRNO),]

max_median_summary$Opt_date <- factor(max_median_summary$Opt_date, 
                                      levels = unique(max_median_summary$Opt_date[order(max_median_summary$TRNO)]))

d<-max_median_summary %>%
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = Opt_date)) +
  labs(fill ="Opt. Date")+
  scale_fill_viridis_d()+
  facet_grid(Variety~ENSO)+
  theme_bw()+
  theme(legend.position = "right", aspect.ratio = 1, 
        axis.text.x = element_text(angle = 90, hjust = 1,size = 14, face ="bold"),
        axis.text.y = element_text(size = 14, face ="bold"),
        axis.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"))+ 
  geom_sf(data=country_sf, fill=NA, color="white", linewidth=0.5)+
  coord_sf(xlim=c(min(max_median_summary$lon), max(max_median_summary$lon)), 
           ylim=c(min(max_median_summary$lat), max(max_median_summary$lat)))+
  xlab("Longitude")+ ylab("Latitude")
ggsave(plot=d,"facet_plot.png", width = 12, height = 12)
d
summary_table <- max_median_summary %>%  group_by(ENSO, Variety,Opt_date) %>%  summarise(count = n())
# 
# 
# 
# oni_map <- function(data, x,y, fill,shp,factor1,factor2){
#   ggplot(data) +
#     geom_raster(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
#     facet_grid(factor1~factor2)+
#     theme_bw()+
#     theme(legend.position = "right")+ 
#     geom_sf(data=shp, fill=NA, color="white", linewidth=0.5)+
#     coord_sf(expand = FALSE, xlim=c(min(data$Lon), max(data$Lon)), ylim=c(min(data$Lat), max(data$Lat)))+
#     xlab("Longitude")+ ylab("Latitude") 
# }
# 
# 


oni_map(max_median_summary,x=Lon,y=Lat,fill=doy,shp=country_sf,factor1=variety,factor2=ENSO)

max_median_summary$doy<-max_median_summary

# Sample DOY vector
# doy <- c(1, 32, 60, 100, 200, 365)










# 
# 
# 
# 
# # Install and load lubridate if you haven't already
# install.packages("lubridate")
# library(lubridate)
# 
# # Sample data frame with DOY column
# data <- data.frame(id = 1:10, doy = c(1, 32, 60, 100, 150, 200, 250, 300, 365, 366))
# 
# year <- 2024
# data$date <- as.Date(data$doy - 1, origin = paste0(year, "-01-01"))
# data$short_date <- format(data$date, "%m-%d")
# print(data)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# head(pyd_select)
# #subset for Niño
# Niño<-pyd_select %>%
#   filter(ENSO=='Niño')
# # m<-which(Niño$ONI==max(Niño$ONI))
# # Niño<-unique(Niño$year)[order(unique(Niño$year),decreasing=TRUE)][1]
# Niño
# 
# #subset for Niña
# Niña<-pyd_select %>%
#   filter(ENSO=='Niña')  
# # m<-which(Niña$ONI==min(Niña$ONI))
# # Niña<-unique(Niña$year)[order(unique(Niña$year),decreasing=TRUE)][1]
# Niña
# 
# #subset for Neutral
# Neutral<-pyd_select %>%
#   filter(ENSO=='Neutral')
# # m<-which(Neutral$ONI==min(abs(Neutral$ONI)))
# # Neutral<-unique(Neutral$year)[order(unique(Neutral$year),decreasing=TRUE)][1]
# Neutral
# 
# y_Neu<-which(pyd_select$year %in% Neutral)
# y_Nino<-which(pyd_select$year %in% Niño)
# y_Nina<-which(pyd_select$year %in% Niña)
# 
# enso_Neu <-pyd_select[y_Neu,]
# enso_Nino <-pyd_select[y_Nino,]
# enso_Nina <-pyd_select[y_Nina,]
# 
# max_yld_Neu <- enso_Neu %>%
#   arrange(desc(XY),-yield)
# 
# max_yld_Nino <- enso_Nino %>%
#   arrange(desc(XY),-yield)
# 
# max_yld_Nina <- enso_Nina %>%
#   arrange(desc(XY),-yield)
# 
# max_yld_Nina_mod <- Reduce(rbind,							 
#                            by(max_yld_Nina,
#                               max_yld_Nina["XY"],
#                               head,
#                               n = 5))
# 
# max_yld_Nino_mod <- Reduce(rbind,							 
#                            by(max_yld_Nino,
#                               max_yld_Nino["XY"],
#                               head,
#                               n = 5))
# 
# max_yld_Neu_mod <- Reduce(rbind,							 
#                            by(max_yld_Neu,
#                               max_yld_Neu["XY"],
#                               head,
#                               n = 5))
# 
# max_opt_yld<-rbind(max_yld_Nina_mod,max_yld_Nino_mod,max_yld_Neu_mod)
# max_opt_yld
# 
# 
# 
# 
# 
# ################################################################################
# ##End##
# ################################################################################
# # max_yld_nina<-max_yld[max_yld$max_yld=='Niño']
# # max_yld<-enso_e[order(ENSO,-yield),]
# # max_yld
# # max_yld_opt<-enso_e[order(ENSO,-yield),]
# # max_yld_opt
# 
# # max_yld <- enso_e[order(enso_e$yield,
# #                                 decreasing = TRUE), ]
# 
# # select top 3 values from each group
# # max_yld_Nina_mod <- Reduce(rbind,							 
# #                    by(max_yld_Nina,
# #                       max_yld_Nina["XY"],
# #                       head,
# #                       n = 3))
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # pyd_select<-pyd_select[pyd_select$ENSO=='Niño',]
# # max_yld <- foreach(k =  1:length(unique(pyd_select$XY))) %dopar% {
# #     test <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
# #     enso<-foreach(e =  1:length(unique(pyd_select$ENSO)[-3])) %dopar% {
# #     # e=0
# #     # e<-e+1
# #     enso.e<-subset(pyd_select, pyd_select$ENSO==unique(pyd_select$ENSO)[-3][e])
# #     # enso.e<-subset(enso.e,enso.e$year==Neutral)
# #     y<-which(enso.e$year %in% c(Neutral,Niña,Niño))
# #     enso.e <-enso.e[y,]
# #     enso.e.neutral <- enso.e %>% 
# #       filter(ENSO=="Niño") %>%
# #       arrange(yield)
# #     enso.e$yld_option <- c(1:nrow(enso.e))
# #     enso.e[1:5,]
# #     }
# #     # enso.e$yld_option <- c(1:nrow(enso.e)) #last to be added
# #    enso <- do.call(rbind, enso)
# # }
# # max_yld <- do.call(rbind, max_yld)
# # names(max_yld)
# # head(max_yld)
# # #     
# # #   test <- test %>% arrange(desc(yield))
# # #   test <- test[1:3,]
# # #   test$yld_option <- c(1:nrow(test))
# # #   }
# # 
# # # ymax_pk <- NULL
# # # for (k in 1:length(unique(pyd_select$XY))){
# # #   pk <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
# # #   # pk <-  subset(pk, pk$variety!='EX899996')
# # #   ymax <- order(pk$yield, decreasing = TRUE)[1:5]
# # #   ymax <- pk[ymax,]
# # #   ymax$yield_option <- c(1:5)
# # #   ymax_pk <- rbind(ymax_pk,ymax)
# # # }
# # 
# # # ymax_pk$doy[ymax_pk$doy==338]<- '1-7 Dec'
# # # ymax_pk$doy[ymax_pk$doy==339]<- '2-8 Dec'
# # # ymax_pk$doy[ymax_pk$doy==345]<- '7-14 Dec'
# # 
# # max_yld$variety[max_yld$variety=="900111"]<- 'Short'
# # max_yld$variety[max_yld$variety=="900112"]<- 'Medium'
# # max_yld$variety[max_yld$variety=="900113"]<- 'Long'
# # 
# # max_yld1<-max_yld[,-c(1,7:8,10,13,15)]
# # names(max_yld1)
# # colnames(max_yld1)<-c("country", "province", "district", "Lon"," Lat","variety_type", "ENSO", "year","doy","Planting_option")
# # 
# # head(max_yld1,10)
# # # ymax_el<-ymax_pk1
# # # ymax_el
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # 
# # ##################################################################################################################
# # 
# # #Subset for Niña
# # pyd_select<-pyd_select[pyd_select$ENSO=='Niña',]
# # max_yld <- foreach(k =  1:length(unique(pyd_select$XY))) %do% 
# #   {test <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
# #   # test <- subset(test, test$variety !='EX899996')
# #   test <- test %>% arrange(desc(yield))
# #   test <- test[1:3,]
# #   test$yld_option <- c(1:nrow(test))
# #   }
# # max_yld <- do.call(rbind, max_yld)
# # 
# # ymax_pk <- NULL
# # for (k in 1:length(unique(pyd_select$XY))){
# #   pk <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
# #   # pk <-  subset(pk, pk$variety!='EX899996')
# #   ymax <- order(pk$yield, decreasing = TRUE)[1:5]
# #   ymax <- pk[ymax,]
# #   ymax$yield_option <- c(1:5)
# #   ymax_pk <- rbind(ymax_pk,ymax)
# # }
# # 
# # # ymax_pk$doy[ymax_pk$doy==338]<- '1-7 Dec'
# # # ymax_pk$doy[ymax_pk$doy==339]<- '2-8 Dec'
# # # ymax_pk$doy[ymax_pk$doy==345]<- '7-14 Dec'
# # 
# # ymax_pk$variety[ymax_pk$variety=="900111"]<- 'Short'
# # ymax_pk$variety[ymax_pk$variety=="900112"]<- 'Medium'
# # ymax_pk$variety[ymax_pk$variety=="900113"]<- 'Long'
# # 
# # ymax_pk1<-ymax_pk[,-c(1,7:8,10,12:13,15)]
# # names(ymax_pk1)
# # colnames(ymax_pk1)<-c("country", "province", "district", "Lon"," Lat","variety_type", "ENSO", "doy", "Best_planting_option")
# # 
# # head(ymax_pk1,5)
# # ymax_la<-ymax_pk1
# # ymax_la
# # 
# # ##################################################################################################################
# # 
# # #Subset for Neutral
# # pyd_select<-pyd_select[pyd_select$ENSO=='Neutral',]
# # max_yld <- foreach(k =  1:length(unique(pyd_select$XY))) %do% 
# #   {test <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
# #   # test <- subset(test, test$variety !='EX899996')
# #   test <- test %>% arrange(desc(yield))
# #   test <- test[1:3,]
# #   test$yld_option <- c(1:nrow(test))
# #   }
# # max_yld <- do.call(rbind, max_yld)
# # 
# # ymax_pk <- NULL
# # for (k in 1:length(unique(pyd_select$XY))){
# #   pk <- subset(pyd_select, pyd_select$XY==unique(pyd_select$XY)[k])
# #   # pk <-  subset(pk, pk$variety!='EX899996')
# #   ymax <- order(pk$yield, decreasing = TRUE)[1:5]
# #   ymax <- pk[ymax,]
# #   ymax$yield_option <- c(1:5)
# #   ymax_pk <- rbind(ymax_pk,ymax)
# # }
# # 
# # # ymax_pk$doy[ymax_pk$doy==338]<- '1-7 Dec'
# # # ymax_pk$doy[ymax_pk$doy==339]<- '2-8 Dec'
# # # ymax_pk$doy[ymax_pk$doy==345]<- '7-14 Dec'
# # 
# # ymax_pk$variety[ymax_pk$variety=="900111"]<- 'Short'
# # ymax_pk$variety[ymax_pk$variety=="900112"]<- 'Medium'
# # ymax_pk$variety[ymax_pk$variety=="900113"]<- 'Long'
# # 
# # ymax_pk1<-ymax_pk[,-c(1,7:8,10,12:13,15)]
# # names(ymax_pk1)
# # colnames(ymax_pk1)<-c("country", "province", "district", "Lon"," Lat","variety_type", "ENSO", "doy", "Best_planting_option")
# # 
# # head(ymax_pk1,5)
# # ymax_neu<-ymax_pk1
# # ymax_neu
# # 
# # ##################################################################################################################
# # ymax_opt1<-rbind(ymax_el,ymax_la,ymax_neu)
# # # ymax_opt<-cbind(ymax_opt1,ymax_neu)
# # 
# # 
# # 
# # 
# # 
