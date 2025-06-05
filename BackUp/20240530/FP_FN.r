#https://stackoverflow.com/questions/73325202/create-circular-areas-around-a-coordinate

# Библиотеки
library(sf)
library(tidyverse)
library(rgdal)

# Входные данные
    labelInput 
    date1 <- substr(basename(labelInput),1,15)
    NFS_Pup_weight_pth <- "2021_Pryb_trainval_20221226"
    savePth <-paste0(labelInput,"\\",basename(labelInput),"_FP-FN.csv")
    ObserverPointP=list.files(paste0(labelInput,"\\Observer_count"), full.names=T, pattern="kml|shp")[1]
				if (is.na(ObserverPointP)==T){ ObserverPointP=list.files(paste0(labelInput,"\\Polygons\\All_layers"), full.names=T, pattern="kml|shp")[1]}
				predPth = paste0(labelInput,"\\Predict\\","#",NFS_Pup_weight_pth,"#",date1,".kml")

# Чтение данных
obs <- readOGR(ObserverPointP)
prd <- readOGR(predPth)

# Преобразование в sf
OBS_CORD <- st_as_sf(obs)
PRED_CORD <- st_as_sf(prd)

# Создание полигона вокруг точек наблюдения
OBSER_POL <- st_buffer(x = OBS_CORD, dist=0.2)

# Нахождение перекрывающихся точек
OVERLAPPING_POINTS <- st_intersection(PRED_CORD, OBSER_POL)

# Расчет TP, FN и FP
TP <- length(OVERLAPPING_POINTS$Name)
obs <- length(obs)
prd <- length(prd)
FN <- obs - TP
FP <- prd - TP

# Сохранение результатов
report <- data.frame(OPP=labelInput,TP=TP,FN=FN,FP=FP, obs=obs,prd=prd)
write.csv(report,savePth)



#plot(st_geometry(OBSER_POL))
#st_write(OBSER_POL,"x.kml")