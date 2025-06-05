


   library(sp)
   library(spatialEco)
   library(raster)
   library(dplyr)
   library(geosphere)
   labelInput
#########################################################################################################
  date1=substr(basename(labelInput),1,15)
  pthTableP=paste0(labelInput,"\\",date1, "Points.csv")
  pthTableNew=paste0(labelInput,"\\",date1, "_CountDist.csv")
  
  if (file.exists(pthTableP)==T){
  
  table1= read.csv(pthTableP)
 # table1<-data.frame(sex=table1$sex, lat=table1$lat,lon=table1$lon,imgName=table1$imgName, west=table1$west,east=table1$east,south=table1$south,north=table1$north, #north50=table1$north50,south50=table1$south50,west50=table1$west50,east50=table1$east50,date1=table1$date1)
  imgList=unique(table1$imgName)
####################################################################################################################
for (i in 1:length(imgList))  {
  options(warn=-1)
  img=paste0(imgList[i])
  listAnimals = table1[table1$imgName== img,] 

  Limit512=unique(data.frame(west=listAnimals$west,east=listAnimals$east,south=listAnimals$south,north=listAnimals$north))
  Poligon512 <- as(raster::extent(Limit512$west,Limit512$east,Limit512$south,Limit512$north), "SpatialPolygons")
  proj4string(Poligon512) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  ###########################################################################################
  Limit1024=unique(data.frame(west50=listAnimals$west50,east50=listAnimals$east50,south50=listAnimals$south50,north50=listAnimals$north50))
  Poligon1024 <- as(raster::extent(Limit1024$west50,Limit1024$east50,Limit1024$south50,Limit1024$north50), "SpatialPolygons")
  proj4string(Poligon1024) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  ###########################################################################################
  coords <- data.frame(lat= listAnimals$lat, lon=listAnimals$lon)   # coordinates
  data   <- data.frame(sex= listAnimals$sex)   # data
  crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") # proj4string of coords
  Points <- SpatialPointsDataFrame(coords = coords,
                                   data = data, 
                                   proj4string = crs)
  ###############################################################  FILTER POINTS FOR THE IMAGE

  
  ModelIN <- point.in.poly(Points,Poligon512) 
 
  MaskPoint=as.data.frame(ModelIN)
  imagePoint=MaskPoint[is.na(MaskPoint[,2]) == FALSE,]
  imagePoint=data.frame(sex=imagePoint$sex,lat=imagePoint$coords.x1,lon=imagePoint$coords.x2)
  SexList=as.vector(unique(imagePoint$sex))
  if (length(SexList) > 0 ) {
   table1$All512[table1$imgName==imgList[i]]=length(imagePoint$sex)
  #  CountPointF= imagePoint %>% 
  #    filter(sex== "F")%>%
  #    group_by(sex) %>%
  #    summarise(count=n())
  #  CountPointF=data.frame(CountPointF)
  #  if (length(CountPointF$sex) != 0) {
  #    table1$F512[table1$imgName==imgList[i]]=  as.numeric(CountPointF$count)
  #  }
  }
  #######################################################################################

  ModelIN <- point.in.poly(Points,Poligon1024) 
 
  MaskPoint=as.data.frame(ModelIN)
  imagePoint=MaskPoint[is.na(MaskPoint[,2]) == FALSE,]
  imagePoint=data.frame(sex=imagePoint$sex,lat=imagePoint$coords.x1,lon=imagePoint$coords.x2)
  SexList=as.vector(unique(imagePoint$sex))
  if (length(SexList) > 0 ) {
    table1$All1024[table1$imgName==imgList[i]]=length(imagePoint$sex)
    #  CountPointF= imagePoint %>% 
    #  filter(sex== "F")%>%
    #  group_by(sex) %>%
    #  summarise(count=n())
    #  CountPointF=data.frame(CountPointF)
    #  if (length(CountPointF$sex) != 0) {
    #  table1$F1024[table1$imgName==imgList[i]]=  as.numeric(CountPointF$count)
  #  }
    }
  ###################################################################################
  mdist <- distm(Points)
  minMdist = min(mdist[mdist !=0])   
  table1$MinDist[table1$imgName==imgList[i]]= minMdist
  #############################################
}
table3=data.frame(date1=table1$date1,  imgName=table1$imgName,All1024=table1$All1024,All512=table1$All512, MinMdist=table1$MinDist) #F512=table1$F512, F1024=table1$F1024
table4=unique(table3)
write.csv(table4, pthTableNew)
options(warn=0)
}