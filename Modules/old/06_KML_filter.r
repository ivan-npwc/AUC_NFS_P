source("Modules/07.1_KMLwrite_function.r")

  library(dplyr)
  Filter_by_Rookery_pol = TRUE
                         
					 
							 labelInput
							 date1=substr(basename(labelInput),1,15)
					         Species= "NFSPup"
                             crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
							 pathTablePoints=paste0(labelInput, "\\Predict\\#2021_PrybSevernoe_trainval_20221228#",date1, ".csv")
					         dat<-read.csv(pathTablePoints)
                             if (Species =="NFSPup") {BlobFemaleLimit=70}
							 kmlPathSave1=paste0(labelInput,"\\Predict\\#2021_PrybSevernoe_trainval_20221228_filter#",date1,".kml")
							 ObserverPointP=list.files(paste0(labelInput,"\\Observer_count"), full.names=T, pattern="kml|shp")[1]
							 Rookery_pol=list.files(paste0(labelInput,"\\Polygons\\Rookery"), full.names=T, pattern="kml|shp")[1]

####################################################################################################
PointsHoulout2=NULL
RookeryPoints=NULL
PupPoints3=NULL
PointsToWrite=NULL
###################

 dat3 <- data.frame(lat=dat$lat,   lon=dat$lon,  area=dat$area)
 ########################################################################
Pointsfilter=function (tble,pthPolygon) {
    coords <- data.frame(lat= tble$lon, lon=tble$lat)   
    data   <- data.frame(area= tble$area)   # data
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  CRS(crs))
	Points =spTransform(Points,CRS(crs))								 							 
    polygon_poly=readOGR(pthPolygon)  
    proj4string(polygon_poly) <- CRS(crs)
   
     pts = Points[!is.na(over(Points,as(polygon_poly,"SpatialPolygons"))),]
   return(pts)
   }

#############################################################################################################################
   PupPoints1=NULL
   if (Filter_by_Rookery_pol == TRUE) {
   PupPoints=Pointsfilter(tble=dat3,pthPolygon=Rookery_pol) 
   PupPoints1=data.frame(area=PupPoints$area,lat= PupPoints$lon ,lon=PupPoints$lat)
   } else { PupPoints1=dat3}
  
   for (k  in 1:length(PupPoints1$area)) {
     if (PupPoints1$area[k] <= BlobFemaleLimit)  {PupPoints1$age[k]="P"}
	# if (PupPoints1$area[k] <= 70)  {PupPoints1$age[k]="SmallError"}
     if (PupPoints1$area[k] > BlobFemaleLimit)   {PupPoints1$age[k]="P"} 
	 }

 

######################################################################################################### 	

KMLwrite(Img3=PupPoints1,kmlPathSave= kmlPathSave1)
############

