source("Modules/07.1_KMLwrite_function.r")

  library(dplyr)
	
                         
					         NFS_Pup_weight_pth
							 labelInput 
							 date1=substr(basename(labelInput),1,15)
					         Species= "NFSPup"
                             crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
							 pathTablePoints=paste0(labelInput, "\\Predict\\",Species, "_BlobTable_GEO_",date1, ".csv")
					         dat<-read.csv(pathTablePoints)
                             if (Species =="NFSPup") {BlobFemaleLimit=200}
							 kmlPathSave1=paste0(labelInput,"\\Predict\\","#",NFS_Pup_weight_pth,"#",date1,".kml")
							 ObserverPointP=list.files(paste0(labelInput,"\\Observer_count"), full.names=T, pattern="kml|shp")[1]
			
			   if (is.na(ObserverPointP)==T){ ObserverPointP=list.files(paste0(labelInput,"\\Polygons\\All_layers"), full.names=T, pattern="kml|shp")[1]}			 
							 
				fltrDir=paste0(labelInput,"\\Polygons\\Model")
				Rookery_pol=list.files(fltrDir, full.names=T, pattern="kml|shp")[1]
				
				#Rookery_pol=list.files(paste0(labelInput,"\\Polygons\\Model"), full.names=T, pattern="kml|shp")[1]
                #if (is.na(Rookery_pol)==T){ Rookery_pol=list.files(, full.names=T, pattern="kml|shp")[1]}	
               # if (is.na(Rookery_pol)==T){ Rookery_pol=list.files(paste0(labelInput,"\\Polygons\\Predict"), full.names=T, pattern="kml|shp")[1]}	
				
 if (is.na(Rookery_pol)==T){stop("No Predict Polygon Found")}
####################################################################################################
PointsHoulout2=NULL
RookeryPoints=NULL
PupPoints3=NULL
PointsToWrite=NULL
###################
 dat<-dat[is.na(dat$lat) ==F ,] 
 dat3 <- data.frame(lat=dat$lat,   lon=dat$lon,  area=dat$s.area)
 ########################################################################


    coords <- data.frame(lat= dat3$lon, lon=dat3$lat)   
    data   <- data.frame(area= dat3$area)   # data
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  CRS(crs))
	Points =spTransform(Points,CRS(crs))								 							 
    polygon_poly=readOGR(Rookery_pol)  
    proj4string(polygon_poly) <- CRS(crs)
    pts = Points[!is.na(over(Points,as(polygon_poly,"SpatialPolygons"))),]
   

#############################################################################################################################

   PupPoints1=data.frame(area=pts$area,lat= pts$lon ,lon=pts$lat)

 
   for (k  in 1:length(PupPoints1$area)) {
     if (PupPoints1$area[k] <= BlobFemaleLimit)  {PupPoints1$age[k]="Err"}
     if (PupPoints1$area[k] > BlobFemaleLimit)   {PupPoints1$age[k]="P"} 
	 }

 

######################################################################################################### 	
KMLwrite(Img3=PupPoints1,kmlPathSave= kmlPathSave1)
############

#autoC=PupPoints1 %>% group_by(age)%>% summarize(count=n())
#Auto_Count=autoC$count
#if (file.exists(ObserverPointP)) {
#obs=readOGR(ObserverPointP)
#print(paste0("Observer count ", length(obs)))
#print(paste0("Auto count ", Auto_Count))
#PercentFromObserverCount= round(abs(length(obs) -  Auto_Count)/length(obs)*100)
#print(paste0("PercentFromObserverCount:    ", PercentFromObserverCount))
#}