   library(spatialEco)
   library(dplyr)
   library(raster)
   library(rgdal)
   
   labelInput#="F:\\PRB\\PRYB_2021\\2021_2006_OPP\\20210809_170154\\20210809_170154_01"
   Species="NFSPup"
   date1<<-substr(basename(labelInput),1,15)
   pattern= listValue$NFS_Pup_weight_pth
                    
						#ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
                       # HouloutPoligonDIR=paste0(labelInput,"\\Polygons\\Haulout")
                       # RookeryPolygonDIR=paste0(labelInput,"\\Polygons\\Rookery")
                        ObserverPointDIR=paste0(labelInput,"\\Observer_count")
                        save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species,"_", date1, ".csv")
                        crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
                        PredDir=paste0(labelInput,"\\Predict")

                       PredictPointPTH_kml <<- list.files(PredDir, full.names=T, pattern=pattern)[2]
					   ObserverPointPTH <<- list.files(ObserverPointDIR,full.names=T,pattern="shp|kml")[1]
                      # ModelPoligonPTH <<- list.files(ModelPoligonDIR,full.names=T,pattern=".shp")
                      # HouloutPoligonPTH <<- list.files(HouloutPoligonDIR,full.names=T,pattern=".shp")
                               			
##############################################################################################################		
	
		  ObserverPoint= readOGR(ObserverPointPTH)
          PredictPoint1=readOGR(PredictPointPTH_kml)

		  proj4string(ObserverPoint) <- crs
		  proj4string(PredictPoint1) <- crs
	
		  observer= length(ObserverPoint)
		  pred = length(PredictPoint1)
		  
		  rep1=data.frame(labelInput=labelInput,obs=observer,pred=pred)
		  rep1$DifInd=  rep1$obs -  rep1$pred
		  rep1$DifPerc=   round(rep1$DifInd/rep1$obs*100, digits = 1)
  
       if (exists("rep2")==TRUE){rep2=rbind(rep1,rep2)} else {rep2=rep1}
	   
	   
  
  
  
  
  
  
  
  
  
  
  
  
  
  