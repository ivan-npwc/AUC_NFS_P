   library(spatialEco)
   library(dplyr)
   library(raster)
   library(rgdal)
   
   library(ggplot2)

   
   labelInput

   pattern="20240321_Val_041_epoch_04"

   date1<<-substr(basename(labelInput),1,15)
                    
						
                        ObserverPointDIR = paste0(labelInput,"Observer_count")
						ObserverPointDIR1 = paste0(labelInput,"Polygons\\All_layers")
                        
                        crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
                        PredDir = paste0(labelInput,"Predict")
      
                       PredictPointPTH_kml <<- list.files(PredDir, full.names=T, pattern = pattern)[2]
				   
					   
					   
					   ObserverPointPTH <<- list.files(ObserverPointDIR,full.names=T,pattern="kml")[1]
	
	if (is.na(ObserverPointPTH) == T){ObserverPointPTH<<- list.files(ObserverPointDIR1,full.names=T,pattern="kml")[1]}	   
					   
                     
                               			
##############################################################################################################		
	
	
if (is.na(PredictPointPTH_kml) == F){
		  ObserverPoint= readOGR(ObserverPointPTH)
          PredictPoint1=readOGR(PredictPointPTH_kml)

		  proj4string(ObserverPoint) <- crs
		  proj4string(PredictPoint1) <- crs
	
		  observer= length(ObserverPoint)
		  pred = length(PredictPoint1)
		  
		  rep1=data.frame(model=basename(PredictPointPTH_kml[i]),labelInput=labelInput,obs=observer,pred=pred)
		  rep1$DifInd=  rep1$obs -  rep1$pred
		  rep1$DifPerc=   round(rep1$DifInd/rep1$obs*100, digits = 1)
  
       if (exists("rep2")==TRUE){rep2=rbind(rep1,rep2)} else {rep2=rep1}
	   
	   }
 


rep2[rep2$DifPerc < -10,]

  
  
  
  
  
  
  