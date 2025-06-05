   library(spatialEco)
   library(dplyr)
   library(raster)
   library(rgdal)
   library(ggplot2)
   
   
   pattern="TRAIN2025"
   listOPP
   fin=NULL
   
  for (i in 1:length(listOPP)){

   labelInput=listOPP[i]
   bsname=basename(labelInput)
   labelInput=gsub(bsname,"",labelInput)
   
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
          PredictPoint1=readOGR(PredictPointPTH_kml)
		  proj4string(PredictPoint1) <- crs
		  pred = length(PredictPoint1)
		  } else {pred=NA}
		  
if (is.na(ObserverPointPTH) == F){		  
		  ObserverPoint= readOGR(ObserverPointPTH)
		  proj4string(ObserverPoint) <- crs
		  observer= length(ObserverPoint)
		  } else {observer=NA}
		  
		  
		  rep1=data.frame(model=basename(PredictPointPTH_kml[i]),labelInput=labelInput,obs=observer,pred=pred)
		 
      fin=rbind(rep1,fin)
  }
  
  
  
  
  
  
  
  
      # if (exists("rep2")==TRUE){rep2=rbind(rep1,rep2)} else {rep2=rep1}
	   
	   
 
          rep1$DifInd=  rep1$obs -  rep1$pred
		  rep1$DifPerc=   round(rep1$DifInd/rep1$obs*100, digits = 1)

#rep2[rep2$DifPerc < -10,]

  
  
  
  
  
  
  