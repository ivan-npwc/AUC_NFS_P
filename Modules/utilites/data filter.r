  
 # modelName="2021_Pryb_trainval_20221226"
  library(spatialEco)
  library(rgdal)
  crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  oppdir="F:\\PRB\\PRYB_2021"

listOPP=list.files(oppdir, full.names=T, recursive=T,pattern="psx")

Count=NULL

for (i in 1:length(listOPP)){

   opp=listOPP[i]
   name_base=basename(opp)
   dirOpp=gsub(name_base,"",opp)
   
   site=strsplit(opp,"/")[[1]][2]
   site1=gsub("_OPP","",site)
   site2=gsub("2022_","",site1)
   
   dat=strsplit(name_base,"_")[[1]][1]
   tim=strsplit(name_base,"_")[[1]][2]
   
   dttime=paste0(dat,"_",tim)
   
    DirPthObsCount=paste0(dirOpp,"Observer_count")
    PthObsCount=list.files(DirPthObsCount, pattern="kml|shp", full.names=T)[1]
	
 # dirPred=paste0(dirOpp,"Predict")
 # PthPredCount=list.files(dirPred, pattern=modelName, full.names=T)[2]
   
   DirModPol=paste0(dirOpp,"Polygons/Model")
   PthModPol=list.files(DirModPol, pattern="kml|shp", full.names=T)[1]
   
  if (is.na(PthModPol)==F){
 
  ModPol = readOGR(PthModPol)
  PointsObserver= readOGR(PthObsCount) 
  #PointsPredist= readOGR(PthPredCount) #
  
  
								 							 
    proj4string(PointsObserver) <- CRS(crs)						 							 
    proj4string(ModPol) <- CRS(crs)
	#proj4string(PointsPredist) <- CRS(crs)
	
   
	ptsObserver= point.in.poly(PointsObserver,ModPol)
	pts1Observer=data.frame(ptsObserver)
	pts2Observer=pts1Observer[is.na(pts1Observer[,3]) == F,]
	
   # PointsPredist1= point.in.poly(PointsPredist,ModPol)
	#PointsPredist2=data.frame(PointsPredist1)
	#PointsPredist3=PointsPredist2[is.na(PointsPredist2[,3]) == F,]
	
	
	
      countObserver= length(pts2Observer[,1])
	# countPredict=  length(PointsPredist3[,1])
	
	pretableObserver=data.frame(site=site2, Dtime=dttime,Type="Observer",Count=countObserver)
	#pretablePredict=data.frame(site=site2, Dtime=dttime,Type="Predict",Count=countPredict)
	#pretable=rbind(pretableObserver,pretablePredict)
	pretable=pretableObserver
    Count=rbind(Count,pretable)
  
  
  }
  }
  
  ##################################################################################################################
  