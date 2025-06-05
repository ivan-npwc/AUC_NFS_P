#https://stackoverflow.com/questions/73325202/create-circular-areas-around-a-coordinate

        #tcltk::tk_choose.dir()
    	#tcltk::tk_choose.files()
	
	#source("/home/ivan/GIT_HUB/AUC_NFS_P_lin/AUCpLIN/Modules/FP_FN.r")

     library(sf)
    library(tidyverse)
	library(dplyr)
    #library(rgdal)

     Species <- "NFSPup"
     PatternPreds="#TRAIN_20241228_val_0.46_epoch_03_256_CH#"
    labelInput ="F:\\PRB\\PRYB_2022/2022_2001_OPP/20220730_115353/20220730_115353_01/"
  
    crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    dst=0.3  # 20 sm dist around poits
    date1 <- substr(basename(labelInput),1,15)
    pattern="#Train_1_2_3_20240529_val_0.61_epoch_01_512#"
    predDir <- paste0(labelInput,"/Predict/")
    savePth <-paste0(labelInput,"/Predict/",Species, "_",basename(labelInput),"_FP-FN.kml")
  
     FP_pth <- paste0(labelInput,"/Predict/",Species, "_",basename(labelInput),"_FP.kml")
	 FN_pth <- paste0(labelInput,"/Predict/",Species, "_",basename(labelInput),"_FN.kml")
	 TP_pth<- paste0(labelInput,"/Predict/",Species, "_",basename(labelInput),"_TP.kml")
	 FPd_pth <- paste0(labelInput,"/Predict/",Species, "_",basename(labelInput),"_FPd.kml")
	
	ModelPoligonDir=paste0(labelInput,"/Polygons/Model")
	ModelPoligonPTH=list.files(ModelPoligonDir, full.names=T, ,pattern="shp|kml")[1]
	
	ObsPointDir=   paste0(labelInput,"/Observer_count")
    ObserverPointP=list.files(ObsPointDir, full.names=T, pattern="kml|shp")[1]
      
     if (is.na(ObserverPointP)==T){  ObsPointDir = paste0(labelInput,"/Polygons/All_layers")
	ObserverPointP=list.files(ObsPointDir, full.names=T, pattern="kml|shp")[1]}
	
     predPth = list.files(predDir, full.names=T, pattern=PatternPreds)[2]
#########################################################################################
# Чтение данных
obs <- st_read(ObserverPointP)
prd <- st_read(predPth)
MdlPol <-st_read(ModelPoligonPTH)

    
	 prd = st_transform(prd,crs=crs)
	 obs = st_transform(obs,crs=crs)
	 MdlPol = st_transform(MdlPol,crs=crs)
	
     PRED_CORD <- st_intersection(prd, MdlPol)
	 OBS_CORD <- st_intersection(obs, MdlPol)
	 
	 
	
  # Создание полигона вокруг точек  учёта
    OBSER_POL <- st_buffer(x = OBS_CORD, dist=dst)
    AU_POL <- st_buffer(x = PRED_CORD, dist=dst)
	#################################################################################
	over= st_intersects(PRED_CORD,OBSER_POL)

	TP_points=AU_POL[over %>% lengths > 0,] # 
	FP_points=AU_POL[over %>% lengths == 0,]
	FN_points=OBSER_POL[st_intersects(OBS_CORD, AU_POL) %>% lengths == 0,]
	#FP_dowbl = AU_POL[over %>% lengths > 1,]
	
	
	FP_points_extra=0	
		for (i in 2: 50){
	FP=OBSER_POL[over %>% lengths == i,] 
	FP=length(FP$Name)
	FP1=FP* (i-1)
	FP_points_extra=FP_points_extra+FP1
	}
	
   FP_points$Name=NULL
   FP_points$Description =NULL
   FP_points$FID =NULL
   FP_points$LAYER=NULL
   FP_points$NAME=NULL
   FP_points$NAME="FP"
    ###
	TP_points$Name =NULL
	TP_points$Description=NULL
	TP_points$FID=NULL
	TP_points$LAYER=NULL
	TP_points$NAME =NULL
	TP_points$NAME="TP"
	
   FN_points$Name=NULL
   FN_points$Description=NULL
   FN_points$FID =NULL
   FN_points$LAYER=NULL
   FN_points$NAME=NULL
   FN_points$NAME="FN"
   
  #  FP_dowbl$Name=NULL
  #  FP_dowbl$Description=NULL
  #  FP_dowbl$FID =NULL
  #  FP_dowbl$LAYER=NULL
  #  FP_dowbl$NAME=NULL
  #  FP_dowbl$NAME="FPd"

 #  FNFPTP=rbind(FP_points,TP_points,FN_points)
	##
   #write_sf(FNFPTP,savePth,driver="kml",layer=FNFPTP$NAME)
	
	write_sf(FP_points,FP_pth,driver="kml",layer="FP")
	write_sf(TP_points,TP_pth,driver="kml",layer="TP")
    write_sf(FN_points,FN_pth,driver="kml",layer="FN")
#	write_sf(FP_dowbl,FPd_pth,driver="kml",layer="FP_d")
