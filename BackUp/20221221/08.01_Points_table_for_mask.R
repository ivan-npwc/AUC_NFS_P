
    library(parallel)
    library(doParallel)
    library(foreach)
    library(sp)
    library(spatialEco)
    library(raster)
    library(rgdal)
   ####################################################
    date1=substr(basename(labelInput),1,15)
    ObserverPointDIR=paste0(labelInput,"\\Observer_count")
	PATHPoints=list.files(ObserverPointDIR,full.names=T,pattern=".shp|kml")
	pthTable=paste0(labelInput,"\\", date1,".csv")
	table0=read.csv(pthTable)
	pthPointsTable=paste0(labelInput,"\\", date1,"Points.csv")
	pthHouloutImg=paste0(labelInput,"\\Predict\\TilesOverlap")
	listImgHoul=list.files(pthHouloutImg)
	crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
	
    if (length(PATHPoints)>1){stop("Only one observer count must be")}
    Points <-readOGR(PATHPoints)   
    proj4string(Points) <- crs
##########################################################
   table0$linlEx=paste0(date1,"_",table0$imgName)
  table1=table0[table0$imgName %in% listImgHoul,]
############################################################
    NSdif50=(table1$north-table1$south)/2 
    table1$north50=table1$north+NSdif50  
    SNdif50=(table1$north-table1$south)/2
    table1$south50=table1$south-SNdif50 
    WEdiff50= (table1$east-table1$west)/2
    table1$west50=table1$west-WEdiff50 
    EWdiff50= (table1$east-table1$west)/2 
    table1$east50=table1$east+EWdiff50 
  ####################################################################################################
  table2= data.frame(imgName=table1$imgName,link=table1$linlEx,west=table1$west, east=table1$east,south=table1$south, north=table1$north,  north50=  table1$north50, south50= table1$south50,west50= table1$west50, east50= table1$east50,date1=date1) 
  ################################################################################################################################################
 # cl <- makePSOCKcluster(detectCores (logical=F)-1) 
 # clusterEvalQ(cl, {
 #   library(sp)
 #   library(spatialEco)
 #   library(raster)					  
 # })
 # registerDoParallel(cl)
  TableAnimalPoints=NULL
 #TableAnimalPoints <- foreach(y = 1:length(table2[,1]), .combine=rbind) %dopar% {	
 options(warn=-1)
    for (y in 1:length(table2[,1])) {
    selectRow=table2[y,]
    Poligon <- as(raster::extent(selectRow$west50,selectRow$east50,selectRow$south50,selectRow$north50), "SpatialPolygons")
    proj4string(Poligon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
 
    pts = Points[!is.na(over(Points,as(Poligon,"SpatialPolygons"))),]
    if (length(pts)> 0) {    
	pts1=data.frame(pts)
     imagePoint1=data.frame(sex=  pts1[,2], lat= pts1$coords.x1, lon= pts1$coords.x2)					
      PointTable2=cbind(imagePoint1,selectRow)
	  TableAnimalPoints=rbind(TableAnimalPoints,PointTable2)
    }
  }
  ##################################
	 options(warn=0)
#  stopCluster(cl)
  write.csv(TableAnimalPoints, pthPointsTable)
  
