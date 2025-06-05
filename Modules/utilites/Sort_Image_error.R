

    labelInput = "C:\\Users\\usato\\SSL_DB\\TEST/20220730_115353_01/"

    ErrDir=paste0(labelInput, "\\Error")
    pointsPthDir=paste0(ErrDir,"\\Points")
	HauloutDir=paste0(labelInput,"\\Predict\\PRESENCE")
    DelExtra=F
    Img_Pred_Contc=T

    labelInput
    date1=substr(basename(labelInput),1,15)

 #  source("Modules/01_create_tile_polygons.r")

    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    table_img_pth=paste0(labelInput,"\\", date1, ".csv")

    if(dir.exists(ErrDir)==F){stop("No Error Points found")}

    pointsPth=list.files(pointsPthDir,full.names=T,recursive=T,pattern=".shp")
    PoligonError=paste0(ErrDir, "\\Polygons")
    SavePTHpol=paste0(PoligonError,"\\Error.kml")

    DirImgError=paste0(ErrDir,"\\Image")
    DirMskError=paste0(ErrDir,"\\Mask")

    unlink(list.files(DirImgError, full.names=T))
    listMsk=list.files(DirMskError, full.names=T, recursive=T, include.dirs=T)
    unlink(listMsk, recursive=T)

   if(dir.exists(DirImgError)==F) {dir.create(DirImgError)}
   if(dir.exists(DirMskError)==F) {dir.create(DirMskError)}

###################################################################################################################

pointsError=shapefile(pointsPth)
proj4string(pointsError) <- crs
pointsError=data.frame(pointsError)
pointsError=data.frame(lat=pointsError$coords.x1,lon=pointsError$coords.x2)

table_img=read.csv(table_img_pth)
table_img$link=paste0(table_img$date , "_", table_img$link)
###################
NSdif50=(table_img$north-table_img$south)/2 
table_img$north50=table_img$north              #-NSdif50  
SNdif50=(table_img$north-table_img$south)/2
table_img$south50=table_img$south              #+SNdif50 
WEdiff50= (table_img$east-table_img$west)/2
table_img$west50=table_img$west                #+WEdiff50 
EWdiff50= (table_img$east-table_img$west)/2 
table_img$east50=table_img$east                 #-EWdiff50 

for (i in 1:length(pointsError[,1])){
  lat=as.numeric(pointsError$lat[i])
  lon=as.numeric(pointsError$lon[i])
  sort=table_img[abs(table_img$north50-lon)==min(abs(table_img$north50-lon)),]
  sort1=sort[abs(sort$east50-lat)==min(abs(sort$east50-lat)),]
  if (i==1) {img_error=sort1} else {
    img_error=rbind(img_error,sort1)
    listError <- c(img_error$imgName)
  }}
  
searchDir=paste0(labelInput,"\\Predict\\PRESENCE")
searchListImg=list.files(searchDir,full.names = T,recursive = T,include.dirs = F)
searchTableImg=data.frame(pth=searchListImg, img=basename(searchListImg))

searchErrorImg<-searchTableImg[searchTableImg$img %in% listError,]

if (length(searchErrorImg[,1])==0) {
listError= gsub("png","jpg",listError)
searchErrorImg <-searchTableImg[searchTableImg$img %in% listError,]}



for (i in 1: length(searchErrorImg$pth)) {
  from=paste0(searchErrorImg$pth[i])
  to = paste0(DirImgError, "\\",searchErrorImg$img[i])
  file.copy(from, to)
}
##################################################################################################
if (DelExtra==T){
 listImgError=list.files(DirImgError)
 Tpth=paste0(labelInput,"\\", date1, ".csv")
 tableNearImg=read.csv(Tpth)
 tableNearImg$link=paste0(tableNearImg$date,"_", tableNearImg$link)
 tableNearImg=tableNearImg[tableNearImg$link %in% listImgError,]
 
 OverImgList1=NULL
  for (i in 1:length(tableNearImg$link)) {
  row1=tableNearImg[i,]
  ListNearImg=c(paste0(row1$date,"_",row1$leftName),paste0(row1$date,"_",row1$upName) ,paste0(row1$date,"_",row1$rightName),paste0(row1$date,"_",row1$downName))
  OverImgList=tableNearImg$link[tableNearImg$link %in% ListNearImg]
  OverImgList1=c(OverImgList,OverImgList1)

 # tableNearImg=tableNearImg[!(tableNearImg$link %in% OverImgList),]
  }
   unlink(paste0(DirImgError,"\\",OverImgList1))
   }
   
######################################################################################################################
if(Img_Pred_Contc==T){ 

     
	    library(EBImage)
		library (magick)
		library(parallel)
		library(doParallel)
		library(foreach)

        labelInput

     
		ImagePath= paste0(labelInput,"\\Error\\Image")
		MaskPath=  paste0(labelInput,"\\Predict\\MaskPredicted") 
		PathCheck= paste0(labelInput,"\\Error\\Image_Mask_pred")

		unlink(PathCheck, recursive=T);dir.create(PathCheck)
		MskList=list.files(MaskPath)
		ImgLst=list.files(ImagePath)
	
		

		cl <- makePSOCKcluster(4) 
		clusterEvalQ(cl, {
		library(EBImage)
		})
			
		registerDoParallel(cl)


		foreach(i = 1:length(ImgLst)) %dopar% {


			pth=ImgLst[i]
			
			ImgP=paste0(ImagePath,"/",pth) 
			MskP= paste0(MaskPath,"/", gsub("jpg","png",pth))
			
		if (file.exists(ImgP)){	
			img=readImage(ImgP)
			msk=readImage(MskP)
			  y1 = channel(msk, 'asred')
			  a= img+y1
			  a1=resize(a,1024,1024)
		   PathCheckImg=paste0(PathCheck,"/",pth)
		   writeImage(a1,PathCheckImg)
		   

		}
		}
        }




