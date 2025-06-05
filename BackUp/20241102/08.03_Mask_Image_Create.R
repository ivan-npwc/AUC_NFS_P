
  library(dplyr)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(sp)
  library(spatialEco)
  library(raster)
  library(EBImage)

   EmptyImgs = F
   selectedAge="P"
   Species="NFSpup"
   labelInput
   cex=0.81
   
  date1=substr(basename(labelInput),1,15)
  HauloutDir=paste0(labelInput,"\\Predict\\TilesOverlap")
  MaskImgDir=paste0(labelInput,"\\", "Mask_Image"); unlink(MaskImgDir, recursive=T); dir.create(MaskImgDir)
  maskDir=paste0(labelInput,"\\", "Mask_Image", "\\","Mask");  dir.create(maskDir)
  imgDir=paste0(labelInput,"\\Mask_Image\\Image"); dir.create(imgDir)
  TablePoints=read.csv(paste0(labelInput, "\\",date1, "Points.csv"))
  pthImg=paste0(labelInput,"\\",date1, "_CountDist.csv")
  TableImg =read.csv(pthImg)
######################################################################################################## CREATE MASK WITH POINTS
#TablePoints = TablePoints %>% filter(sex== selectedAge)
imgList= TableImg   %>% filter(All512>0) %>% summarise (imgList=unique(imgName )) 
imgList=imgList$imgList


cl <- makePSOCKcluster(detectCores (logical=F)) 
clusterEvalQ(cl, {	
  library(magick)
})
registerDoParallel(cl)

if (EmptyImgs==F){

foreach(i = 1:length(imgList)) %dopar% {	
  # for (i in 1:length(imgList)) {
  img=imgList[i]
  
  points=  data.frame(TablePoints[TablePoints$imgName==img,])
   if (length(points$sex)>0) {
  
  lat=points$lat
  lon=points$lon
  
  xlim<<-unique(c(points$west50,points$east50))
  ylim<<-unique(c(points$south50,points$north50))
 
  #cex=  paste0(points$sex)
  #cex=  gsub("CU_P",  "0.25", cex)
  #cex=  gsub("P",  "0.25", cex)
 # cex=as.numeric(cex)
  
  ###################
  fig <- image_graph(width = 1084, height = 1084, res =720)
  par(mai=c(0,0,0,0),bg=NA,fig=c(0,1,0,1),bty ="n") 
  plot(lat,lon,xlim=xlim,ylim=ylim, col="red",
       pch=16,cex=cex,bg=16,axes=F,frame.plot=F, ann=F, xaxt='n', yaxt='n')	 
  dev.off()
  fig=image_crop(fig,"1024x1024+30+30+30+30")
   #imgN=gsub("png","gif", img)
  pathImgSave=paste0(maskDir, "\\", date1, "_",img)
  pathImgSave=gsub("jpg","png",pathImgSave)
  image_write(fig, path = pathImgSave, format = "png")   
}
}

###############################################################################

mskList=list.files(maskDir)
imgsList=gsub("png","jpg",mskList)
imgsList=gsub(date1,"",imgsList)
imgsList=gsub("_","",imgsList)
imgsList1=paste0(HauloutDir,"\\",imgsList)
imgsListTo=paste0(imgDir,"\\",date1,"_",imgsList)
file.copy(imgsList1,imgsListTo,overwrite = F)


listMsk=list.files(maskDir,full.names=T)
 for (i in 1: length(listMsk)) {
Msk= readImage(listMsk[i])
 Msk=1-Msk
 Msk1=Msk[,,2]
 writeImage(Msk1, listMsk[i])
}
}
#################################################################   IMGS MSK WITHOUT ANIMALS
########################################################################################################
  if (EmptyImgs == T) {
  

  MaskImgDir0=paste0(labelInput,"\\Mask_Image0"); if (dir.exists(MaskImgDir0)== F ) {dir.create(MaskImgDir0)}
  maskDir0=paste0(labelInput,"\\Mask_Image0\\Mask0") ;if (dir.exists(maskDir0)== F ) {dir.create(maskDir0)}
  imgDir0=paste0(labelInput,"\\Mask_Image0\\Image0");if (dir.exists(imgDir0)== F ) {dir.create(imgDir0)}
  
  
 
  
  ImgPres=list.files(imgDir)
  ImgPres1=gsub("jpg","png",ImgPres)
  ImgPres2=gsub(date1,"",ImgPres1)
  ImgPres3=gsub("_","",ImgPres2)
  
   for (i in 1: length(ImgPres3)){
   ImgPres3[i]= gsub(extension(ImgPres3[i]),"",ImgPres3[i])
   }
 
     ListImgsHaulout=list.files(HauloutDir) 
	 ListImgsHaulout=data.frame(ListImgsHaulout=ListImgsHaulout)
	
	 
	    for (i in 1: length(ListImgsHaulout$ListImgsHaulout)){
   ListImgsHaulout$LIH[i] = gsub(extension(ListImgsHaulout$ListImgsHaulout[i]),"",ListImgsHaulout$ListImgsHaulout[i])
   }
    
	
	   for (i in 1: length(TableImg$imgName)){
   TableImg$presence[i]= gsub(extension(TableImg$imgName[i]),"",TableImg$imgName[i])
  
   }
   

  
   Empty =ListImgsHaulout[!(ListImgsHaulout$LIH %in% ImgPres3),]
   Empty1= Empty$ListImgsHaulout[!Empty$LIH  %in% TableImg$presence]
  
  NeedImgs1=paste0(HauloutDir,"\\",Empty1)
  index=sample(1:(length(NeedImgs1)*0.1))
  NeedImgs2=NeedImgs1[index]
  to=paste0(imgDir0,"\\", date1,"_", basename(NeedImgs2))
  file.copy(NeedImgs2,to,overwrite = F)
  
  
  ListEmptyMsk=gsub("jpg","png",basename(NeedImgs2))
  
   fig=EBImage::readImage( "C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\IMAGE_BLANK.png")
   
 # foreach(y = 1:length(ListEmptyMsk)) %dopar% {	
    for (y in 1:length(ListEmptyMsk)){
	   img = ListEmptyMsk[y] 
	   img1=paste0(date1,"_",img)
	   pathImgSave=paste0(maskDir0, "\\",img1)
       pathImgSave=gsub("jpg","png",pathImgSave)
       writeImage(fig,files  = pathImgSave, type = "png",quality = 100)   
    }
	
	
	}
 

 stopCluster(cl)

  
  
  

