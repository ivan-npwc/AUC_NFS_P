
  library(dplyr)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(sp)
  library(spatialEco)
  library(raster)
  library(EBImage)

   IncludeEmptyImgs=F
   selectedAge="P"
   Species="NFSpup"
   labelInput

  date1=substr(basename(labelInput),1,15)
  HauloutDir=paste0(labelInput,"\\Predict\\TilesOverlap")
  MaskImgDir=paste0(labelInput,"\\", "Mask_Image"); if (dir.exists(MaskImgDir)== F ) {dir.create(MaskImgDir)}
  maskDir=paste0(labelInput,"\\", "Mask_Image", "\\","Mask") ;if (dir.exists(maskDir)== F ) {dir.create(maskDir)}
  imgDir=paste0(labelInput,"\\Mask_Image\\Image");if (dir.exists(imgDir)== F ) {dir.create(imgDir)}
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
  cex=0.25
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
stopCluster(cl)
###############################################################################



mskList=list.files(maskDir)
imgsList=gsub("png","jpg",mskList)
imgsList=gsub(date1,"",imgsList)
imgsList=gsub("_","",imgsList)
imgsList1=paste0(HauloutDir,"\\",imgsList)
imgsListTo=paste0(imgDir,"\\",date1,"_",imgsList)
file.copy(imgsList1,imgsListTo)


listMsk=list.files(maskDir,full.names=T)
 for (i in 1: length(listMsk)) {
Msk= readImage(listMsk[i])
 Msk=1-Msk
 Msk1=Msk[,,2]
 writeImage(Msk1, listMsk[i])
}
#################################################################   IMGS MSK WITHOUT ANIMALS
#  if (IncludeEmptyImgs==T) {
#  
# 
#  ListImgsHaulout=list.files(HauloutDir)
#  Presence=list.files(maskDir)
#  ImgPres=list.files(imgDir)
#  NeedImgs=ListImgsHaulout[!(ListImgsHaulout %in% ImgPres)]
#  NeedImgs1=paste0(HauloutDir,"\\",NeedImgs)
#  file.copy(NeedImgs1,imgDir)
#  
#  ListEmptyMsk=gsub("jpg","png",NeedImgs)
#  
#  foreach(y = 1:length(ListEmptyImgs)) %dopar% {	
#   # for (y in 1:length(ListEmptyImgs)){
#	   img = ListEmptyMsk[y]
#       fig=image_blank(1024,1024,color = "black")
#	   pathImgSave=paste0(maskDir, "\\",img)
#       pathImgSave=gsub("jpg","png",pathImgSave)
#       image_write(fig, path = pathImgSave, format = "png")   
#    }
	
	
#	}
 

 

  
  
  

