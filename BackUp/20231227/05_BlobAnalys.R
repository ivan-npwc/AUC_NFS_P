     
	 library(EBImage)
	 library(magick)
     Species="NFSPup"
	 labelInput
	 Blob_analysis_Check = F

	 
     predsDir=paste0(labelInput,"\\Predict\\Preds")
     listPreds=list.files(predsDir,full.names=T,pattern=Species)  
     date1=substr(basename(labelInput),1,15)
     resultBlob=NULL
	  

  for (f in 1:length(listPreds)) {
  
   cl <- makePSOCKcluster(detectCores (logical=F)-1) 
    clusterEvalQ(cl, {library(EBImage); library(magick)})
    registerDoParallel(cl)
  
  
       Species=strsplit(basename(listPreds[f]),split = "_")[[1]][2]
       pth_resultBlob <<- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv") 
       pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\",Species, "_BlobTable_", date1, "_tmp.csv")
       PredsRDS=readRDS(listPreds[f])
       listImageBl=PredsRDS$listImageBl
       preds=PredsRDS$preds
       dimModel=PredsRDS$dimModel
      dimPreds=PredsRDS$DimPreds
      dim(preds)=c(dimPreds)
	  lngth= dim(preds)[1]
#########################################
resultBlob_tmp=NULL
resultBlob_tmp <- foreach(i = 1:lngth,.combine=rbind) %dopar% {
#   for (i in 1: length (listImageBl)) {

     name=basename(listImageBl[i])
     img_pth=listImageBl[i]
     mask0=preds[i, , , ]
	 
     img0 <- t(mask0)
     dim(img0) <- c(512, 512, 1)
     img = getFrame(img0, 1)
       nmask = thresh(img, 18, 18, 0.009)  
       nmask1 <- fillHull(nmask)
       nmask2 = opening(nmask1, makeBrush(7,shape='disc') ) # shape='Gaussian', sigma=50 
       nmask3 = fillHull(nmask2)
       nmask4 = bwlabel(nmask3)
         if (max(nmask4)!=0) {   
            fts = computeFeatures.moment(nmask4)  # coordinat
            shapeFeatures <- computeFeatures.shape(nmask4) # get radiuus, perimetr, area for a future IT IS MATERIAL FOR XGBOOST
            BlobTable=data.frame(fts,shapeFeatures,img=name,img_pth=img_pth) 
           # resultBlob_tmp=rbind(resultBlob_tmp,BlobTable)			



}
} 
######################
################################################################   BLOB CHECK
if (Blob_analysis_Check==T){
saveDir=paste0(labelInput,"\\Predict\\BlobCheck"); unlink(saveDir,recursive=T);dir.create(saveDir)	
    foreach(i = 1:length(listImageBl)) %dopar% {
  #  for (i in 1: length (listImageBl)) {

     name=basename(listImageBl[i])
     img_pth=listImageBl[i]
     mask0=preds[i, , , ]
	 mask0 <- t(mask0)
		   
		   nmask5=as.Image(resize(mask0,1024,1024))
		   colorMode(nmask5)="Color"
		      image=image_read(img_pth)
              mask=image_read(nmask5)
		      Check=image_composite(image,mask,operator = "blend", compose_args="40")
              PathCheckImg=paste0(saveDir,"\\",name)
              image_write(Check,PathCheckImg,format="jpg")	
		}	
			
}
############################################################
if (is.null(resultBlob_tmp)==F){resultBlob=rbind(resultBlob,resultBlob_tmp)}
             
     stopCluster(cl)         
 
}
            resultBlob$DimModel="512"
		   write.csv(resultBlob,pth_resultBlob,row.names = F)
		  
      
