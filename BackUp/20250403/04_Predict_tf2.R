
library(unet)
labelInput 
          
	

Species="NFSPup"
batch_size = 64
batch_size_global =  batch_size*10
vision_dimensions=256
resultBlob=NULL
file_size = 10000

NotUseEmptyImgs = T
IncludeBlobAnalis = T

Brush=9
thresh=0.009

MaskCreate = F
ImageMaskConcetence = F	
 
 if (MaskCreate == T){IncludeBlobAnalis = T
                      MskDir = paste0(labelInput,"\\Predict\\MaskPredicted") 
					  unlink(MskDir, recursive=T);dir.create(MskDir)}

date1=substr(basename(paste0(labelInput)),1,15)
predict_dir = paste0(labelInput,"\\Predict\\TilesOverlap")
predict_dir1 = paste0(labelInput,"\\Predict\\PRESENCE")

predsDir = paste0(labelInput,"\\Predict\\Preds") 
unlink(predsDir,recursive=T)
dir.create(predsDir, showWarnings=F)
PTHweight = paste0(System_data,"\\weights\\NFSpup\\",listValue$NFS_Pup_weight_pth)
kmlPathSave1=paste0(labelInput,"\\Predict\\","#",basename(PTHweight),"#",date1,".kml")
NFSpresTblpth=paste0(labelInput,"\\Predict\\Presenc-Absance.csv")
NFSpresTbl=read.csv(NFSpresTblpth)

for (i in 1:length(NFSpresTbl$name)){
 if(NFSpresTbl$PRESENCE[i] > 0.3){NFSpresTbl$name[i]="PRESENCE"} else {NFSpresTbl$name[i]="ABSANCE"}
 }


listImage_glob1 = list.files(predict_dir1, full.names = TRUE)
file.copy(listImage_glob1,predict_dir)
listImage_glob = list.files(predict_dir, full.names = TRUE)

if (file.exists(NFSpresTblpth)){
                                listImage_g=basename(NFSpresTbl$link[NFSpresTbl$name=="PRESENCE"])
								listImage_glob=paste0(predict_dir,"\\",listImage_g)

								}


  if (file.exists(kmlPathSave1)==T){
  
#listImgdonePredPTH = paste0(labelInput,"\\Predict\\listImgdonePred.csv"); unlink(listImgdonePredPTH)
pth_resultBlob <<- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv")
###############################################################################################################

 dice_coef <<- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})
 dice_coef_loss <- function(y_true, y_pred) - dice_coef(y_true, y_pred)
 ##########################################################################################
#unet1 <- load_model_hdf5(pth, custom_objects = c(dice_coef = dice_coef,
#                                                       dice_coef_loss=dice_coef_loss))
##############################################################################

 if(exists("unet1")==F) {source("C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\Modules\\utilites\\UnetCreate_addBlock.r")} 
##############################################################################  
weight <<-readRDS(PTHweight)
set_weights(unet1,weight)

#dice_coef <<- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
#  y_true_f <- k_flatten(y_true)
#  y_pred_f <- k_flatten(y_pred)
#  intersection <- k_sum(y_true_f * y_pred_f)
#  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
#})
#dice_coef_loss <- function(y_true, y_pred) - dice_coef(y_true, y_pred)
############################################################### 
#unet1 <<- unet1 %>%
#  compile(
#    optimizer =  optimizer_adam(lr= 0.0001 , decay = 1e-6 ),
#    loss = dice_coef_loss,
#    metrics = dice_coef 
#  )
################################################

if (NotUseEmptyImgs==T) {
                         inf=data.frame(listImage_glob=listImage_glob,file_size=as.numeric(file.size(listImage_glob)))
						 listImage_glob=inf$listImage_glob[inf$file_size > file_size]
						 exl=length(inf$listImage_glob[inf$file_size < file_size])
						 print(paste0("Exlude  ", exl, " Images and for predict available  ", length (listImage_glob) , "   Images"))
}  
global_steps <<- round(length(listImage_glob)/batch_size_global)  #+1 
##############################################################################	
#std <- function(img) {img %>% 
#                             tf$image$per_image_standardization()#%>% 
#                            # tf$clip_by_value(0, 1)
#							}
						
##############################################################
create_dataset <- function(data1, batch_size = batch_size, vision_dimensions) {  
  dataset <- data1 %>% 
    tensor_slices_dataset() %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$decode_jpeg(tf$io$read_file(.x$img))
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32)
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions))
    ))

	
  dataset <- dataset %>% 
    dataset_batch(batch_size)
  dataset %>% 
    dataset_map(unname) # Keras needs an unnamed output.
}
###################################################################################
#################################################################################
for (e in 1:global_steps) {
  batch_ind_global <- c(1:length(listImage_glob))[1:batch_size_global]
  listImage <- listImage_glob[batch_ind_global]
  listImage=listImage[is.na(listImage)==F]
  if (length(listImage_glob) > length(listImage)) {
 # batch_ind_global=batch_ind_global[is.na(batch_ind_global)==F]
    listImage_glob <<- listImage_glob[-batch_ind_global] 
  }
  data1 <<- tibble::tibble(img = listImage)
  #######################################################################################
  pred_dataset <- create_dataset(data1, batch_size=batch_size, vision_dimensions=vision_dimensions)
  preds=keras:::predict.keras.engine.training.Model(object=unet1,
                                                    x=pred_dataset)
 print(paste0("Done  ", e, "  pred from  " ,global_steps)) 													
 ########################################################################################### 
  
  
   if (IncludeBlobAnalis==T){
     if (e==1) {
               cl <- makePSOCKcluster(detectCores (logical=F)-1) 
               clusterEvalQ(cl, {library(EBImage)})
               registerDoParallel(cl)
               }
#########################################
resultBlob_tmp=NULL
resultBlob_tmp <- foreach(i = 1:length(listImage),.combine=rbind) %dopar% {
#   for (i in 1: length (listImageBl)) {

     name=basename(listImage[i])
     img_pth=listImage[i]
    
     mask0=preds[i, , , ]
     img0 <- t(mask0)
	 img0 <- t(img0)
     dim(img0) <- c(vision_dimensions, vision_dimensions, 1)
     img = getFrame(img0, 1)
     nmask = thresh(img, 18, 18, thresh)  
     nmask1 <- fillHull(nmask)
     nmask2 = opening(nmask1, makeBrush(Brush,shape='disc') ) # shape='Gaussian', sigma=50   
     nmask3 = fillHull(nmask2)
     nmask4 = bwlabel(nmask3)
	   
	       if (MaskCreate==T){
	             nmask5 <- t(nmask4)
	             nmask5=resize(nmask5,1024,1024)
				 MaskPth=paste0(MskDir,"\\", gsub("jpg","png",name))
                 writeImage(nmask5,MaskPth)	   	   	   
	   } 
       
         if (max(nmask4)!=0) {   
            fts = computeFeatures.moment(nmask4)  # coordinat
            shapeFeatures <- computeFeatures.shape(nmask4) # get radiuus, perimetr, area for a future IT IS MATERIAL FOR XGBOOST
            BlobTable=data.frame(fts,shapeFeatures,img=name,img_pth=img_pth) 
           # resultBlob_tmp=rbind(resultBlob_tmp,BlobTable)			

} 
}
######################
if (is.null(resultBlob_tmp)==F){resultBlob=rbind(resultBlob,resultBlob_tmp)}
print(paste0("Done  ", e, "  blobs analisis from  " ,global_steps))		   
   } 
 ###########################################################################
   if (IncludeBlobAnalis==F){
         pthSavePreds=paste0(predsDir,"\\Preds_",Species,"_",e)
         Preds1=list(preds=preds,DimPreds=dim(preds),dimModel=vision_dimensions,listImageBl=listImage)
         saveRDS(Preds1,pthSavePreds)
		 print(paste0(e," in  ",global_steps))
		 }
		 
}
 if (IncludeBlobAnalis==T){write.csv(resultBlob,pth_resultBlob,row.names = F);stopCluster(cl)}
}


if (ImageMaskConcetence==T){source("C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\Modules\\utilites\\Image_Mask.r")}