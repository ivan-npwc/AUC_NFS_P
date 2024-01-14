
#https://blogs.rstudio.com/ai/posts/2019-08-23-unet/

#################################################################
library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
library(unet)

##################################################################
	listOPPtoCheck=list.files("D:\\PRYB_2021",full.names=T,pattern=".psx", recursive=T)
	for (i in 1: length(listOPPtoCheck)) {
	trainDir0=gsub(basename(listOPPtoCheck[i]),"",listOPPtoCheck[i])
	trainDir=paste0(trainDir0,"Mask_Image")
	#####################################################################
	SWeight=readRDS("Zero")
	epochs =20
	batch_size=4L	
	vision_dimensions=512
	ValTrainSplit=0.9 # use 90% for train and 10 for validate
	NotUseEmptyImgs = T
	limitImgsSize=10000
	Species=basename(trainDir)
    dateTrain=format(Sys.time(),  "%Y%m%d") 
	images_dir=paste0(trainDir,"\\Image")
	masks_dir=paste0(trainDir,"\\Mask")
	shape=c(vision_dimensions,vision_dimensions,3)
##################################################################################
     listImgS <<- list.files(images_dir)
	 ImgsExten=extension(listImgS)[1]
	 listImgSNoExt=substr(listImgS,1,nchar(listImgS)-nchar(ImgsExten))
	                   if (length(listImgS)==0) {stop ("No Images found")}
     ListMskS<<- list.files(masks_dir)
	 MskExten=extension(ListMskS)[1]
	 MskNoExt=substr(ListMskS,1,nchar(ListMskS)-nchar(MskExten))
     deleteListImgs<<- listImgSNoExt[!(listImgSNoExt %in% MskNoExt)] # here is IMGS DELETE without msk 
     deleteListMsks<<- MskNoExt[!(MskNoExt %in% listImgSNoExt)]
     deleteListImgs1=paste0(images_dir,"\\",deleteListImgs,".jpg")
	 deleteListImgs2=paste0(images_dir,"\\",deleteListImgs,".png")
     deleteListMsk2=paste0(masks_dir,"\\",deleteListMsks,".png")
	if (file.exists(deleteListImgs1[1])){ unlink(deleteListImgs1, recursive=T)}
	if (file.exists(deleteListImgs2[1])){ unlink(deleteListImgs2, recursive=T) }
	if (file.exists(deleteListMsk2[1])){unlink(deleteListMsk2)}
	 print(paste0("Found   ",length(ListMskS), "  Masks"))
#################
######################################
if (exists("unet1")==F){source("C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\Modules\\utilites\\TRAIN_TF_2\\Unet512Create.r")} #UnetVGG16Create
#unet1 = unet(input_shape = shape, num_classes = 1, dropout = 0.1, filters = 16, num_layers = 4, output_activation = "sigmoid")}
     
	 set_weights(unet1,SWeight)


 dice_coef <<- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})
 dice_coef_loss <- function(y_true, y_pred) - dice_coef(y_true, y_pred)
###############################################################
  unet1 <<- unet1 %>%
       compile(
           optimizer =  optimizer_adam(lr= 0.0001 , decay = 1e-6 ),
           loss = dice_coef_loss,#"binary_crossentropy",
           metrics = dice_coef #, metric_binary_accuracy
              )
########################################	
	  data1 <- tibble::tibble(
	  img = list.files(images_dir, full.names = TRUE),
	  mask = list.files(masks_dir, full.names = TRUE),
	  imgSize=file.size(list.files(images_dir, full.names = TRUE)))
	  
     if (NotUseEmptyImgs==T){data1=data1[data1$imgSize > limitImgsSize,];print (paste0("Found   ",length(data1$img), "  Imgs with size more than  "))}
	  
	  data <- rsample::initial_split(data1, prop = ValTrainSplit)	
##########################################################################

###################################################################################################################	
	create_dataset <- function(data, train, batch_size = batch_size, vision_dimensions) {  
	  
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
		  mask = tf$image$decode_gif(tf$io$read_file(.x$mask))[1,,,][,,1,drop=FALSE]
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
		))
	  
		dataset2 <- dataset %>% 
		  dataset_shuffle(buffer_size = batch_size*vision_dimensions)	  
	  dataset3 <- dataset2 %>% 
		dataset_batch(batch_size)
	
	dataset3 %>% 
		dataset_map(unname) # Keras needs an unnamed output.
	}
	training_dataset <- create_dataset(training(data), train = TRUE, batch_size=batch_size, vision_dimensions=vision_dimensions)
########################################################################################################################################################################	
	create_val <- function(data, train, batch_size = batch_size, vision_dimensions) {  
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
		  mask = tf$image$decode_gif(tf$io$read_file(.x$mask))[1,,,][,,1,drop=FALSE]
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
		))
		dataset2 <- dataset %>% 
		  dataset_shuffle(buffer_size = batch_size*vision_dimensions)	  
	  dataset3 <- dataset2 %>% 
		dataset_batch(batch_size)
	  dataset3 %>% 
		dataset_map(unname) # Keras needs an unnamed output.
	}
	validation_dataset <- create_val(testing(data), train = FALSE, batch_size=batch_size, vision_dimensions=vision_dimensions)

#####################################################################################################################
# Training -----------------------------------------------------
checkpoint_dir=paste0(trainDir,"\\Checkpoints")

dir.create(checkpoint_dir,showWarnings = F)
unlink(checkpoint_dir, recursive=T)
dir.create(checkpoint_dir,showWarnings = F)

BaseName <<- basename(file.path(checkpoint_dir, "Val_{dice_coef:.2f}_epoch_{epoch:02d}_512.h5"))
filepath <<- paste0(checkpoint_dir,"\\",Species,"_",dateTrain,"_",BaseName)
output_name="train"

callbacks_list <- list(
  callback_tensorboard(paste0(checkpoint_dir,output_name,"_",vision_dimensions[1])),
  callback_early_stopping(monitor = "dice_coef",
                          min_delta = 1e-4,
                          patience = 4,
                          verbose = 1,
                          mode = "max"),
  callback_reduce_lr_on_plateau(monitor = "dice_coef",
                                factor = 0.1,
                                patience = 4,
                                verbose = 1,
                                mode = "max"),
  callback_model_checkpoint(filepath = filepath,
                            monitor = "dice_coef",
							 period = 1)
                           )
#############################################################################
unet1 %>%  keras:::fit.keras.engine.training.Model(
  training_dataset,  
  epochs = epochs, 
  validation_data = validation_dataset,  
 callbacks = callbacks_list)

}
##################################################
	listOPPtoCheck=list.files("D:\\PRYB_2021",full.names=T,pattern=".psx", recursive=T)
	fin=NULL
	for (i in 1: length(listOPPtoCheck)) {
	
	trainDir0=gsub(basename(listOPPtoCheck[i]),"",listOPPtoCheck[i])
	checkpoint_dir=paste0(trainDir0,"Mask_Image\\Checkpoints")
    mdls=list.files(checkpoint_dir)[20]
	val=strsplit(mdls,"_")[[1]][5]
	epoch=strsplit(mdls,"_")[[1]][7]
	trainDir0= trainDir0
     row1=data.frame(val=val,epoch=epoch,trainDir0=trainDir0)
     fin=rbind(row1,fin)
}
#################
write.csv(fin,"CheckDataPrb2021.csv")
fin=read.csv("CheckDataPrb2021.csv")
fin[order(fin$val),]
##############################################################
library(EBImage)
library(parallel)
library(doParallel)
library(foreach)

cl <- makePSOCKcluster(4) 
clusterEvalQ(cl, {
library(EBImage)
})	
registerDoParallel(cl)

problems=fin[fin$val<0.20,]
problems[order(problems$val),]

  for (y in 1:length(problems$val)) {
 labelInput=problems$trainDir0[y]

 Mask_Image= paste0(labelInput,"Mask_Image")
 Images_Path=paste0(Mask_Image,"/", "Image")
 Masks_Path=paste0(Mask_Image,"/", "Mask")
 Check_Path=paste0(Mask_Image,"/", "Check"); unlink(Check_Path, recursive=T)

 dir.create(Check_Path, showWarnings=F)
 MskList=list.files(Masks_Path)





#foreach(i = 1:length(MskList)) %dopar% {
for (i in 1:length(MskList)){
  
 
   pth=MskList[i]
    mskP=paste0(Masks_Path,"/",pth) 
    ImgP= paste0(Images_Path,"/",gsub("png","jpg",pth))
    
	img=readImage(ImgP)
    msk=readImage(mskP)
      y1 = channel(msk, 'asred')
	  a= img+y1
   PathCheckImg=paste0(Check_Path,"/",pth)
   writeImage(a,PathCheckImg)

}
}
#stopCluster(cl)


problems[order(problems$val),]


#############################################################
 
 
 fin=read.csv("CheckDataPrb2021.csv")
 GoodData=fin[fin$val > 0.17,]
 Percent_30= round(length(GoodData$trainDir0)*0.3)
 index=sample(1:length(GoodData$trainDir0))[1:Percent_30]
 TestData=GoodData[index,]
 
 
 to = "C:\\Users\\usato\\SSL_DB\\TRAIN\\2021"
 ImgTo=paste0(to,"\\Image")
 MskTo=paste0(to,"\\Mask")
 
 
 
  for (y in 1: length(GoodData$val)) {
  
  labelInput=GoodData$trainDir0[y]
  Mask_Image= paste0(labelInput,"Mask_Image")
  Images_Path=paste0(Mask_Image,"/", "Image")
  Masks_Path=paste0(Mask_Image,"/", "Mask")
  
  MskFrom=list.files(Masks_Path, full.names=T)
  ImgFrom=list.files(Images_Path, full.names=T)
  
   file.copy(MskFrom,MskTo)
   file.copy(ImgFrom,ImgTo)
  }
 
 


  







