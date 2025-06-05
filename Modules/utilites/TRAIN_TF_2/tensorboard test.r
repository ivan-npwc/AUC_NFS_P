#https://blogs.rstudio.com/ai/posts/2019-08-23-unet/
#https://www.kaggle.com/code/bigironsphere/loss-function-library-keras-pytorch#Combo-Loss    SEE FOR LOS
#################################################################

library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
library(unet)
# Parameters -----------------------------------------------------
	trainDir =   "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\tensorBoard"#"C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\OPTY"
    valDir  =  "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\tensorBoard" #"C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_VAL ORIG 2022"
	

	epochs =50
	batch_size=4L	
	vision_dimensions=256
	

	
	Species=basename(trainDir)
    dateTrain=format(Sys.time(),  "%Y%m%d") 
	
tensorboard("logs/run_a")
	
	shape=c(vision_dimensions,vision_dimensions,3)
##################################################################################
 CheckImgMskDir=function (dirs) {   
    images_dir=paste0(dirs,"\\Image")
	masks_dir=paste0(dirs,"\\Mask")
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
    if (file.exists(deleteListImgs1[1])){unlink(deleteListImgs1, recursive=T)}
	if (file.exists(deleteListImgs2[1])){unlink(deleteListImgs2, recursive=T) }
	if (file.exists(deleteListMsk2[1])){unlink(deleteListMsk2)}
	 print(paste0("Found   ",length(list.files(masks_dir)), "  Masks"))
	 print(paste0("Found   ",length(list.files(images_dir)), "  Image"))
	 if (length(list.files(masks_dir)) != length(list.files(images_dir))) stop()
	 }
 	 CheckImgMskDir(trainDir)
     CheckImgMskDir(valDir)
#################

	 images_dir=paste0(trainDir,"\\Image")
	 masks_dir=paste0(trainDir,"\\Mask")
	  
	  data1 <- tibble::tibble(
	                              img = list.files(images_dir, full.names = TRUE),
	                              mask = list.files(masks_dir, full.names = TRUE))
 
	  data1 <- rsample::initial_split(data1, prop = 0.7)
	
  # data_val=create_tibl(valDir,type="val")
##########################################################################
source("C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\Modules\\04.2_UnetVggCreate_New.r") #UnetVGG16Create
 # SWeight=readRDS(Weight)
  #  set_weights(unet1,SWeight)

 dice_coef <- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  true_positives <- k_sum(y_true_f * y_pred_f)
  (true_positives*2 + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})

 dice_coef_loss <- function(y_true, y_pred) - dice_coef(y_true, y_pred)

###############################################################	 
  unet1 <<- unet1 %>%
       compile(
           optimizer = optimizer_adam(lr= 1e-04 , decay = 1e-6 ), #start is lr= 0.0001 , then 0.00001 and finish 0.000001 ?
           loss =     dice_coef_loss,#"binary_crossentropy", 
           metrics = dice_coef #, metric_binary_accuracy
              )
########################################
   

###################################################################################################################	
	create_dataset <- function(data, batch_size = batch_size, vision_dimensions) {    #data=data_train
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
		  mask = tf$image$decode_jpeg(tf$io$read_file(.x$mask))#[1,,,][,,1,drop=FALSE]
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
		))
############

		
	#	dataset <- dataset %>%  dataset_shuffle(buffer_size = batch_size*vision_dimensions)
	  
	 dataset3 <- dataset %>%  dataset_batch(batch_size)
	
	dataset3 %>%  dataset_map(unname) # Keras needs an unnamed output.


	}
	training_dataset <- create_dataset(training(data1), batch_size=batch_size, vision_dimensions=vision_dimensions)
	
	#to see what these transformations do to our images: 

########################################################################################################################################################################	
	create_val <- function(data, batch_size = batch_size, vision_dimensions) {  
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
		  mask = tf$image$decode_jpeg(tf$io$read_file(.x$mask))#[1,,,][,,1,drop=FALSE]
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
		))
		
	#	dataset2 <- dataset  %>%  dataset_shuffle(buffer_size = batch_size*vision_dimensions)	
		  
	  dataset3 <- dataset %>%  dataset_batch(batch_size)
	  
	  dataset3  %>%  dataset_map(unname) # Keras needs an unnamed output.
	}
 validation_dataset <- create_val(testing(data1), batch_size=batch_size, vision_dimensions=vision_dimensions)

#####################################################################################################################
# Training -----------------------------------------------------



#############################################################################
history1 <- unet1 %>%  keras:::fit.keras.engine.training.Model(
  training_dataset,  
  epochs = epochs,
  validation_data = validation_dataset)
    
	 plot(history1)


ggplot(history1, aes(x=date, y=TottalCount,fill="red")) +
    geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
    scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)+
    theme(axis.text.x= element_text(angle=90,hjust=1))+
    xlab("Date")+
    ylab("Count, individual")+
    ggtitle("TF count")

