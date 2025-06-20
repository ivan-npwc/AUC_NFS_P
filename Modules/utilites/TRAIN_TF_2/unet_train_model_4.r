#https://blogs.rstudio.com/ai/posts/2019-08-23-unet/
#https://keras3.posit.co/articles/examples/vision/oxford_pets_image_segmentation.html
#https://github.com/r-tensorflow/unet/blob/master/vignettes/carvana.Rmd
#https://github.com/christianversloot/machine-learning-articles/blob/main/how-to-build-a-u-net-for-image-segmentation-with-tensorflow-and-keras.md
#https://www.kaggle.com/code/bigironsphere/loss-function-library-keras-pytorch#Combo-Loss    SEE FOR LOS
#https://runebook.dev/ru/docs/tensorflow/-index-
#################################################################

library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
library(unet)
# Parameters -----------------------------------------------------
	trainDir =   "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\ORIGINAL"
    valDir  =   "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_VAL ORIG 2022"
#	modelDir =  "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\ORIG\\Checkpoints"
	
	#lstMdls=list.files(modelDir, full.names=T)
	#lastmdl=length(lstMdls)
	#model_pth=lstMdls[lastmdl]
	#model_pth=  "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\OPTY\\Checkpoints\\OPTY_20241015_val_0.44_epoch_03_512.h5"
	Weight = "C:\\Users\\usato\\Documents\\YandexDisk\\System data\\weights\\NFSadult_20220104_Val_084_epoch_04_256"
	epochs =50
	batch_size=8L	
	vision_dimensions=256
	
	NotUseEmptyImgs = T
	limitImgsSize=10000
	Species=basename(trainDir)
    dateTrain=format(Sys.time(),  "%Y%m%d") 
	

	
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
	 create_tibl=function(dirs,type){
	 
	 if (type=="train"){ValTrainSplit=0.99}
	 if (type=="val"){ValTrainSplit=0.01}
	
	 images_dir=paste0(dirs,"\\Image")
	 masks_dir=paste0(dirs,"\\Mask")
	  
	  data_tibl <- tibble::tibble(
	  img = list.files(images_dir, full.names = TRUE),
	  mask = list.files(masks_dir, full.names = TRUE),
	  imgSize=file.size(list.files(images_dir, full.names = TRUE)))
	  
	
     if (NotUseEmptyImgs==T){data_tibl=data_tibl[data_tibl$imgSize > limitImgsSize,];print (paste0("Found   ",length(data_tibl$img), "  Imgs with size more than  ", limitImgsSize))}
	  tible <- rsample::initial_split(data_tibl, prop = ValTrainSplit)
	  return(tible)
   }
   data_train=create_tibl(trainDir,type="train")
   data_val=create_tibl(valDir,type="val")
##########################################################################
 source("C:\\Users\\usato\\SSL_DB\\AUC\\Modules\\UnetVGG16Create.r") #UnetVGG16Create
  SWeight=readRDS(Weight)
    set_weights(unet1,SWeight)

 dice_coef <- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  true_positives <- k_sum(y_true_f * y_pred_f)
  (true_positives*2 + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})
##################
 tversky_coef <- custom_metric("tversky_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
 true_positives <- k_sum(y_true_f * y_pred_f)
 true = k_sum(y_true_f)
 pred = k_sum(y_pred_f)
 
 
 true_positives+1/true_positives + k_sum(y_true_f -y_pred_f) + k_sum(y_pred_f-y_true_f)+1
 
 })
################
 dice_coef_loss <- function(y_true, y_pred) - dice_coef(y_true, y_pred)

###############################################################
#unet1 <- load_model_hdf5(model_pth, custom_objects = c(dice_coef = dice_coef,
#                                                    dice_coef_loss=dice_coef_loss))
###############################################################	 
  unet1 <<- unet1 %>%
       compile(
           optimizer = optimizer_adam(lr= 1e-04 , decay = 1e-6 ), #start is lr= 0.0001 , then 0.00001 and finish 0.000001 ?
           loss =     dice_coef_loss,#"binary_crossentropy", 
           metrics = tversky_coef #, metric_binary_accuracy
              )
			  
			  
#unet1 %>% compile(
#        optimizer = optimizer_adam(lr = 1e-5),
#        loss = "binary_crossentropy",
#        metrics = list(dice_coef, metric_binary_accuracy)
#) 
			  
			  
########################################
	random_bsh <- function(img) {                                           
	img %>% 
	tf$image$random_brightness(max_delta = 0.2) %>% 
	tf$image$random_contrast(lower = 0.3, upper = 0.7) %>% 
	tf$image$random_saturation(lower = 0.3, upper = 0.7) %>% 
	#tf$image$random_hue(max_delta = 0.1)  
	#tf$image$random_jpeg_quality(80,100) # %>%
	#tf$image$per_image_standardization()#%>%
	tf$clip_by_value(0, 1) # clip the values into [0,1] range.
	}
# std <- function(img) {img %>% 
                           #  tf$image$per_image_standardization()%>% 
#                            tf$clip_by_value(0, 1)}
#						
#random_rotate <- function(img) {
 #    img %>%
 #    tf$image$stateless_random_flip_left_right (seed=1234)%>%
#	 tf$image$stateless_random_flip_up_down   (seed=4321)
# }
 
############


#left_right  <- function(img) {img %>% tf$image$flip_left_right()}
#up_down <- function(img) {img %>% tf$image$flip_up_down()}
#grayscale <- function(img) {img %>% tf$image$rgb_to_grayscale()%>%  tf$image$grayscale_to_rgb()
 #           }       

###################################################################################################################	
	create_dataset_train <- function(data, batch_size = batch_size, vision_dimensions) {  
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$io$decode_jpeg(tf$io$read_file(.x$img),channels=3),
		  mask = tf$io$decode_png(tf$io$read_file(.x$mask),channels=1)#[1,,,][,,1,drop=FALSE]
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),  
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32) 
		)) 
		##################################
        dataset <- dataset %>% dataset_map(~.x %>% list_modify(
		              img = tf$image$random_flip_left_right(.x$img,seed=123),
		              mask = tf$image$random_flip_left_right(.x$mask,seed=123)
		))
		##################
		dataset <- dataset %>% dataset_map(~.x %>% list_modify(
		              img = tf$image$random_flip_up_down(.x$img,seed=321),
		              mask = tf$image$random_flip_up_down(.x$mask,seed=321)	
		))
		##############
       if (rnorm(1)>0){ 
        dataset <- dataset %>% dataset_map(~.x %>% list_modify(
		              img = tf$image$transpose(.x$img),
		              mask = tf$image$transpose(.x$mask)
		)) }
		###################
		dataset <- dataset %>% dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
		))
############

	dataset <- dataset %>% 
		dataset_batch(batch_size)
	
	dataset %>% 
		dataset_map(unname) # Keras needs an unnamed output.
	}
########################################################################################


	training_dataset <- create_dataset_train(training(data_train), batch_size=batch_size, vision_dimensions=vision_dimensions)
	
	#to see what these transformations do to our images: 
	check=function(){
	

	data_train
	

	training_dataset <- training(data_train) %>%  
    tensor_slices_dataset() %>% 
    dataset_map(~.x %>% list_modify(
    img = tf$io$decode_jpeg(tf$io$read_file(.x$img),  channels=3),
    mask = tf$io$decode_png(tf$io$read_file(.x$mask), channels=1)
  ))
	

	training_dataset <- training_dataset %>% 
  dataset_map(~.x %>% list_modify(
    img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
    mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
  ))
	
	training_dataset <- training_dataset %>% 
  dataset_map(~.x %>% list_modify(
    img = tf$image$resize(.x$img, size = shape(1024, 1024)),
    mask = tf$image$resize(.x$mask, size = shape(1024, 1024))
  ))
	
	###########################################
		random_bsh <- function(img) {                                           
	img %>% 
	tf$image$random_jpeg_quality(min_jpeg_quality=99,max_jpeg_quality=100) # %>%

	}
	
	dataset <- training_dataset %>% dataset_map(~.x %>% list_modify(img = tf$image$random_jpeg_quality(image=.x$img,
	                                                                                         min_jpeg_quality=99,
																							max_jpeg_quality=100))) 
	#########################################
	training_dataset <- training_dataset %>% 
    dataset_map(~.x %>% list_modify(
    img = random_bsh(.x$img)
  ))
	
	############################
	  training_dataset <- training_dataset %>% dataset_map(~.x %>% list_modify(img = left_right(.x$img))) 
	  training_dataset <- training_dataset %>% dataset_map(~.x %>% list_modify(mask = left_right(.x$mask))) 
	####
	 training_dataset <- training_dataset %>% dataset_map(~.x %>% list_modify(img = grayscale(.x$img))) 
	####
	
	
	example <- training_dataset %>% as_iterator() %>% iter_next()
    example$img %>% as.array() %>% as.raster() %>% plot()
	}
########################################################################################################################################################################	
	create_val <- function(data, batch_size = batch_size, vision_dimensions) {  
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$io$decode_jpeg(tf$io$read_file(.x$img),channels =3),
		  mask = tf$io$decode_png(tf$io$read_file(.x$mask),channels =1)#[1,,,][,,1,drop=FALSE]
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
		)) %>%
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
		))
		
		#tf$image$per_image_standardization()
		#dataset <- dataset %>% dataset_map(~.x %>% list_modify(img = std(.x$img)))
		                                                        
		
		
#	#	dataset2 <- dataset %>% 
#	#	  dataset_shuffle(buffer_size = batch_size*vision_dimensions)		  
	  dataset <- dataset %>% 
		dataset_batch(batch_size)
	  dataset %>% 
		dataset_map(unname) # Keras needs an unnamed output.
	}
 validation_dataset <- create_val(testing(data_val), batch_size=batch_size, vision_dimensions=vision_dimensions)

#####################################################################################################################
# Training -----------------------------------------------------
checkpoint_dir=paste0(trainDir,"\\Checkpoints");dir.create(checkpoint_dir,showWarnings = F)
BaseName <<- basename(file.path(checkpoint_dir, "val_{val_dice_coef:.2f}_epoch_{epoch:02d}_512.h5"))
filepath <<- paste0(checkpoint_dir,"\\",Species,"_",dateTrain,"_",BaseName)
output_name="train"

callbacks_list <- list(
  callback_tensorboard(paste0(checkpoint_dir,output_name,"_",vision_dimensions[1])),
#  callback_early_stopping(monitor = "dice_coef",
#                          min_delta = 1e-4,
#                          patience = 4,
#                          verbose = 1,
#                          mode = "max"),
# callback_reduce_lr_on_plateau(monitor = "dice_coef",
#                                factor = 0.1,
#                                patience = 4,
#                                verbose = 1,
#                                mode = "max"),
  callback_model_checkpoint(filepath = filepath,
                            monitor = "dice_coef",
							 period = 1)
                           )
#############################################################################
unet1 %>%  keras:::fit.keras.engine.training.Model(
  training_dataset,  
  epochs = epochs,
  callbacks = callbacks_list, 
  validation_data = validation_dataset)
     

#a=get_weights(unet1)
#saveRDS(a,"1")
##################################################################### CHECK
# predictions=keras:::predict.keras.engine.training.Model(object=unet1,
#                                                   x=training_dataset,
#                                                   batch_size = 2,
#                                                   verbose = 0,
#                                                   steps = 1
#                                                   )
##################################################################
#batch <- training_dataset %>% as_iterator() %>% iter_next()


#images <- tibble(
# image = batch[[1]] %>% array_branch(1),
  #predicted_mask = predictions[,,,1] %>% array_branch(1),
#  mask = batch[[2]][,,,1]  %>% array_branch(1)
#) %>% 
#  sample_n(2) %>% 
#  map_depth(2, function(x) {
#    as.raster(x) %>% magick::image_read()
#  }) %>% 
#  map(~do.call(c, .x))


#out <- magick::image_append(c(
#  magick::image_append(images$mask, stack = TRUE),
#  magick::image_append(images$image, stack = TRUE) 
# # magick::image_append(images$predicted_mask, stack = TRUE)
#  )
#)
#plot(out)

