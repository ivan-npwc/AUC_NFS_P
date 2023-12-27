#https://towardsdatascience.com/understanding-semantic-segmentation-with-unet-6be4f42d4b47
#https://keras.rstudio.com/articles/examples/unet.html
#################################################################
#this code removes fit_generator
library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
# Parameters -----------------------------------------------------
    TypeTrain ="TrainEmptyModel" #Retrain
	trainDir =  "C:\\Users\\usato\\SSL_DB\\TRAIN\\test train"
	img_dir=paste0(trainDir,"\\Image")
	msk_dir=paste0(trainDir,"\\Mask")
	
	Weight = "C:\\Users\\usato\\Documents\\YandexDisk\\System data\\weights\\NFSpup\\NFSpup_20220830"
	
	epochs =100
	batch_size=16L	
	vision_dimensions=512
	ValTrainSplit=0.9 # use 90% for train and 10 for validate
	
	
	Species=basename(trainDir)
    dateTrain=format(Sys.time(),  "%Y%m%d") 
	images_dir=paste0(trainDir,"\\Image")
	masks_dir=paste0(trainDir,"\\Mask")
	shape=c(vision_dimensions,vision_dimensions,3)
############
     listImgS <<- list.files(images_dir)
	 ImgsExten=extension(listImgS)[1]
	 listImgSNoExt=substr(listImgS,1,nchar(listImgS)-nchar(ImgsExten))
	                   if (length(listImgS)==0) {stop ("No Images found")}
     ListMskS<<- list.files(masks_dir)
	 MskExten=extension(ListMskS)[1]
	 MskNoExt=substr(ListMskS,1,nchar(ListMskS)-nchar(MskExten))
     deleteListImgs<<- listImgSNoExt[!(listImgSNoExt %in% MskNoExt)] # here is IMGS DELETE without msk 
     deleteListMsks<<- MskNoExt[!(MskNoExt %in% listImgSNoExt)]
     deleteListImgs1=paste0(images_dir,"\\",deleteListImgs,ImgsExten)
     deleteListMsk1=paste0(masks_dir,"\\",deleteListMsks,MskExten)
	 unlink(deleteListImgs1)
	 unlink(deleteListMsk1)
#################
######################################
if (exists("unet1")==F){source("C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\Modules\\utilites\\TRAIN_TF_2\\Unet512Create.r")} # Unet512Create

#if (TypeTrain == ""){
  #   SWeight=readRDS(Weight)
#	 set_weights(unet1,SWeight)
#	 
#	 
#unet1 <- unet1 %>%
#    compile(
#        optimizer = optimizer_adam(lr= 2e-7),
#        loss =   dice_coef_loss,    
#        metrics = c(dice_coef) 
#   )
########################################	to check
images <- tibble(
  img = list.files(here::here(img_dir), full.names = TRUE),
  mask = list.files(here::here(msk_dir), full.names = TRUE)
  ) %>% 
  sample_n(2) %>% 
  map(. %>% magick::image_read() %>% magick::image_resize("128x128"))

out <- magick::image_append(c(
  magick::image_append(images$img, stack = TRUE), 
  magick::image_append(images$mask, stack = TRUE)
  )
)
########################################## 
data <- tibble(
  img = list.files(here::here(img_dir), full.names = TRUE),
  mask = list.files(here::here(msk_dir), full.names = TRUE)
)

data <- initial_split(data, prop = ValTrainSplit)
#####################################################################
training_dataset <- training(data) %>%  
  tensor_slices_dataset() %>% 
  dataset_map(~.x %>% list_modify(
    # decode_jpeg yields a 3d tensor of shape (1280, 1918, 3)
    img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
    # decode_gif yields a 4d tensor of shape (1, 1280, 1918, 3),
    # so we remove the unneeded batch dimension and all but one 
    # of the 3 (identical) channels
    mask = tf$image$decode_gif(tf$io$read_file(.x$mask))[1,,,][,,1,drop=FALSE]
  ))
########################################### to check
example <- training_dataset %>% as_iterator() %>% iter_next()
example
#######################################
training_dataset <- training_dataset %>% 
  dataset_map(~.x %>% list_modify(
    img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
    mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
  ))
  ############
  training_dataset <- training_dataset %>% 
  dataset_map(~.x %>% list_modify(
    img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
    mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
  ))
  
  ############################ augumentation
  random_bsh <- function(img) {
  img %>% 
    tf$image$random_brightness(max_delta = 0.3) %>% 
    tf$image$random_contrast(lower = 0.5, upper = 0.7) %>% 
    tf$image$random_saturation(lower = 0.5, upper = 0.7) %>% 
    # make sure we still are between 0 and 1
    tf$clip_by_value(0, 1) 
}

training_dataset <- training_dataset %>% 
  dataset_map(~.x %>% list_modify(
    img = random_bsh(.x$img)
  ))
 ################################# to check augumentation
example <- training_dataset %>% as_iterator() %>% iter_next()
#example$img %>% as.array() %>% as.raster() %>% plot()  now dosnt work
 ############################################ shuffling on training set only
   training_dataset <- training_dataset %>% 
      dataset_shuffle(buffer_size = batch_size*vision_dimensions)
 ################################### 
    training_dataset <- training_dataset %>% dataset_batch(batch_size) # train in batches; batch size might need to be adapted depending on
  # available memory
  #######################################
  unet1 %>%  fit(
  training_dataset,  
  epochs = epochs)
 
  
  
  
  
  
  