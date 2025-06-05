          library(keras)
		  library(magick)
							  train_Gen_dir=   "C:\\Users\\usato\\SSL_DB\\TRAIN\\NFS_PUP_FS_SPLIT\\TRAIN"
							  train_dir=train_Gen_dir
							  checkpoint_dir = "C:\\Users\\usato\\SSL_DB\\TRAIN\\NFS_PUP_FS_SPLIT\\Checkpoints"
							  dir.create(checkpoint_dir)
							  NewModelCreate=T
							  BaseModel_pth =  ""
							  Species="NFSpup_FP_remove"
							  bth_size =128
							  trgt_size =128
							  validation_split=0.2
							  dateTrain="20241206"
							  file_size = 10000
################################################################							  
	# lstImgs=list.files(train_dir, full.names=T, recursive=T)
	# inf=data.frame(lstImgs=lstImgs,file_size=as.numeric(file.size(lstImgs)))
	#					 delete=inf$lstImgs[inf$file_size < file_size]
	#					 length(delete)
	#					 unlink(delete)
#################################################################		
    AllImg=length(list.files(train_dir, recursive=T))/2
	train_step = round(AllImg/bth_size*1* (1-validation_split))
    val_step = round(AllImg/bth_size*1* (validation_split))
#####################################	
		   early_stopping <<- callback_early_stopping(patience = 5)   # 5 epoch for check if regress exists
			cp_callback <- callback_model_checkpoint( 
				filepath = paste0(checkpoint_dir, "\\",Species, "_",dateTrain,"_", basename(file.path(checkpoint_dir, "loss_{val_loss:.2f}_epoch_{epoch:02d}.h5"))),
			#	period = 1,
				verbose = 1)  
	############################################
	train_datagen = image_data_generator(
	  rescale = 1/255,
	  horizontal_flip = T, vertical_flip = T,
      brightness_range=c(1,3),
	  fill_mode = "constant",
	  validation_split=validation_split
	)
	val_datagen=image_data_generator(rescale = 1/255,validation_split=validation_split)
	##########################################################
	train_generator <- flow_images_from_directory(
	  train_dir,
	  train_datagen,
	  target_size = c(trgt_size, trgt_size),
	  batch_size = bth_size,
	  class_mode = "categorical",
	 subset="training")
	#############################################################
	validation_generator <- flow_images_from_directory(
	  train_dir,
	  val_datagen,
	  target_size = c(trgt_size, trgt_size),
	  batch_size = bth_size,
	  class_mode = "categorical",
	  subset="validation"
	)
	##############################################################################################
	if (NewModelCreate==T){
		  conv_base <- application_vgg16(
		   weights = "imagenet",
		   include_top = FALSE,
		   input_shape = c(trgt_size, trgt_size, 3))
		   
    modelTrain <<- keras_model_sequential() %>%
	conv_base %>%
    layer_dropout(rate = 0.3) %>% 
    layer_flatten() %>%  
	layer_dense(units = 512, activation = "relu",name = "fc3") %>% #512
    layer_batch_normalization() %>% 
	layer_dense(units = 256, activation = "relu",name = "fc4") %>%   #256
    layer_batch_normalization() %>%    	
	layer_dense(units = train_generator$num_classes, activation = "softmax",name = "predictions")
	 
	freeze_weights(conv_base)
	
	modelTrain %>% compile(
				optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
				loss = "categorical_crossentropy",
				metrics = c("accuracy"))
				modelTrain
	###########################
	 modelTrain %>% fit(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = 5,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	   callbacks = list(cp_callback)) #early_stopping

	#################################
	unfreeze_weights(conv_base, from = "block3_conv1")
	modelTrain
	modelTrain %>% compile(
						optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
						loss = "categorical_crossentropy",
						metrics = c("accuracy"))					
##########################################################								
	 modelTrain %>% fit(
	  train_generator,
	  steps_per_epoch =   train_step,
	  epochs = 20,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	  callbacks = list(early_stopping,cp_callback))
	  }  						 
#############################
if (NewModelCreate==F){
                      
							
							   TrainIndex=1
                               train_step=round(length(AllImg)/bth_size*1* (1-validation_split))
								
modelTrain=load_model_hdf5(BaseModel_pth)
	  conv_base=get_layer(modelTrain,"vgg16")
	  unfreeze_weights(conv_base, from = "block3_conv1")
	 modelTrain
	  modelTrain %>% compile(
						optimizer =    optimizer_adam(lr= 0.0001 , decay = 1e-6 ), #"rmsprop",  #
						loss = "categorical_crossentropy",
						metrics = c("accuracy"))
						
	 modelTrain %>% fit(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = epochs,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	  callbacks = list(early_stopping,cp_callback))

	  }
