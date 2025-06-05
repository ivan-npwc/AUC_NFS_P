

library(keras)
K <- backend()
shape = c(512, 512, 3)
UseVGG16 = T # 
#######################################
library(keras)
K <- backend()


unet_layer <- function(object, filters, kernel_size = c(3, 3),
                       padding = "same", kernel_initializer = "he_normal",
                       dropout, activation="relu"){
  object %>%
    layer_conv_2d(filters = filters, kernel_size = kernel_size, padding = padding) %>%
    layer_batch_normalization() %>%
    layer_activation(activation) %>%
    layer_conv_2d(filters = filters, kernel_size = kernel_size, padding = padding) %>%
    layer_batch_normalization() %>%
    layer_activation(activation)
}

unet1 <- function(shape, nlevels=nlevels, nfilters = nfilters, dropouts = dropouts){
  message("Constructing U-Net with ", nlevels, " levels initial number of filters is: ", nfilters)
  filter_sizes <- nfilters*2^seq.int(0, nlevels)
  clayers <- clayers_pooled <- list()
  clayers_pooled[[1]] <- layer_input(shape = shape)
  
  for(i in 2:(nlevels+1)) {
    clayers[[i]] <- unet_layer(clayers_pooled[[i - 1]], filters = filter_sizes[i - 1], dropout = dropouts[i-1])
    clayers_pooled[[i]] <- layer_max_pooling_2d(clayers[[i]], pool_size = c(2, 2), strides = c(2, 2))
  }
  
  elayers <- list()
  elayers[[nlevels + 1]] <- unet_layer(clayers_pooled[[nlevels + 1]], filters = filter_sizes[nlevels + 1], dropout = dropouts[nlevels + 1])
  
  for(i in nlevels:1) {
    elayers[[i]] <- layer_conv_2d_transpose(elayers[[i+1]],
                                            filters = filter_sizes[i],
                                            kernel_size = c(2, 2),
                                            strides = c(2, 2),
                                            padding = "same")
    elayers[[i]] <- layer_concatenate(list(elayers[[i]], clayers[[i + 1]]), axis = 3)
    elayers[[i]] <- unet_layer(elayers[[i]], filters = filter_sizes[i], dropout = dropouts[i])
  }
  
  outputs <- layer_conv_2d(elayers[[1]], filters = 1, kernel_size = c(1, 1), activation = "sigmoid")
  return(keras_model(inputs = clayers_pooled[[1]], outputs = outputs))
}

unet1 <<- unet1(shape = c(512, 512, 3), nlevels = 4, nfilters = 64, dropouts = c(0.9, 0.9, 0.9, 0.9,0.9)) # for VGG16
#unet1 <<- unet1(shape = c(512, 512, 3), nlevels = 4, nfilters = 8, dropouts = c(0.5, 0.5, 0.5, 0.5,0.5))
if (UseVGG16 ==T){
##############################################################################
conv_base <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = shape)
###############################################################################


if (R.version$nickname == "Dark and Stormy Night") {
  
  VGG16_block1_conv1= conv_base %>% get_layer("block1_conv1")  #1
  VGG16weights_block1_conv1 = get_weights(VGG16_block1_conv1)
  
  UNet_block1_conv1=   unet1 %>% get_layer("conv2d") 
  set_weights(UNet_block1_conv1, VGG16weights_block1_conv1)
  freeze_weights(UNet_block1_conv1)
  #################                                              2
  VGG16_block1_conv2= conv_base %>% get_layer("block1_conv2") 
  VGG16weights_block1_conv2 = get_weights(VGG16_block1_conv2)
  
  UNet_block1_conv2=   unet1 %>% get_layer("conv2d_1") 
  set_weights(UNet_block1_conv2, VGG16weights_block1_conv2)
  freeze_weights(UNet_block1_conv2)
  #################											3
  VGG16_block1_pool= conv_base %>% get_layer("block1_pool") 
  VGG16weights_block1_pool = get_weights(VGG16_block1_pool)
  
  UNet_block1_pool=   unet1 %>% get_layer("max_pooling2d") 
  set_weights(UNet_block1_pool, VGG16weights_block1_pool)
  freeze_weights(UNet_block1_pool)
  #################												4
  VGG16_block2_conv1= conv_base %>% get_layer("block2_conv1") 
  VGG16weights_block2_conv1 = get_weights(VGG16_block2_conv1)
  
  UNet_block2_conv1 =   unet1 %>% get_layer("conv2d_2") 
  set_weights(UNet_block2_conv1, VGG16weights_block2_conv1)
  freeze_weights(UNet_block2_conv1)
  ##################################								5
  VGG16_block2_conv2= conv_base %>% get_layer("block2_conv2") 
  VGG16weights_block2_conv2 = get_weights(VGG16_block2_conv2)
  
  UNet_block2_conv2 =   unet1 %>% get_layer("conv2d_3") 
  set_weights(UNet_block2_conv2, VGG16weights_block2_conv2)
  freeze_weights(UNet_block2_conv2)
  ##################################								6
  VGG16_block2_pool= conv_base %>% get_layer("block2_pool") 
  VGG16weights_block2_pool = get_weights(VGG16_block2_pool)
  
  UNet_block2_pool=   unet1 %>% get_layer("max_pooling2d_1") 
  set_weights(UNet_block2_pool, VGG16weights_block2_pool)
  freeze_weights(UNet_block2_pool)
  #############################################					7
  VGG16_block3_conv1= conv_base %>% get_layer("block3_conv1") 
  VGG16weights_block3_conv1 = get_weights(VGG16_block3_conv1)
  
  UNet_block3_conv1 =   unet1 %>% get_layer("conv2d_4") 
  set_weights(UNet_block3_conv1, VGG16weights_block3_conv1)
  freeze_weights(UNet_block3_conv1)
  ##################################								8
  VGG16_block3_conv2= conv_base %>% get_layer("block3_conv2") 
  VGG16weights_block3_conv2 = get_weights(VGG16_block3_conv2)
  
  UNet_block3_conv2 =   unet1 %>% get_layer("conv2d_5") 
  set_weights(UNet_block3_conv2, VGG16weights_block3_conv2)
  freeze_weights(UNet_block3_conv2)
  ######################											9
  VGG16_block3_pool= conv_base %>% get_layer("block3_pool") 
  VGG16weights_block3_pool = get_weights(VGG16_block3_pool)
  
  UNet_block3_pool =   unet1 %>% get_layer("max_pooling2d_2") 
  set_weights(UNet_block3_pool, VGG16weights_block3_pool)
  freeze_weights(UNet_block3_pool)
  ######################################################3		10		
  VGG16_block4_conv1 = conv_base %>% get_layer("block4_conv1") 
  VGG16weights_block4_conv1  = get_weights(VGG16_block4_conv1)
  
  UNet_block4_conv1  =   unet1 %>% get_layer("conv2d_6") 
  set_weights(UNet_block4_conv1, VGG16weights_block4_conv1)
  freeze_weights(UNet_block4_conv1)
  ######################											11
  VGG16_block4_conv2 = conv_base %>% get_layer("block4_conv2") 
  VGG16weights_block4_conv2  = get_weights(VGG16_block4_conv2)
  
  UNet_block4_conv2  =   unet1 %>% get_layer("conv2d_7") 
  set_weights(UNet_block4_conv2, VGG16weights_block4_conv2)
  freeze_weights(UNet_block4_conv2)
}
##################################################################################################

if (R.version$nickname =="Shake and Throw" ) {
  ######################
  VGG16_block1_conv1= conv_base %>% get_layer("block1_conv1") 
  VGG16weights_block1_conv1 = get_weights(VGG16_block1_conv1)
  
  UNet_block1_conv1=   unet1 %>% get_layer("conv2d_1") 
  set_weights(UNet_block1_conv1, VGG16weights_block1_conv1)
  freeze_weights(UNet_block1_conv1)
  #################
  VGG16_block1_conv2= conv_base %>% get_layer("block1_conv2") 
  VGG16weights_block1_conv2 = get_weights(VGG16_block1_conv2)
  
  UNet_block1_conv2=   unet1 %>% get_layer("conv2d") 
  set_weights(UNet_block1_conv2, VGG16weights_block1_conv2)
  freeze_weights(UNet_block1_conv2)
  #################
  VGG16_block1_pool= conv_base %>% get_layer("block1_pool") 
  VGG16weights_block1_pool = get_weights(VGG16_block1_pool)
  
  UNet_block1_pool=   unet1 %>% get_layer("max_pooling2d") 
  set_weights(UNet_block1_pool, VGG16weights_block1_pool)
  freeze_weights(UNet_block1_pool)
  #################
  VGG16_block2_conv1= conv_base %>% get_layer("block2_conv1") 
  VGG16weights_block2_conv1 = get_weights(VGG16_block2_conv1)
  
  UNet_block2_conv1 =   unet1 %>% get_layer("conv2d_3") 
  set_weights(UNet_block2_conv1, VGG16weights_block2_conv1)
  freeze_weights(UNet_block2_conv1)
  ##################################
  VGG16_block2_conv2= conv_base %>% get_layer("block2_conv2") 
  VGG16weights_block2_conv2 = get_weights(VGG16_block2_conv2)
  
  UNet_block2_conv2 =   unet1 %>% get_layer("conv2d_2") 
  set_weights(UNet_block2_conv2, VGG16weights_block2_conv2)
  freeze_weights(UNet_block2_conv2)
  ##################################
  VGG16_block2_pool= conv_base %>% get_layer("block2_pool") 
  VGG16weights_block2_pool = get_weights(VGG16_block2_pool)
  
  UNet_block2_pool=   unet1 %>% get_layer("max_pooling2d_1") 
  set_weights(UNet_block2_pool, VGG16weights_block2_pool)
  freeze_weights(UNet_block2_pool)
  #############################################
  VGG16_block3_conv1= conv_base %>% get_layer("block3_conv1") 
  VGG16weights_block3_conv1 = get_weights(VGG16_block3_conv1)
  
  UNet_block3_conv1 =   unet1 %>% get_layer("conv2d_5") 
  set_weights(UNet_block3_conv1, VGG16weights_block3_conv1)
  freeze_weights(UNet_block3_conv1)
  ##################################
  VGG16_block3_conv2= conv_base %>% get_layer("block3_conv2") 
  VGG16weights_block3_conv2 = get_weights(VGG16_block3_conv2)
  
  UNet_block3_conv2 =   unet1 %>% get_layer("conv2d_4") 
  set_weights(UNet_block3_conv2, VGG16weights_block3_conv2)
  freeze_weights(UNet_block3_conv2)
  ######################
  VGG16_block3_pool= conv_base %>% get_layer("block3_pool") 
  VGG16weights_block3_pool = get_weights(VGG16_block3_pool)
  
  UNet_block3_pool =   unet1 %>% get_layer("max_pooling2d_2") 
  set_weights(UNet_block3_pool, VGG16weights_block3_pool)
  freeze_weights(UNet_block3_pool)
  ######################################################3
  VGG16_block4_conv1 = conv_base %>% get_layer("block4_conv1") 
  VGG16weights_block4_conv1  = get_weights(VGG16_block4_conv1)
  
  UNet_block4_conv1  =   unet1 %>% get_layer("conv2d_7") 
  set_weights(UNet_block4_conv1, VGG16weights_block4_conv1)
  freeze_weights(UNet_block4_conv1)
  ######################
  VGG16_block4_conv2 = conv_base %>% get_layer("block4_conv2") 
  VGG16weights_block4_conv2  = get_weights(VGG16_block4_conv2)
  
  UNet_block4_conv2  =   unet1 %>% get_layer("conv2d_6") 
  set_weights(UNet_block4_conv2, VGG16weights_block4_conv2)
  freeze_weights(UNet_block4_conv2)
}

if (R.version$nickname =="Someone to Lean On") {
  ####################################################################
  VGG16_block1_conv1= conv_base %>% get_layer("block1_conv1") 
  VGG16weights_block1_conv1 = get_weights(VGG16_block1_conv1)
  
  UNet_block1_conv1=   unet1 %>% get_layer("conv2d_1") 
  set_weights(UNet_block1_conv1, VGG16weights_block1_conv1)
  freeze_weights(UNet_block1_conv1)
  #################
  VGG16_block1_conv2= conv_base %>% get_layer("block1_conv2") 
  VGG16weights_block1_conv2 = get_weights(VGG16_block1_conv2)
  
  UNet_block1_conv2=   unet1 %>% get_layer("conv2d_2") 
  set_weights(UNet_block1_conv2, VGG16weights_block1_conv2)
  freeze_weights(UNet_block1_conv2)
  #################
  VGG16_block1_pool= conv_base %>% get_layer("block1_pool") 
  VGG16weights_block1_pool = get_weights(VGG16_block1_pool)
  
  UNet_block1_pool=   unet1 %>% get_layer("max_pooling2d_1") 
  set_weights(UNet_block1_pool, VGG16weights_block1_pool)
  freeze_weights(UNet_block1_pool)
  #################
  VGG16_block2_conv1= conv_base %>% get_layer("block2_conv1") 
  VGG16weights_block2_conv1 = get_weights(VGG16_block2_conv1)
  
  UNet_block2_conv1 =   unet1 %>% get_layer("conv2d_3") 
  set_weights(UNet_block2_conv1, VGG16weights_block2_conv1)
  freeze_weights(UNet_block2_conv1)
  ##################################
  VGG16_block2_conv2= conv_base %>% get_layer("block2_conv2") 
  VGG16weights_block2_conv2 = get_weights(VGG16_block2_conv2)
  
  UNet_block2_conv2 =   unet1 %>% get_layer("conv2d_4") 
  set_weights(UNet_block2_conv2, VGG16weights_block2_conv2)
  freeze_weights(UNet_block2_conv2)
  ##################################
  VGG16_block2_pool= conv_base %>% get_layer("block2_pool") 
  VGG16weights_block2_pool = get_weights(VGG16_block2_pool)
  
  UNet_block2_pool=   unet1 %>% get_layer("max_pooling2d_2") 
  set_weights(UNet_block2_pool, VGG16weights_block2_pool)
  freeze_weights(UNet_block2_pool)
  #############################################
  VGG16_block3_conv1= conv_base %>% get_layer("block3_conv1") 
  VGG16weights_block3_conv1 = get_weights(VGG16_block3_conv1)
  
  UNet_block3_conv1 =   unet1 %>% get_layer("conv2d_5") 
  set_weights(UNet_block3_conv1, VGG16weights_block3_conv1)
  freeze_weights(UNet_block3_conv1)
  ##################################
  VGG16_block3_conv2= conv_base %>% get_layer("block3_conv2") 
  VGG16weights_block3_conv2 = get_weights(VGG16_block3_conv2)
  
  UNet_block3_conv2 =   unet1 %>% get_layer("conv2d_6") 
  set_weights(UNet_block3_conv2, VGG16weights_block3_conv2)
  freeze_weights(UNet_block3_conv2)
  ######################
  VGG16_block3_pool= conv_base %>% get_layer("block3_pool") 
  VGG16weights_block3_pool = get_weights(VGG16_block3_pool)
  
  UNet_block3_pool =   unet1 %>% get_layer("max_pooling2d_3") 
  set_weights(UNet_block3_pool, VGG16weights_block3_pool)
  freeze_weights(UNet_block3_pool)
  ######################################################3
  VGG16_block4_conv1 = conv_base %>% get_layer("block4_conv1") 
  VGG16weights_block4_conv1  = get_weights(VGG16_block4_conv1)
  
  UNet_block4_conv1  =   unet1 %>% get_layer("conv2d_7") 
  set_weights(UNet_block4_conv1, VGG16weights_block4_conv1)
  freeze_weights(UNet_block4_conv1)
  ######################
  VGG16_block4_conv2 = conv_base %>% get_layer("block4_conv2") 
  VGG16weights_block4_conv2  = get_weights(VGG16_block4_conv2)
  
  UNet_block4_conv2  =   unet1 %>% get_layer("conv2d_8") 
  set_weights(UNet_block4_conv2, VGG16weights_block4_conv2)
  freeze_weights(UNet_block4_conv2)
  ######################
}
}




