   library(keras)
   library(tensorflow)
   library(tfdatasets)


library(reticulate)
horovod <- import("horovod.tensorflow.keras", convert = FALSE)












   shuffle = T
   batch_size=4

  
    train_files <- list.files("train/NFS_Pup/2018-2021_TULENY/Image", full.names = TRUE)
    train_masks <- list.files("train/NFS_Pup/2018-2021_TULENY/Mask", full.names = TRUE)

# Check available GPUs
gpus <- tf$config$experimental$list_physical_devices("GPU")
cat("Available GPUs:", length(gpus), "\n")

# Define strategy for multi-GPU training
if (length(gpus) > 1) {
  strategy <- tf$distribute$MirroredStrategy()
  cat("Training using", strategy$num_replicas_in_sync, "GPUs\n")
} else {
  strategy <- tf$distribute$get_strategy()
  cat("Training using single GPU or CPU\n")
}
#########
 dice_coef <<- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})
 dice_coef_loss <- function(y_true, y_pred) - dice_coef(y_true, y_pred)
##############################################################
# Define U-Net within distribution scope
with(strategy$scope(), {
  # Input layer
  inputs <- layer_input(shape = c(256, 256, 3))
  
  # Encoder
  # Block 1
  conv1 <- inputs %>%
    layer_conv_2d(64, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(64, 3, activation = "relu", padding = "same")
  pool1 <- layer_max_pooling_2d(conv1, pool_size = c(2, 2))
  
  # Block 2
  conv2 <- pool1 %>%
    layer_conv_2d(128, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(128, 3, activation = "relu", padding = "same")
  pool2 <- layer_max_pooling_2d(conv2, pool_size = c(2, 2))
  
  # Block 3
  conv3 <- pool2 %>%
    layer_conv_2d(256, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(256, 3, activation = "relu", padding = "same")
  pool3 <- layer_max_pooling_2d(conv3, pool_size = c(2, 2))
  
  # Block 4
  conv4 <- pool3 %>%
    layer_conv_2d(512, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(512, 3, activation = "relu", padding = "same")
  pool4 <- layer_max_pooling_2d(conv4, pool_size = c(2, 2))
  
  # Bottleneck
  conv5 <- pool4 %>%
    layer_conv_2d(1024, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(1024, 3, activation = "relu", padding = "same")
  
  # Decoder
  # Block 6
  up6 <- layer_conv_2d_transpose(conv5, 512, 2, strides = 2, padding = "same")
  up6 <- layer_concatenate(list(up6, conv4))
  conv6 <- up6 %>%
    layer_conv_2d(512, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(512, 3, activation = "relu", padding = "same")
  
  # Block 7
  up7 <- layer_conv_2d_transpose(conv6, 256, 2, strides = 2, padding = "same")
  up7 <- layer_concatenate(list(up7, conv3))
  conv7 <- up7 %>%
    layer_conv_2d(256, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(256, 3, activation = "relu", padding = "same")
  
  # Block 8
  up8 <- layer_conv_2d_transpose(conv7, 128, 2, strides = 2, padding = "same")
  up8 <- layer_concatenate(list(up8, conv2))
  conv8 <- up8 %>%
    layer_conv_2d(128, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(128, 3, activation = "relu", padding = "same")
  
  # Block 9
  up9 <- layer_conv_2d_transpose(conv8, 64, 2, strides = 2, padding = "same")
  up9 <- layer_concatenate(list(up9, conv1))
  conv9 <- up9 %>%
    layer_conv_2d(64, 3, activation = "relu", padding = "same") %>%
    layer_conv_2d(64, 3, activation = "relu", padding = "same")
  
  # Output
  outputs <- layer_conv_2d(conv9, 1, 1, activation = "sigmoid")
  
  # Create model
  model <- keras_model(inputs = inputs, outputs = outputs)
  
  # Compile the model
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 1e-4),
    loss = dice_coef_loss,
    metrics = dice_coef  
  )
 }) 
#############
# Print model summary
#summary(model)
###############################################################################
# Example using tfdatasets for better performance
with(strategy$scope(), {
create_dataset <- function(image_files, mask_files, batch_size = 16, shuffle = F) {
  dataset <- tensor_slices_dataset(list(image_files, mask_files))
  
  if (shuffle) {
    dataset <- dataset %>% 
      dataset_shuffle(buffer_size = length(image_files))
  }
  
  dataset <- dataset %>%
    dataset_map(function(x, y) {
      # Read and decode images
      img <- tf$io$read_file(x)
      img <- tf$image$decode_jpeg(img, channels = 3)
      img <- tf$image$convert_image_dtype(img, tf$float32)
      
      mask <- tf$io$read_file(y)
      mask <- tf$image$decode_png(mask, channels = 1)
      mask <- tf$image$convert_image_dtype(mask, tf$float32)
      
      # Resize if needed
      img <- tf$image$resize(img, c(256L, 256L))
      mask <- tf$image$resize(mask, c(256L, 256L))
      
      return(list(img, mask))
    }) %>%
    dataset_batch(batch_size) %>%
    dataset_prefetch(buffer_size = tf$data$AUTOTUNE)
  
  return(dataset)
}


 train_dataset <- create_dataset(train_files, train_masks, batch_size = batch_size)
 #train_dataset %>% as_iterator() %>% iter_next()
  }) 
 ##############################################

with(strategy$scope(), {
# Train the model
history <- model %>% fit(
  x = train_dataset,
   epochs = 10)
  
 }) 
  
  
  
  
  
  ,  # Use your dataset here
  #validation_data = train_dataset,
  epochs = 100
 # callbacks = callbacks,
 # verbose = 1
)

# Save final model
save_model_tf(model, "unet_final")
 
 
  ####################################################################################
# callbacks <- list(
#  callback_model_checkpoint(
#    filepath = "unet_best.h5",
#    monitor = "val_iou",  # Monitoring IoU now
#    mode = "max",
#    save_best_only = TRUE
#  ),
#  callback_early_stopping(
#    monitor = "val_iou",
#    patience = 15,
#    mode = "max",
#    restore_best_weights = TRUE
#  ),
#  callback_reduce_lr_on_plateau(
#    monitor = "val_iou",
#    factor = 0.2,
#    patience = 5,
#    mode = "max"
#  )
#)
 
 
 
 
 
 
 