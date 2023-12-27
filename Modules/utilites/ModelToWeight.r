

library(keras)
pth="C:\\Users\\usato\\SSL_DB\\TRAIN\\Checkpoints\\TRAIN_20231219_Val_0.39_epoch_18_256.h5"

 dice_coef <<- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})
 dice_coef_loss <- function(y_true, y_pred) - dice_coef(y_true, y_pred)
 
unet1 <- load_model_hdf5(pth, custom_objects = c(dice_coef = dice_coef,
                                                        dice_coef_loss=dice_coef_loss))


a=get_weights(unet1)
saveRDS(a,"20231219_Val_039_epoch_18")