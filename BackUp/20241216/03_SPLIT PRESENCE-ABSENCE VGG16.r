
library(keras)
library(magick)
library(tidyr)
library(tfdatasets)


	library(tidyverse)
	#library(rsample)
	library(reticulate)
	#library(raster)
	
	
labelInput # ="F:\\PRB\\PRYB_2022\\2022_2001_OPP\\20220730_115353\\20220730_115353_01"
Path_model="C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\System data\\Model\\NFS_PUP_FS_SPLIT_20241029_loss_0.20_epoch_08.h5"
NotUseEmptyImgs=T
file_size = 10000
batch_size=32
vision_dimensions=512
Age_Name= c( "ABSANCE","PRESENCE")
pth <- paste0(labelInput,"\\Predict\\TilesOverlap")
AbsenceDir = paste0(labelInput,"\\Predict\\ABSENCE");dir.create(AbsenceDir, showWarnings=F)
################################################################
if(exists("model_split")==F) {
model_split=load_model_hdf5(Path_model)
}
####################################################################
listImgPred <- list.files(pth, full.names=T)
if (length(listImgPred)==0) {stop("No Imgs Found")}
###############################################################
if (NotUseEmptyImgs==T) {
                         inf=data.frame(listImgPred=listImgPred,file_size=as.numeric(file.size(listImgPred)))
						 listImgPred=inf$listImgPred[inf$file_size > file_size]
						 exl=length(inf$listImgPred[inf$file_size < file_size])
						 print(paste0("Exlude  ", exl, " Images and for predict available  ", length (listImgPred) , "   Images"))
}
############################################################################
data <- tibble::tibble(img = listImgPred)
	create_dataset <- function(data, train, batch_size = batch_size, vision_dimensions) {
	 
	 dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_jpeg(tf$io$read_file(.x$img))
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float64)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		))
	   dataset3 <- dataset %>% dataset_batch(batch_size) %>%
                        	#   dataset_cache() %>%
							   dataset_map(unname) # Keras needs an unnamed output.
							   #%>% dataset_shuffle(buffer_size = #batch_size*vision_dimensions)
	}
	pred_data_set <- create_dataset(data, train = FALSE, batch_size=batch_size, vision_dimensions=vision_dimensions)
    pred = model_split %>% predict(pred_data_set)
	
###############################################################################################
    preds3 <<- data.frame(pred)
	names(preds3) <- Age_Name
	preds3$link=listImgPred
for (i in 1:length(listImgPred)) {
   if(preds3[,1][i]> preds3[,2][i]){preds3$name[i]=Age_Name[1]}
   if(preds3[,1][i]< preds3[,2][i]){preds3$name[i]=Age_Name[2]}
   }
 

absc=preds3$link[preds3$name=="ABSANCE"]
file.copy(absc,AbsenceDir)
unlink(absc)











