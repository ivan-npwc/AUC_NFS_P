library(keras)
library(magick)
library(tidyr)

labelInput  #="F:\\PRB\\PRYB_2022\\2022_2001_OPP\\20220808_174628\\20220808_174628_01"
Path_model="C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\System data\\Model\\NFS_PUP_FS_SPLIT_20241029_loss_0.20_epoch_08.h5"
NotUseEmptyImgs=T
file_size = 10000
pth <- paste0(labelInput,"\\Predict\\TilesOverlap")
AbsenceDir = paste0(labelInput,"\\Predict\\ABSENCE");dir.create(AbsenceDir, showWarnings=F)

if(exists("model_split")==F) {
model_split=load_model_hdf5(Path_model)
}
#######################################
 imageRead <- function(image_file,
                          target_width = 512, 
                          target_height = 512) {
      img <- image_read(image_file)
	  img=image_flop(img)
	  img=image_rotate(img,270)
      img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
      result <- aperm(as.numeric(img[[1]])[, , 1:3], c(2, 1, 3)) # transpose
      dim(result) <- c(1, target_width, target_height, 3)
      return(result)
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

finWrite=NULL
for (u in 1: length(listImgPred)) {
  imgPth = listImgPred[u]
 
  img_tensor=imageRead(imgPth)
  preds = c(model_split %>% predict(img_tensor))
  name= c("ABSENCE","PRESENCE")  
  
  position=c(1:length(name))
  result=data.frame(preds,name)
  result=result[order(-preds),]
  result<<-data.frame(result,position)
  result=result[1,]
  result=cbind(result,imgPth)
  finWrite=rbind(finWrite,result)
} 

absc=finWrite$imgPth[finWrite$name=="ABSENCE"]
file.copy(absc,AbsenceDir)
unlink(absc)











