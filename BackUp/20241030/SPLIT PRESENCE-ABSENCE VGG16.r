library(keras)
library(magick)
library(tidyr)
brand_name  <<-read.csv(Brands_name)
tableP<<-paste0(labelOutput,"\\",basename(labelInput),  "_Brand_read.csv")
pth<<-paste0(labelOutput,"\\", basename(labelInput),"_Crop_256")
dir512Crop=paste0(labelOutput,"\\", basename(labelInput))
save_pth_less98Con=paste0(labelOutput,"\\For processing_resight\\",basename(labelInput))
dir.create(paste0(labelOutput,"\\For processing_resight"))
dir.create(save_pth_less98Con)


if(exists("model_brand_read")==F) {
model_brand_read=load_model_hdf5(Path_model_brand_read)
}
############################################################################
 imageRead <- function(image_file,
                          target_width = 110, 
                          target_height = 110) {
      img <- image_read(image_file)
	  img=image_flop(img)
	  img=image_rotate(img,270)
      img <- image_scale(img, paste0(target_width, "x", target_height, "!"))
      result <- aperm(as.numeric(img[[1]])[, , 1:3], c(2, 1, 3)) # transpose
      dim(result) <- c(1, target_width, target_height, 3)
      return(result)
	}
####################################################################
listImgPred<<-list.files(pth)
if (length(listImgPred)>0) {
finWrite=NULL
for (u in 1: length(listImgPred)) {
  imgPth=paste0(pth, "\\", listImgPred[u])
 

   img_tensor=imageRead(imgPth)
  
  preds = c(model_brand_read %>% predict(img_tensor))
  name= brand_name  
  
  position=c(1:length(name))
  result=data.frame(preds,name)
  result=result[order(-preds),]
  result<<-data.frame(result,position)
  result=result[1,]
  result=cbind(result,imgPth)
  finWrite=rbind(finWrite,result)
} 
write.csv(finWrite,tableP)


brand_table=read.csv(tableP)
Brand_resight_dir=paste0(labelOutput,"\\Resight_brand")
if (dir.exists(Brand_resight_dir)==F) {dir.create(Brand_resight_dir)}

for (f in 1:length(brand_table[,1])) {
  Brand=  paste0(brand_table$x[f])

  if (brand_table$preds[f] < 0.9) {Brand="Else_brand"}
 
  fromPth=paste0(brand_table$imgPth[f])
  toDir=paste0(Brand_resight_dir,"\\",Brand)
  if (dir.exists(toDir)==F) {dir.create(toDir)}
  toPth=paste0(toDir,"\\",basename(fromPth))
  file.copy(fromPth,toPth)
}}
############################################################################### COPY CROP 512 FOR MANUAL RESIGHT IF PREDS LESS .98
brand_table=read.csv(tableP)
for (i in 1:length(brand_table$imgPth)){

Name256=basename(as.character(brand_table$imgPth[i]))
BaseName=strsplit(Name256,"_res_")[[1]][1]
Name256TMP=strsplit(Name256,"_res_")[[1]][2]
Name256TMP1=strsplit(Name256TMP,"_")
Name512=paste0(BaseName,"_res_",Name256TMP1[[1]][1],"_",Name256TMP1[[1]][2],"_",Name256TMP1[[1]][3],extension(Name256))
brand_table$Name512[i]=Name512
}
for (u in 1:length(brand_table$Name512)) {
if (brand_table$preds[u] < 0.9) {
from=paste0(labelOutput,"\\",basename(labelOutput),"\\",  brand_table$Name512[u])
file.copy(from,save_pth_less98Con)
}}





