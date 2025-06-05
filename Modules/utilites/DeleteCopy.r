
ImgDirTo = "F:\\train_256\\PRB_2021\\Image"
MskDirTo = "F:\\train_256\\PRB_2021\\Mask"

oppsdir = "F:\\PRB\\PRYB_2021"
listOpss=list.files(oppsdir, full.names=T, pattern=".psx", recursive=T)

testData= listOpss
#testData= listOpss[basename(listOpss)%in% basename(TestData)]

for (i in 1:length(testData)){

   opp=testData[i]
   name_base=basename(opp)
   dirOpp=gsub(name_base,"",opp)
   
   ImgFromDir=paste0(dirOpp,"/Mask_Image//Image")
   MskFromDir=paste0(dirOpp,"/Mask_Image/Mask")
   
   lstImgs=list.files(ImgFromDir, full.names=T)
   lstMsks=list.files(MskFromDir, full.names=T)
   
   file.copy(lstImgs,ImgDirTo)
   file.copy(lstMsks,MskDirTo)

}


##################################
dirOPP = "F:\\PRB\\PRYB_2021"
To="C:\\Users\\usato\\SSL_DB\\TRAIN\\NFS_PUP_FS_SPLIT\\ABSENCE"
opslist = list.files(dirOPP, full.names=T, recursive=T,pattern=".psx")

for (i in 1:length(opslist)){
 opp=opslist[i]
 
 dir1 = gsub(".psx","",opp)
 bsnm = basename(dir1)

 dir2 = gsub(bsnm,"",dir1)
 
 ImgDirFrom=paste0(dir2,"//", bsnm, "//Mask_Image0//Image0")
 imgs=list.files(ImgDirFrom, full.names=T)
 
 file.copy(imgs,To)
}
##########################################################
##################################
dirOPP = "F:\\PRB\\PRYB_2022"
ToImg= "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_VAL ORIG 2022\\Image"
ToMsk= "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_VAL ORIG 2022\\Mask"

opslist = list.files(dirOPP, full.names=T, recursive=T,pattern=".psx")

for (i in 1:length(opslist)){
 opp=opslist[i]
 
 dir1 = gsub(".psx","",opp)
 bsnm = basename(dir1)
 dir2 = gsub(bsnm,"",dir1)
 
 ImgDirFrom=paste0(dir2,"//", bsnm, "//Mask_Image//Image")
 MskDirFrom=paste0(dir2,"//", bsnm, "//Mask_Image//Mask")
 
 imgs=list.files(ImgDirFrom, full.names=T)
 msks=list.files(MskDirFrom, full.names=T)
 
 file.copy(imgs,ToImg)
 file.copy(msks,ToMsk)
}














