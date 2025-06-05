
library(EBImage)

 ImagePath= "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\Image"
 MskDir="C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\Mask_Opti"
 
 Dir_Flip_Flop_Img = "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\Flip_Flop_Image"
 Dir_Flip_Flop_Msk = "C:\\Users\\usato\\SSL_DB\\TRAIN\\PRB_2021\\Flip_Flop_Mask"

dir.create(Dir_Flip_Flop_Img)
dir.create(Dir_Flip_Flop_Msk)

ImgList=list.files(ImagePath, full.names=T)



library(parallel)
library(doParallel)
library(foreach)

cl <- makePSOCKcluster(4) 
clusterEvalQ(cl, {
library(EBImage)
})
	
registerDoParallel(cl)


foreach(i = 1:length(ImgList)) %dopar% {

     pth=ImgList[i]
	
	 Name= basename(pth)
	 MskName=gsub("jpg","png", Name)
	 MskPth=paste0(MskDir, "\\",MskName)
	 
	 img=readImage(pth)
	 msk=readImage(MskPth)

	 FlipName_Img = paste0("Flip_",Name)
	 FlopName_Img = paste0("Flop_",Name)
	 FlipFlopName_Img=paste0("FlipFlop_",Name)
	 
	 FlipName_msk=gsub("jpg","png",FlipName_Img)
	 FlopName_msk=gsub("jpg","png",FlopName_Img)
	 FlipFlopName_msk=gsub("jpg","png",FlipFlopName_Img)
	 
	 img_flop=flop(img)
	 msk_flop=flop(msk)
	 
	 img_flip=flip(img)
	 msk_flip=flip(msk)
	 
	 img_flip_flop=flip(img_flop)
	 msk_flip_flop=flip(msk_flop)
	 
	 
	 FlopName_Img_pth=paste0(Dir_Flip_Flop_Img,"\\",FlopName_Img)
	 FlopName_msk_pth=paste0(Dir_Flip_Flop_Msk,"\\",FlopName_msk)
	 
	 FlipName_Img_pth=paste0(Dir_Flip_Flop_Img,"\\",FlipName_Img)
	 FlipName_msk_pth=paste0(Dir_Flip_Flop_Msk,"\\",FlipName_msk)
	 
	 FlipFlopName_Img_pth=paste0(Dir_Flip_Flop_Img,"\\",FlipFlopName_Img)
	 FlipFlopName_msk_pth=paste0(Dir_Flip_Flop_Msk,"\\",FlipFlopName_msk)
	 
	 
	 
	 writeImage(img_flop,FlopName_Img_pth)
	 writeImage(msk_flop,FlopName_msk_pth)
	 
	 
	 writeImage(img_flip,FlipName_Img_pth)
	 writeImage(msk_flip,FlipName_msk_pth)
	
	 writeImage(img_flip_flop,FlipFlopName_Img_pth)
	 writeImage(msk_flip_flop,FlipFlopName_msk_pth)


}