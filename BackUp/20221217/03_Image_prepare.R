     
	 library(spatialEco)
	 
	 
	 
     date1=substr(basename(labelInput),1,15)
     Tpth = paste0(labelInput,"\\", date1, ".csv")
	 KMLDir = paste0(labelInput,"\\",date1)
     table=read.csv(Tpth)
     SaveDir=paste0(labelInput, "\\Predict\\TilesOverlap")
######################
    dir.create(paste0(labelInput, "\\Predict"),showWarnings=F); dir.create(SaveDir,showWarnings=F)

    Need=length(table$imgName)
    Presence=length(list.files(SaveDir))
    if (Need !=Presence) {
    
    cl <- makePSOCKcluster(detectCores (logical=FALSE)-1) 
   # clusterExport(cl, "KMLDir")
    clusterEvalQ(cl, {library(magick)})
    registerDoParallel(cl)
	
  foreach(i = 1:length(table$imgName)) %dopar% {
  ################
   for (i in 1:length(table$imgName)){
if (exists("blank")==F){blank= image_blank(512,512,color = "black")}  #white
	  selectRow <- table[i,]
      ImgN=selectRow$imgName 	                                                               
      path<-paste0(KMLDir, "\\",ImgN)            
      CheckExists = paste0(SaveDir, "\\",ImgN)

      if(file.exists(CheckExists)==F) {                                                                   
        img=image_read(path)  
        ################################################################################## CENTRAL WITH left right EARS
        lpi<-paste0(KMLDir,"\\",selectRow$leftName)
        if (file.exists(lpi)==T) {left.img= image_read(lpi)} else {left.img=blank}
        ###################################################### RIGHT
        rimg1= selectRow$rightName
        rimg<-paste0(KMLDir,"\\",rimg1)
        if (file.exists(rimg)==T){right.img=image_read(rimg)} else { right.img= blank}
        ########
        uimg1= selectRow$upName
        uimg<-paste0(KMLDir,"\\",uimg1)
        if (file.exists(uimg)==T) {up.img=image_read(uimg)} else{up.img=blank}  
        ##########
        dimg1= selectRow$downName
        dimg=paste0(KMLDir,"\\",dimg1)
        if (file.exists(dimg)==T) {down.img=image_read(dimg)} else {down.img= blank}
        
        leftCrop=image_crop(left.img, "256x512+256")
        leftJoin=image_append(c(leftCrop,img))
        ###
        RightCrop=image_crop(right.img, "256x512") 
        leftRightJoin=image_append(c(leftJoin,RightCrop))
        ############################################################## UP LEVEL
        
        Uplevel= table[table$imgName==paste0(selectRow$upName),] 
        if (length(Uplevel$imgName)==0) {
          UpLeft=blank
          UpRight= blank
        } else {
          
         # up.img.left=Uplevel$leftName
         # up.img.right=Uplevel$rightName
          
 
         # ulimg1= up.img.left
          ulimg = paste0(KMLDir,"\\",Uplevel$leftName) 
          if (file.exists(ulimg)==T) {UpLeft=image_read(ulimg)} else{UpLeft=blank}  
   #################################################################################
       #   urimg1= up.img.right
          urimg<<-paste0(KMLDir,"\\",Uplevel$rightName)
          if (file.exists(urimg)==T) {UpRight=image_read(urimg)} else{UpRight= blank}
		  }
        UplevelImg=image_append(c(UpLeft,up.img,UpRight))
        UpCrop=image_crop(UplevelImg, "1536x256+0+256")
        UpCrop1=image_crop(UpCrop, "1280x256+256")
        UpCrop2=image_crop(UpCrop1, "1024x256")
        ###
        leftRightUpJoin=image_append(c(UpCrop2,leftRightJoin), stack = T)
        #############################################################################   DOWN LEVEL
        
        Downlevel= table[table$imgName==paste0(selectRow$downName),] 
        if (length(Downlevel$imgName)==0) {
          DownLeft= blank
          DownRight= blank
        } else {
        #  down.img.left=Downlevel$leftName
        #  down.img.right=Downlevel$rightName
          ############ 
         # dlimg1= down.img.left
          dlimg = paste0(KMLDir,"\\",Downlevel$leftName)  
          if (file.exists(dlimg)==T) {DownLeft=image_read(dlimg)} else{DownLeft= blank}
          ###########
         # drimg1= down.img.right
          drimg=paste0(KMLDir,"\\",Downlevel$rightName) 
          if (file.exists(drimg)==T) {DownRight=image_read(drimg)} else {DownRight= blank}
        }
        DownlevelImg=image_append(c(DownLeft,down.img,DownRight))
        DownCrop=image_crop(DownlevelImg, "1536x256")
        DownCrop1=image_crop(DownCrop, "1280x256+256")
        DownCrop2=image_crop(DownCrop1, "1024x256")
        ############
        leftRightUpDownJoin=image_append(c(leftRightUpJoin,DownCrop2), stack = T)
        image_write(leftRightUpDownJoin,CheckExists,format="jpg")
      }
    }
 
  }
  showNotification("Done") 
     stopCluster(cl)
