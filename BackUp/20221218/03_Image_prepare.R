     
	
	 library(magick)
	 
	 
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
	
    #	if (exists("blank")==F){blank= image_blank(512,512,color = "white")}  #white
		
    cl <- makePSOCKcluster(detectCores (logical=FALSE)-1) 
   # clusterExport(cl, "blank")
    clusterEvalQ(cl, {library(magick)})
    registerDoParallel(cl)

  foreach(i = 1:length(table$imgName)) %dopar% {
  ################
 #for (i in 1:length(table$imgName)){

	  selectRow <- table[i,]
      ImgN=selectRow$imgName 	                                                               
      path<-paste0(KMLDir, "\\",ImgN)            
      CheckExists = paste0(SaveDir, "\\",ImgN)

      if(file.exists(CheckExists)==F) {                                                                   
        img=image_read(path)  
        ################################################################################## CENTRAL WITH left right EARS
       # lpi<-paste0(KMLDir,"\\",selectRow$leftName)
	   lpi<-paste0(KMLDir,"\\",selectRow$rightName)
        if (file.exists(lpi)==T) {left.img= image_read(lpi)} else {left.img= image_blank(512,512,color = "white")}

       rimg<-paste0(KMLDir,"\\",selectRow$leftName)
      #  rimg<-paste0(KMLDir,"\\",selectRow$rightName)
        if (file.exists(rimg)==T){right.img=image_read(rimg)} else {right.img=  image_blank(512,512,color = "white")}
        ########
        uimg<-paste0(KMLDir,"\\",selectRow$upName)                                                           # prepare up img for next up line level
        if (file.exists(uimg)==T) {up.img=image_read(uimg)} else{up.img= image_blank(512,512,color = "white")}  
        ##########
        dimg=paste0(KMLDir,"\\",selectRow$downName)                                                          # prepare down img for next down line level
        if (file.exists(dimg)==T) {down.img=image_read(dimg)} else {down.img=  image_blank(512,512,color = "white")}
        
        leftCrop=image_crop(left.img, "256x512+256")
        leftJoin=image_append(c(leftCrop,img))
        ###
        RightCrop=image_crop(right.img, "256x512") 
        leftRightJoin=image_append(c(leftJoin,RightCrop))
        ############################################################## UP LEVEL
        
        Uplevel= table[table$imgName==paste0(selectRow$upName),] 
        if (length(Uplevel$imgName)==0) {
          UpLeft= image_blank(512,512,color = "white")
          UpRight=  image_blank(512,512,color = "white")
        } else {
   
       #   ulimg = paste0(KMLDir,"\\",Uplevel$leftName) 
	       ulimg = paste0(KMLDir,"\\",Uplevel$rightName) 
          if (file.exists(ulimg)==T) {UpLeft=image_read(ulimg)} else{UpLeft= image_blank(512,512,color = "white")}  
      #    urimg=paste0(KMLDir,"\\",Uplevel$rightName)
	       urimg=paste0(KMLDir,"\\",Uplevel$leftName)
          if (file.exists(urimg)==T) {UpRight=image_read(urimg)} else{UpRight=  image_blank(512,512,color = "white")}
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
          DownLeft=  image_blank(512,512,color = "white")
          DownRight=  image_blank(512,512,color = "white")
        } else {
      #    dlimg = paste0(KMLDir,"\\",Downlevel$leftName)  
	       dlimg = paste0(KMLDir,"\\",Downlevel$rightName) 
          if (file.exists(dlimg)==T) {DownLeft=image_read(dlimg)} else{DownLeft=  image_blank(512,512,color = "white")}
       #   drimg=paste0(KMLDir,"\\",Downlevel$rightName) 
	       drimg=paste0(KMLDir,"\\",Downlevel$leftName)
          if (file.exists(drimg)==T) {DownRight=image_read(drimg)} else {DownRight=  image_blank(512,512,color = "white")}
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
     stopCluster(cl)
