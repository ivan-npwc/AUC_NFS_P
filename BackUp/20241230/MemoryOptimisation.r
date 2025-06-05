
      listTMP <<-readRDS("C:\\Users\\usato\\SSL_DB\\AUC_NFS_P\\listTMP")
	  OPPListPred1<<-listTMP$OPPListPred1
	  #######################################	   
	while (OPPListPred1 != "") {

      rm(list=ls());.rs.restartR()
	  Sys.sleep(10) 
      setwd("C:\\Users\\usato\\SSL_DB\\AUC_NFS_P")
      listValue <<- readRDS("listUniq")
      listTMP <<-readRDS("listTMP")

      pthOPP<<-listValue$pthOPP
      BatchProcessVector<<-listTMP$BatchProcessVector
	  OPPListPred1<<-listTMP$OPPListPred1
	  pth <<- paste0(pthOPP,"\\log.csv")
###########################

#########################

       k <<- length(OPPListPred1)
	   psx <<- OPPListPred1[1]
	   labelInput <<- gsub(basename(psx),"", psx)
	   print(labelInput)
	   lg = read.csv(pth)
   for (g in 1:length(BatchProcessVector)) {
               action <<- paste0("Modules/", BatchProcessVector[g],".r")
			   source(action)   			 
              Sys.sleep(1) 
     	     print(paste0("Done  ",action,"   ",labelInput ))
			 
               strt <<- data.frame(OPPListPred1=psx, Process=BatchProcessVector[g], dtime=paste0(date()))
               lg = rbind(lg,strt)
              write.csv(lg,pth,row.names = F)                
   }
   OPPListPred1<<-OPPListPred1[!(OPPListPred1 %in% psx)]
	 source("Modules/ListTMPUpdate.r") 


   }
   
  #####################################
#	   fryMem=memuse::Sys.meminfo()
#       fryMem1=as.character(fryMem)
#       fryMem2=data.frame(fryMem1)
#       fryMem2$frym=strsplit(fryMem2$fryMem1,",")[[2]][2]
#       fryMem2$frym1= gsub("size = ","",fryMem2$frym)
#       fryMem2$frym1=round(as.numeric(fryMem2$frym1))
#       FryMemory=fryMem2$frym1[1]
#       if (FryMemory < 2 ){rm(list=ls());.rs.restartR()}
# 
#      setwd("C:\\Users\\usato\\SSL_DB\\AUC_NFS_P")
#      listValue <<- readRDS("listUniq")
#      listTMP <<-readRDS("listTMP")#
#
 #     pthOPP<<-listValue$pthOPP
#      BatchProcessVector<<-listTMP$BatchProcessVector
#	  OPPListPred1<<-listTMP$OPPListPred1
#	  pth <<- paste0(pthOPP,"\\log.csv")
	  #######################################
  