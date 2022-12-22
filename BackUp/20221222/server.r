function(input, output, session) {
############################################################### 
  re = observeEvent(input$ResetSetting, {  
   pthOPP<<-choose.dir(default = pthOPP)
   listOPP<<-list.files(pthOPP,recursive=T, pattern=".psx", full.names=T)
  
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'ResetSetting',   label=paste0("INPUT:___", pthOPP))
    })
########################################################################################### 
 #observeEvent(input$Up, {
 #  
 #   BatchProcessVector<<-input$Bath_Process
 # 	OPPListPred1<<-input$OPPListPred
#	
#  source("Modules/ListTMPUpdate.r") 
# })
  ############################################################################### 
  observeEvent(input$Start_Batch_process, {
  
    BatchProcessVector<<-input$Bath_Process
  	OPPListPred1<<-input$OPPListPred
   if (OPPListPred1=="All") {OPPListPred1=list.files(pthOPP,recursive=T, pattern=".psx", full.names=T)}
   source("Modules/ListTMPUpdate.r") 
#############  	   
    for (k in 1:length(OPPListPred1)) {
	   psx <<- OPPListPred1[k]
	   labelInput <<- gsub(basename(psx),"", psx)
	#pth_log<<-paste0(labelInput,substr(basename(labelInput),1,15), "-Log.csv")
	#if (file.exists(pth_log)==F) {log1=NULL } else {log1<-read.csv(pth_log)}
        withProgress(message = paste0("Doing  ",labelInput), value = k , {    # for all opp
   for (i in 1:length(BatchProcessVector)) {
               withProgress(message = paste0(BatchProcessVector[i]), value = i, {     # 
               action <<- paste0("Modules/", BatchProcessVector[i],".r")
			   source(action)
     #       ValueAction <<-""
      #        startAction <<- data.frame(moment="Start", action=paste0(action), dtime=paste0(date() ),ValueAction=ValueAction)
       #       log1=rbind(log1,startAction)
        #      write.csv(log1,pth_log, row.names = F)
    #  Sys.sleep(1) 
     
	 #   Sys.sleep(1) 
     	     print(paste0("Done  ",action,"   ",labelInput ))
         #  endAction <<-data.frame(moment="Finish", action=paste0(action), dtime=paste0(date() ),ValueAction=ValueAction)
          # log1=rbind(log1,endAction)
           #write.csv(log1,pth_log, row.names = F)               
   }) 
   }
   OPPListPred1<<-OPPListPred1[!(OPPListPred1 %in% psx)]
   message(paste0("Done  ",labelInput,"----",Sys.time() ),appendLF=TRUE)
   source("Modules/ListTMPUpdate.r")  
   })
   }
   beep()
   })
########################################################################################  
  ################################################################################### 
  re17 = observeEvent( input$NFS_Pup_weight_pth, {
    NFS_Pup_weight_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'NFS_Pup_weight_pth',   label= paste0("NFS_Pup_weight_pth:  ",NFS_Pup_weight_pth))
  }) 
  ######################################################################
  re21 = observeEvent( input$SQLite_path, {
    SQLite_path<<-file.choose()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'SQLite_path',   label= paste0("SQLite_path:  ",SQLite_path))
  }) 
  #################################################################
  re22 = observeEvent( input$KK_Effort, {
    KK_Effort<<-file.choose()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'KK_Effort',   label= paste0("KK_Effort:  ",KK_Effort))
  }) 
  #########################################################################################
       re25 = observeEvent( input$System_data, {
    System_data<<-choose.dir()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'System_data',   label= paste0("System_data:  ",System_data))
  }) 
  ####################################################################################### 
re28 = observeEvent(input$DarkTheme, {
DarkTheme<<-input$DarkTheme
    source("Modules/ListUniqueUpdate.r")
  }) 
############################################################################################  
  }
  
