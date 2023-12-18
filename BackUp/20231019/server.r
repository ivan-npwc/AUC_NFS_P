function(input, output, session) {
############################################################### 
  re = observeEvent(input$ResetSetting, {  
   pthOPP<<-choose.dir(default = pthOPP)
   listOPP<<-list.files(pthOPP,recursive=T, pattern=".psx", full.names=T)
  
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'ResetSetting',   label=paste0("INPUT:___", pthOPP))
	message("The list of OPP has been updated",appendLF=TRUE)
    })
########################################################################################### 
 observeEvent(input$Up, {
   
    BatchProcessVector<<-input$Bath_Process
 	OPPListPred1<<-input$OPPListPred
	if (OPPListPred1=="All") {OPPListPred1 <<- listOPP}
  source("Modules/ListTMPUpdate.r")  })
  ############################################################################### 
  observeEvent(input$Start_Batch_process, {
    BatchProcessVector<<-input$Bath_Process
  	OPPListPred1<<-input$OPPListPred
   if (OPPListPred1=="All") {OPPListPred1 <<- listOPP}
   source("Modules/ListTMPUpdate.r")
    pth <<- paste0(pthOPP,"\\log.csv")
    lg=data.frame(OPPListPred1=OPPListPred1,Process="Start",dtime=paste0(date())); write.csv(lg,pth,row.names = F)   
#############  	   
	while (OPPListPred1 != "") {
       k <<- length(OPPListPred1)
	   psx <<- OPPListPred1[1]
	   
	   labelInput <<- gsub(basename(psx),"", psx)
	     lg = read.csv(pth)
        withProgress(message = paste0("Doing  ",labelInput), value = k , {    # for all opp
   for (g in 1:length(BatchProcessVector)) {
               withProgress(message = paste0(BatchProcessVector[g]), value = g, {     # 
               action <<- paste0("Modules/", BatchProcessVector[g],".r")
			   source(action)       
      Sys.sleep(1) 
     	     print(paste0("Done  ",action,"   ",labelInput ))
			 
               strt <<- data.frame(OPPListPred1=psx, Process=BatchProcessVector[g], dtime=paste0(date()))
               lg = rbind(lg,strt)
              write.csv(lg,pth,row.names = F)                
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
re29 = observeEvent(input$Blob_analysis_Check, {
Blob_analysis_Check<<-input$Blob_analysis_Check
    source("Modules/ListUniqueUpdate.r")
  }) 
################################################################################################
  }
  
