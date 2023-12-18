
if (length(OPPListPred1)==0) {OPPListPred1=""}

         lastBackUp=unique(listTMP$backUpDate)
        if (lastBackUp != format(Sys.Date(),"%Y%m%d")) {
  from= "Modules"
  from1=list.files(from,full.names=T)
  to=paste0("BackUp\\",format(Sys.Date(),"%Y%m%d"))
  dir.create(to)
  file.copy(from1,to)
}                                     


ListTMP <- list(
                  BatchProcessVector = BatchProcessVector,
                 OPPListPred1= OPPListPred1,
                 backUpDate= format(Sys.Date() ,"%Y%m%d")

                )
				
				
    saveRDS(ListTMP, "ListTMP")
    listTMP <<- readRDS("ListTMP")


 
            BatchProcessVector<<-unique(listTMP$BatchProcessVector)
            OPPListPred1<<-unique(listTMP$OPPListPred1)
	
			