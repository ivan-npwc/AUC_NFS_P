
library(rgdal)

listsites=list.dirs(path = "D:\\PRYB_2022", full.names = TRUE, recursive = FALSE)
fin=NULL

for (z in 1:length(listsites)){
sitedir=listsites[z]
site=strsplit(basename(sitedir),"_")[[1]][2]
OPPListPred1=list.files(sitedir, full.names=T, recursive=T, pattern=".psx")

  for (i in 1:length(OPPListPred1)) {
  psx=OPPListPred1[i]
  bsnm=basename(psx)
  date1=gsub(".psx","",bsnm)
  date2=substr(date1,1,15)
  dr=gsub(bsnm,"",psx)
  predDir=paste0(dr,"Predict")
  preds=list.files(predDir, full.names=T, pattern=".kml")
   for (y in 1:length(preds)){
   pth=preds[y]
   ModelName=basename(pth)
   ModelName=strsplit(ModelName,"#")[[1]][2]
   Count=length(readOGR(pth))
   row1=data.frame(site=site,date1 = date2, ModelName=ModelName,Count=Count)
   fin=rbind(fin, row1)

  }
  }
  }
  fin$ModelName[is.na(fin$ModelName)==T]="NoName"
  
 # write.csv(fin,"auto2022.csv", row.names=F)
  ########### obswerver count

  library(rgdal)

listsites=list.dirs(path = "D:\\PRYB_2022", full.names = TRUE, recursive = FALSE)
#fin=NULL

for (z in 1:length(listsites)){
sitedir=listsites[z]
site=strsplit(basename(sitedir),"_")[[1]][2]
OPPListPred1=list.files(sitedir, full.names=T, recursive=T, pattern=".psx")

  for (i in 1:length(OPPListPred1)) {
  psx=OPPListPred1[i]
  bsnm=basename(psx)
  date1=gsub(".psx","",bsnm)
  date2 = substr(date1,1,15)
  dr=gsub(bsnm,"",psx)
 # Dir=paste0(dr,"Observer_count")
  Dir=paste0(dr,"Polygons\\All_layers")
  obsPth=list.files(Dir, full.names=T, pattern=".kml")
  Count=length(readOGR(obsPth))
   row1=data.frame(site=site,date1 = date2, ModelName="Observer",Count=Count)
   fin=rbind(fin, row1)
  
  
  }
  }
  
  write.csv(fin,"count2022.csv", row.names=F)
  
  
  
  
  
    ################################################# get observer count 2021