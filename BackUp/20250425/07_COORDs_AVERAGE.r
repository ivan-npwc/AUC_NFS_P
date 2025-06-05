
  library(dplyr)
  library(rgdal)
  library(sf)
                         
					      
							 labelInput 
							 NFS_Pup_weight_pth = "TRAIN2025"
							 date1=substr(basename(labelInput),1,15)
					         Species= "NFSPup"
                             crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
							 kmlPathSave1=paste0(labelInput,"\\Predict\\","#",NFS_Pup_weight_pth,"#",date1,".kml")
                             dst=0.05		# 5 cm buffer size	average				 
######################################################################################################### 	
prd <- st_read(kmlPathSave1)
prd=prd[prd$Description== "P",]

prd = st_transform(prd,crs=crs)
POL <- st_buffer(x = prd, dist=dst)


over = st_intersects(POL)
DowblPoints=over[over %>% lengths > 1]
 if (length(DowblPoints) !=0){
UniqPoints= prd[over %>% lengths == 1,]
fin=NULL
for (i in 1: length(DowblPoints)){
rw=DowblPoints[i][[1]][1]
fin=c(rw,fin)}
index=unique(fin)
MergPoints=prd[index,]
FinalPoints=rbind(UniqPoints,MergPoints)
FinalPoints$Name =NULL
FinalPoints$Description =NULL
FinalPoints$NAME="P"
write_sf(FinalPoints,kmlPathSave1,driver="kml",layer="P")
}