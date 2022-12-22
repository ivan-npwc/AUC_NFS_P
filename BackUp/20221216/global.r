if (!require("Rcpp")) {install.packages("Rcpp"); library("Rcpp")}
if (!require("DT")) {install.packages("DT"); library("DT")}
if (!require("keras")) {install.packages("keras"); library("keras")}
if (!require("XML")) {install.packages("XML"); library("XML")}
if (!require("magick")) {install.packages("magick"); library("magick")}
if (!require("filesstrings")) {install.packages("filesstrings"); library("filesstrings")}
if (!require("abind")) {install.packages("abind"); library("abind")}
if (!require("reticulate")) {install.packages("reticulate"); library("reticulate")}
if (!require("parallel")) {install.packages("parallel"); library("parallel")}
if (!require("doParallel")) {install.packages("doParallel"); library("doParallel")}
if (!require("foreach")) {install.packages("foreach"); library("foreach")}
if (!require("tensorflow")) {install.packages("tensorflow"); library("tensorflow")}
if (!require("sp")) {install.packages("sp"); library("sp")}
if (!require("rgdal")) {install.packages("rgdal"); library("rgdal")}
if (!require("geosphere")) {install.packages("geosphere"); library("geosphere")}
if (!require("dismo")) {install.packages("dismo"); library("dismo")}
if (!require("rgeos")) {install.packages("rgeos"); library("rgeos")}
if (!require("kohonen")) {install.packages("kohonen"); library("kohonen")}
if (!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if (!require("beepr")) {install.packages("beepr"); library("beepr")}
#if (!require("tcltk")) {install.packages("tcltk"); library("tcltk")}
#if (!require("sf")) {install.packages("sf"); library("sf")}
if (!require("spatialEco")) {install.packages("spatialEco");library("spatialEco")}
#if (!require("encryptr")) {install.packages("encryptr");library("encryptr")}
if (!require("RSQLite")) {install.packages("RSQLite")}
#if (!require("sparklyr")) {install.packages("sparklyr");library(sparklyr);spark_install(version = "2.1.0")}
if (!require("writexl")) {install.packages("writexl")}
if (!require("shinythemes")) {install.packages("shinythemes");; library("shinythemes")}

#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#  BiocManager::install("EBImage")

#########################################################
listValue <<- readRDS("listUniq")
listTMP <<-readRDS("listTMP")
######################
labelInput<<-listValue$labelInput

    nchaBName=nchar(basename(labelInput))+1
    pthOPP<<-substr(labelInput,0, nchar(labelInput)-nchaBName)
    listOPP<<-list.files(pthOPP)
#pthOPP<<- listValue$pthOPP
	

NFS_Pup_weight_pth<<-listValue$NFS_Pup_weight_pth




SQLite_path<<-  listValue$SQLite_path
KK_Effort <<-listValue$KK_Effort
DarkTheme<<-listValue$DarkTheme; if (DarkTheme==T){Theme<<-"slate"} else {Theme<<-"lumen"}



System_data<<-listValue$System_data





if(is.null(NFS_Pup_weight_pth)==T) {NFS_Pup_weight_pth="_"}


if(is.null(SQLite_path)==T) {SQLite_path="_"}
if(is.null(KK_Effort)==T) {KK_Effort="_"}

#################################################################
pth_log<<-paste0(labelInput,"\\",basename(labelInput), "-Log.csv")
if (file.exists(pth_log)==F) {
  log1=NULL } else { log1<<-read.csv(pth_log) 
    }
########################################################################
lastBackUp=unique(listTMP$backUpDate)
if (lastBackUp != format(Sys.Date(),"%Y%m%d")) {
  from= "Modules"
  from1<<-list.files(from,full.names=T)
  to<<-paste0("BackUp\\",format(Sys.Date(),"%Y%m%d"))
 if (dir.exists(to)==F){dir.create(to)}
  file.copy(from1,to)
  file.copy("global.r",to)
  file.copy("server.r",to)
  file.copy("ui.r",to)
  
}
#####################################################