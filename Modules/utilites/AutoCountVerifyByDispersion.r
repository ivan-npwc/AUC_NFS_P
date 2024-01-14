
pth= "C:\\Users\\usato\\Documents\\YandexDisk\\CURRENT WORK\\GR_2024\\REPORT\\AutoCount_2022.csv"
tbl=read.csv(pth)
unique(tbl$model)
#tbl=tbl[tbl$model != "2021_Pryb_trainval_20221226",]


tbl$opp=paste0(tbl$site,"_", tbl$date)
opplist=unique(tbl$opp)
  fin=NULL
  
  for (i in 1:length(opplist)) {
  opp=opplist[i]
  subtbl=tbl[tbl$opp==opp,]
  auto=subtbl[subtbl$model != "Observer",]
  sd1=sd(auto$Count)
  dispersionAuto= max(auto$Count)-min(auto$Count)
  meanAuto=mean(auto$Count)

  MedAuto=median(auto$Count)
  observer=subtbl$Count[subtbl$model== "Observer"]
  row1=data.frame(site=subtbl$site[1], date=subtbl$date[1],meanAuto = meanAuto,MedAuto=MedAuto,dispersionAuto=dispersionAuto,observer=observer,sd1=sd1)
  fin=rbind(fin,row1)
  }
  




fin$DisPercent= (fin$dispersionAuto/fin$MedAuto)*100
fin$SDPercent= (fin$sd1/fin$MedAuto)*100
fin$Dev= (fin$observer-fin$MedAuto)/fin$observer *100

super = fin[fin$DisPercent <= 25,]
super
SoSo = fin[fin$DisPercent > 25 & fin$DisPercent <=55,]
SoSo
bad= fin[fin$DisPercent > 55,]
bad



plot( fin$Dev ~ fin$DisPercent)