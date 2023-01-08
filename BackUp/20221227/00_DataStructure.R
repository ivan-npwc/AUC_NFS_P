
	System_data
    labelInput
   date1=substr(basename(labelInput),1,15)

  

  PolygonDir=paste0(labelInput,"\\Polygons")
  ObserverCountDir=paste0(labelInput,"\\Observer_count")

  AnimalMeasurDir=paste0(labelInput,"\\Polygons\\Animal_measurments")
  HauloutDir=paste0(labelInput,"\\Polygons\\Haulout")
  RookeryDir=paste0(labelInput,"\\Polygons\\Rookery")
   ExcludeDir=paste0(labelInput,"\\Polygons\\Exclude")
   ModelPolDir=paste0(labelInput,"\\Polygons\\Model")
  

  dir.create(PolygonDir,showWarnings = F)
  dir.create(ObserverCountDir,showWarnings = F)
   dir.create(AnimalMeasurDir,showWarnings = F)
   dir.create(HauloutDir,showWarnings = F)
   dir.create(RookeryDir,showWarnings = F)
  dir.create(ExcludeDir,showWarnings = F)
   dir.create(ModelPolDir,showWarnings = F)










