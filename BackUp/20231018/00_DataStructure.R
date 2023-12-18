

  labelInput
	
  PolygonDir=paste0(labelInput,"\\Polygons")
  ObserverCountDir=paste0(labelInput,"\\Observer_count")

 
  PredPolDir=paste0(labelInput,"\\Polygons\\Predict")

   ModelPolDir=paste0(labelInput,"\\Polygons\\Model")
   # RookeryDir=paste0(labelInput,"\\Polygons\\Rookery")
   # ExcludeDir=paste0(labelInput,"\\Polygons\\Exclude")
   # AnimalMeasurDir=paste0(labelInput,"\\Polygons\\Animal_measurments")
  dir.create(PolygonDir,showWarnings = F)
  dir.create(ObserverCountDir,showWarnings = F)
 
  dir.create(PredPolDir,showWarnings = F)
  dir.create(ModelPolDir,showWarnings = F)
  # dir.create(RookeryDir,showWarnings = F)
  # dir.create(ExcludeDir,showWarnings = F)
  #dir.create(AnimalMeasurDir,showWarnings = F)










