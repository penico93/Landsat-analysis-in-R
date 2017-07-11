#requried libraries

library(raster)
library(rgdal)
library(maptools)
library(RStoolbox)

# READ filename_L8s of LANDSAT 8

filename_L81_L8 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LC8", full.names = TRUE, ignore.case = TRUE)
filename_L82_L8 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LC08", full.names = TRUE, ignore.case = TRUE)
filename_L8 <- c(filename_L81_L8,filename_L82_L8)

filename1_L7 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LE7", full.names = TRUE, ignore.case = TRUE)
filename2_L7 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LE07", full.names = TRUE, ignore.case = TRUE)
filename_L7 <- c(filename1_L7,filename2_L7)

filename_L5 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LT05", full.names = TRUE, ignore.case = TRUE)


#START OF CODE FOR L8

printPDF_lansat8 <- function(file_name){
  f.name <- file_name
  #set working directory to the directory of f.name
  setwd(f.name)
  #get metadatafilename_L8 from f.name directory use pattern MTL present in the metadata files
  met_filename_L8 <- list.files(pattern = "MTL", full.names = TRUE)
  #Read metadata from the directory f.name
  metaData <- readMeta(met_filename_L8)
  #Read files associate with the metadata file i.e. read bands L8
  lsat <- stackMeta(metaData)
  #subset and crop stack
  #extent to use
  e <-extent(475000, 525000,  910000,  967500)
  #crop the composite to improve pc efficiency
  #lsat2<-crop(lsat,e)
  #Making a subset of the bands in a stack: subset(raster,2:7) in this case from the bands we are interested in
  lsat2 <- subset(lsat,2:7)
  
  #obtain aqcuisition date, satellite number, area and save in dataframe...
  #save 3 images WAT, original and Kmeans ==1 in order to check them later...
  
  #obtain ACQUISITION_DATE: metaData$ACQUISITION_DATE
  scene.date <- substr(metaData$ACQUISITION_DATE, start=1, stop=10)
  scene.sat <-metaData$SATELLITE

  
  directory <- "E:/Sedimentology/Pdf_processed_image_ayapel/certainAREA/Wetland_connections/"
  
  pdf_name <- paste(c(directory,scene.date,"_",scene.sat,'.pdf'), collapse="")
  pdf(file = pdf_name,width = 10, height = 10)
  plotRGB(lsat2, r = 4, g = 5, b = 6, axes = TRUE, stretch = "hist", ext=e, main = paste(c("",scene.sat," " ,scene.date), collapse =""))
  dev.off()
  
  texto <- paste (c("funciono para ", scene.date,"_",scene.sat ), collapse = '')
  return(texto)
  
  #Clear workspace
  rm(list = ls())
  
}





for(i in 1:length(filename_L8)){
#for(i in 1:15){
  f.name <- filename_L8[i]
  newrow <- printPDF_lansat8(f.name)
  print(newrow)
  
}
#END OF CODE FOR L8


#BEGIN OF CODE FOR L7

printPDF_lansat7 <- function(file_name){
  f.name <- file_name
  #set working directory to the directory of f.name
  setwd(f.name)
  #get metadatafilename_L7 from f.name directory use pattern MTL present in the metadata files
  met_filename_L7 <- list.files(pattern = "MTL", full.names = TRUE)
  #Read metadata from the directory f.name
  metaData <- readMeta(met_filename_L7)
  #Read files associate with the metadata file i.e. read bands L7
  lsat <- stackMeta(metaData)
  #subset and crop stack
  #extent to use
  e <-extent(475000, 525000,  910000,  967500)
  #crop the composite to improve pc efficiency
  #lsat2<-crop(lsat,e)
  #Making a subset of the bands in a stack: subset(raster,2:7) in this case from the bands we are interested in
  lsat2 <- subset(lsat,c(1:5,7))
  #obtain aqcuisition date, satellite number, area and save in dataframe...
  #save 3 images WAT, original and Kmeans ==1 in order to check them later...
  
  #obtain ACQUISITION_DATE: metaData$ACQUISITION_DATE
  scene.date <- substr(metaData$ACQUISITION_DATE, start=1, stop=10)
  scene.sat <-metaData$SATELLITE
  
  
  directory <- "E:/Sedimentology/Pdf_processed_image_ayapel/certainAREA/Wetland_connections/"
  
  pdf_name <- paste(c(directory,scene.date,"_",scene.sat,'.pdf'), collapse="")
  pdf(file = pdf_name,width = 10, height = 10)
  plotRGB(lsat2, r = 4, g = 5, b = 6, axes = TRUE, stretch = "hist", ext=e, main = paste(c("",scene.sat," " ,scene.date), collapse =""))
  dev.off()
  
  texto <- paste (c("funciono para ", scene.date,"_",scene.sat ), collapse = '')
  return(texto)
  
  #Clear workspace
  rm(list = ls())
  
}





for(i in 1:length(filename_L7)){
  #for(i in 1:15){
  f.name <- filename_L7[i]
  newrow <- printPDF_lansat7(f.name)
  print(newrow)
  
}


#END OF CODE FOR L7

#START OF CODE FOR L5

printPDF_lansat5 <- function(file_name){
  f.name <- file_name
  #set working directory to the directory of f.name
  setwd(f.name)
  #get metadatafilename_L5 from f.name directory use pattern MTL present in the metadata files
  met_filename_L5 <- list.files(pattern = "MTL", full.names = TRUE)
  #Read metadata from the directory f.name
  metaData <- readMeta(met_filename_L5)
  #Read files associate with the metadata file i.e. read bands L5
  lsat <- stackMeta(metaData)
  #subset and crop stack
  #extent to use
  e <-extent(475000, 525000,  910000,  967500)
  #crop the composite to improve pc efficiency
  #lsat2<-crop(lsat,e)
  #Making a subset of the bands in a stack: subset(raster,2:7) in this case from the bands we are interested in
  lsat2 <- subset(lsat,c(1:5,7))
  #obtain aqcuisition date, satellite number, area and save in dataframe...
  #save 3 images WAT, original and Kmeans ==1 in order to check them later...
  
  #obtain ACQUISITION_DATE: metaData$ACQUISITION_DATE
  scene.date <- substr(metaData$ACQUISITION_DATE, start=1, stop=10)
  scene.sat <-metaData$SATELLITE
  
  
  directory <- "E:/Sedimentology/Pdf_processed_image_ayapel/certainAREA/Wetland_connections/"
  
  pdf_name <- paste(c(directory,scene.date,"_",scene.sat,'.pdf'), collapse="")
  pdf(file = pdf_name,width = 10, height = 10)
  plotRGB(lsat2, r = 4, g = 5, b = 6, axes = TRUE, stretch = "hist", ext=e, main = paste(c("",scene.sat," " ,scene.date), collapse =""))
  dev.off()
  
  texto <- paste (c("funciono para ", scene.date,"_",scene.sat ), collapse = '')
  return(texto)
  
  #Clear workspace
  rm(list = ls())
  
}





for(i in 1:length(filename_L5)){
  #for(i in 1:15){
  f.name <- filename_L5[i]
  newrow <- printPDF_lansat5(f.name)
  print(newrow)
  
}
#END OF CODE FOR L5