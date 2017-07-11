# Script to read all the landsat in a folder and obtain the filename and acquisition date
# for each scene


#requried libraries

library(RStoolbox)
library(WriteXLS)

#READ filenames of Landsat folders

filename_L81_L8 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LC8", full.names = TRUE, ignore.case = TRUE)
filename_L82_L8 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LC08", full.names = TRUE, ignore.case = TRUE)
filename_L8 <- c(filename_L81_L8,filename_L82_L8)

filename1_L7 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LE7", full.names = TRUE, ignore.case = TRUE)
filename2_L7 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LE07", full.names = TRUE, ignore.case = TRUE)
filename_L7 <- c(filename1_L7,filename2_L7)

filename_L5 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LT05", full.names = TRUE, ignore.case = TRUE)

filenames <- c(filename_L5,filename_L7,filename_L8)
files<-basename(filenames)

#Create a dataframe with length(filenames)
df.guardar <- data.frame(matrix(0,ncol=3,nrow=length(filenames)))
colnames(df.guardar)<-c("filename","date","satellite")

#loop to go file by file asking for the acquisitio date and saving it in the dataframe
for(i in 1:length(filenames)){
  
  f.name <- filenames[i]
  #set working directory to the directory of f.name
  setwd(f.name)
  #get metadatafilename_L8 from f.name directory use pattern MTL present in the metadata files
  met_filename <- list.files(pattern = "MTL", full.names = TRUE)
  #Read metadata from the directory f.name
  metaData <- readMeta(met_filename)
  #obtain acquisition date from metadata file within scene folder
  scene.date <- substr(metaData$ACQUISITION_DATE, start=1, stop=10)
  #obtain which satellite took the image from metadata file within scene folder
  scene.sat <-metaData$SATELLITE
  
  df.guardar[i,1] <- files[i]
  df.guardar[i,2] <- scene.date
  df.guardar[i,3] <- scene.sat
}

setwd("E:/Sedimentology/Landsat_files/")
#Write 
write.table(df.guardar, file=paste(c("E:/Sedimentology/Landsat_files/","FileName&Date.txt"),collapse=""))

#write csv
write.csv(df.guardar, file="FileName&Date.csv")



