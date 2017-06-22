#With this script you can process a L8 scene and plot it both corrected and uncorrected...

#requried libraries

library(raster)
library(rgdal)
library(maptools)
library(RStoolbox)


#Code for Landsat 8 images
#This code reads the Lansdat8 images stored in a folder, corrects this images from
#DN's to surface reflectace and calculates the area of water within a polygon.

# the only input is the filename
#files are stored in E:/Sedimentology/Landsat_files
#setwd("E:/Sedimentology/Landsat_files")

filename1 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LC8", full.names = TRUE, ignore.case = TRUE)
filename2 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LC08", full.names = TRUE, ignore.case = TRUE)

filename <- c(filename1,filename2)

# i am going to do everything for the first file in this list and then the idea is to replicate for all of the files with a loop

f.name <- filename[1]

#set working directory to the directory of f.name
setwd(f.name)

#get metadatafilename from f.name directory use pattern MTL present in the metadata files
met_filename <- list.files(pattern = "MTL", full.names = TRUE)


#Read metadata from the directory f.name
metaData <- readMeta(met_filename)

#Read files associate with the metadata file i.e. read bands L8
lsat <- stackMeta(metaData)

#subset and crop stack
#extent to use
e <-extent(475000, 500000,  910000,  937500)
#crop the composite to improve pc efficiency
lsat2<-lsat

## Correct DN to at-surface-reflecatance with simple DOS
## Automatic haze estimation
hazeDN <- estimateHaze(lsat2, hazeBands = 2:5, darkProp = 0.01, plot = TRUE)
lsat3 <- radCor(lsat2, metaData = metaData, method = "sdos",hazeValues = hazeDN, hazeBands = 2:5)

plotRGB(lsat2, r = 4, g = 5, b = 6, axes = TRUE, stretch = "lin", main = "Landsat uncorrected")
plotRGB(lsat3, r = 4, g = 5, b = 6, axes = TRUE, stretch = "lin", main = "Landsat corrected to SR")

#Clear workspace
rm(list = ls())
