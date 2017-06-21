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
#Making a subset of the bands in a stack: subset(raster,2:7) in this case from the bands we are interested in
lsat2 <- subset(lsat,2:7)
#extent to use
e <-extent(475000, 500000,  910000,  937500)
#crop the composite to improve pc efficiency
lsat3<-crop(lsat2,e)

#function to calculate the water index. In this case, I will use: NDWI from McFeeters, S. K. (1996)
#here you can change for whichever index you prefer...
watindex <- function(img, k, i){
  bi <- img[[i]] #near infrared
  bk <- img[[k]] #green band
  watindex <- (bk-bi)/(bk+bi)
  return(watindex)
}

#For Landsat 8 the green band is the band #3 but as we did a subset it is band #2
#the nearinfrared is band #5 but as we did a subset it is band #4
#apply the function and save it in wat
wat <- watindex(lsat3,2,4)

#K means cluster insupervised analysis is applied to the object "wat"

#5 categories are selected
wat.kmeans <- kmeans(wat[], 5, iter.max = 100, nstart = 3)
#create a blank raster
kmeansraster<-raster(wat)
#fill blank raster cluster column to the raster
kmeansraster[]<-wat.kmeans$cluster

# Now we determine which of the 5 categories in the Kmeans cluster analysis
#is the one representing water by using a shapefile of points of which we now
# a priori that should be inside the Ayapel wetland

# Read shapefile containing the points: ayapel_water_points
points.1 <- readOGR(dsn="E:/Sedimentology/Project FINAL/Area_ayapel_polygons", layer = "ayapel_water_points")
#extract the values from the kmeans raster to the points 
ext <- extract(kmeansraster,points.1)

#check which is the more common value in the points or the mode
# this will be the value (out of the 5 categories) that represents water
temp <- table(as.vector(ext))
mod <- names(temp)[temp==max(temp)]
#Obtain the value by converting the string to int with the function strtoi()
val <- strtoi(mod)

#subset the kmeans raster by this value
kmeansraster2 <- kmeansraster == val


#Load a shapefile with a polygon that limits the area of maximum flood or maximum area
# for the wetland

#FUNCTION TO READ SHAPEFILES READOGR FROM THE RGDAL PACKAGE
poly.AREA <- readOGR(dsn="E:/Sedimentology/Project FINAL/Area_ayapel_polygons", layer = "DEFINEDAREA")

#count pixels within the polygon of maximum inundation
count_within_poly <- extract(kmeansraster2,poly.AREA)

#obtain the counts in the list ext: this would be pixel counts that have a value of 1
count.pixel <- table(count_within_poly)[[2]]

#Obtain area by multiplying the number of pixels counted in the object
# count.pixel by the area of a pixel which is 30*30 meters
area.wetland <- count.pixel*30*30


