#Function and loop to calculate area of the ayapel wetland in landsat 8 images

#if_Landsat7

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

filename1 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LE7", full.names = TRUE, ignore.case = TRUE)
filename2 <- dir("E:/Sedimentology/Landsat_files", pattern = "^LE07", full.names = TRUE, ignore.case = TRUE)

filename <- c(filename1,filename2)




calc.AREA <- function(file_name){
  
  f.name <- file_name
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
  lsat2<-crop(lsat,e)
  
  ### Convert DN to top of atmosphere reflectance and brightness temperature
  #lsat_ref <- radCor(crop(lsat,e), metaData = metaData, method = "apref")
  
  ## Correct DN to at-surface-reflecatance with simple DOS
  ## Automatic haze estimation
  hazeDN <- estimateHaze(lsat2, hazeBands = 1:4, darkProp = 0.01, plot = TRUE)
  lsat3 <- radCor(lsat2, metaData = metaData, method = "sdos",hazeValues = hazeDN, hazeBands = 1:4)
  
  #for landsat 7 only!!!
  #Making a subset of the bands in a stack: subset(raster,c(1:5,7)) in this case from the bands we are interested in
  lsat3 <- subset(lsat3,c(1:5,7))
  
  #function to calculate the water index. In this case, I will use: NDWI from McFeeters, S. K. (1996)
  #here you can change for whichever index you prefer...
  
  #water index used is NDWI
  #Reference:McFeeters, S. K. (1996). The use of normalized difference water index (NDWI) in the delineation of open water features. International Journal of Remote Sensing, 17 (7), 1425-1432.
  #now we are using the modified MNDWI by Xu 2006 that takes B5 of L7 instead
  # of B4
  
  watindex <- function(img){
    bi <- img[[5]] #near infrared, in Xu 2006 middle infrared
    bk <- img[[2]] #green band
    watindex <- (bk-bi)/(bk+bi)
    return(watindex)
    
  }
  
  #this index below is very bad...
  # watindex <- function(img){
  #   
  #   #4  ??band2?????band5
  #   #? ???? 0:25  ??band4 ? 2:75  ??band7
  #   #? ?
  # 
  #   watindex <- (img[[2]] -img[[5]])*4 - (0.25*(img[[4]])+(img[[6]])*2.75) 
  #   return(watindex)
  # 
  # 
  # }
  #For Landsat 8 the green band is the band #3 but as we did a subset it is band #2
  #the nearinfrared is band #5 but as we did a subset it is band #4
  #apply the function and save it in wat
  wat <- watindex(lsat3)
  
  classes <- setValues(raster(wat), 1)
  
  names(classes) <- "class"
  
  wat <- addLayer(wat, classes)
  #K means cluster insupervised analysis is applied to the object "wat"
  
  #5 categories are selected I need to fix this because I have nan's
  km <- kmeans(na.omit(wat[]), 3, iter.max = 100, nstart = 3)
  
  #check if there are na in the raster
  #this part of the method is taken from:https://geoscripting-wur.github.io/AdvancedRasterAnalysis/
  
  #obtain a vector with the values in wat
  dat <- getValues(wat)
  
  #check if there is any NA in the vector dat
  any(is.na(dat))
  
  #we need to have a kind of mask raster, indicating where the NA values
  #throughout the wat raster are located.
  
  #Create a blank raster of the size of wat with default values of 0
  rNA <- setValues(raster(wat), 0)
  #assign a value of 1 to the former raster of 0's where there are NA's in wat
  rNA[is.na(wat[[1]])] <- 1
  
  #now we convert rNA to a vector
  rNA <- getValues(rNA)
  
  #conver dat into a dataframe
  dat <- as.data.frame(dat)
  
  #If rNA is a 0, assign the cluster value at that position
  
  dat$class[rNA==0] <- km$cluster
  
  dat$class[rNA==1] <- NA
  
  ## Create a blank raster
  classes2 <- raster(wat)
  ## Assign values from the 'class' column of valuetable
  classes3 <- setValues(classes2, dat$class)
  #plot(classes3, legend=FALSE, col=c("dark green", "orange", "light blue"))
  
  kmeansraster <- classes3
  
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
  scene.area <- count.pixel*30*30

  #Cout NA's : only 1238 pixles... very small number compared to the total...
  #table(extract(kmeansraster,poly.AREA), useNA="always")[[4]]
  
  
  #obtain aqcuisition date, satellite number, area and save in dataframe...
  #save 3 images WAT, original and Kmeans ==1 in order to check them later...
  
  #obtain ACQUISITION_DATE: metaData$ACQUISITION_DATE
  scene.date <- substr(metaData$ACQUISITION_DATE, start=1, stop=10)
  scene.sat <-metaData$SATELLITE
  
  df.guardar <- data.frame(matrix(0,ncol=3,nrow=1))
  df.guardar[1] <- scene.date
  df.guardar[2] <- scene.area
  df.guardar[3] <- scene.sat
  
  colnames(df.guardar) <- c("Date","Area","Satellite")
  
  directory <- "E:/Sedimentology/Pdf_processed_image_ayapel/"
  
  pdf_name <- paste(c(directory,scene.date,"_",scene.sat,'.pdf'), collapse="")
  pdf(file = pdf_name,width = 10, height = 10)
  par(mfrow=c(2,2))
  plotRGB(lsat2, r = 4, g = 5, b = 6, axes = TRUE, stretch = "lin", main = paste(c("corrected ",scene.sat," " ,scene.date), collapse =""))
  plot(wat[[1]], main = "MNDWI index")
  plot(kmeansraster, main = "K-means clusters")
  plot(points.1, add = T)
  plot(kmeansraster2, main = "K-means water cluster")
  plot(poly.AREA, add = T)
  text(x = 480000, y= 935000, paste(c("area: ",scene.area), collapse =""))
  dev.off()
  

  return(df.guardar)
  
  #Clear workspace
  rm(list = ls())
  
}


#Create a dataframe
df.1 <- data.frame(matrix(0,ncol=3,nrow=0))
colnames(df.1) <- c("Date","Area","Satellite")

for(i in 45:length(filename)){
#for(i in 1:10){
  f.name <- filename[i]
  newrow <- calc.AREA(f.name)
  df.1 <- rbind(df.1,newrow)
  
}

#Write 
write.table(df.1, file=paste(c("E:/Sedimentology/Landsat_files/","table_landsat7.txt"),collapse=""))
