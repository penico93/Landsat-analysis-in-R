
library(raster)
library(rgdal)

setwd("E:/Sedimentology/Project FINAL/new scenes without process/LC08_L1TP_009054_20151229_20170331_01_T1")


f<- list.files(".",pattern=".TIF$")

s<-stack(f[5],f[7]) 

  
  comp <- brick('LC08_L1TP_009054_20131121_composite.tif')
  
  Band7<- raster('LC08_L1TP_009054_20131121_20170428_01_T1_B7.tif')

  
  nlayers(comp)
  nlayers(Band7)
  
  plotRGB(comp, r = 4, g = 5, b = 6, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")
  dev.off()
  #gives name of bands names(comp)  
#give extent of image extent(comp)
  
  #crop image crop(image,extent)
  
  e <-extent(450000, 520000,  900000,  1000000)
  comp2 <- crop(comp,e)
  plotRGB(comp2, r = 6, g = 5, b = 4, axes = TRUE, stretch = "hist", main = "Landsat True Color Composite")
  dev.off()
  
 #to plot only one band plot(Band7)
  
  
  
  #create the index
  e <-extent(470000, 500000,  900000,  950000)
  #but first crop s
  s<-crop(s,e)
  
  
  watindex <- function(img, k, i){
    bi <- img[[i]] #near infrared
    bk <- img[[k]] #green band
    watindex <- (bk-bi)/(bk+bi)
    return(watindex)
    }
  
  wat <- watindex(s,1,2)
  
  dev.new(width=5, height=4)

  
  pe <- extent(c(470000, 500000,  900000,  950000))
 
  plot(wat, ext = pe)
  
  
  wat.kmeans <- kmeans(wat[], 10, iter.max = 100, nstart = 3)
  kmeansraster<-raster(wat)
  kmeansraster[]<-wat.kmeans$cluster
  plot(kmeansraster)
  #writeRaster(kmeansraster,"p224r63_unsup_5_classif.tif", overwrite=TRUE)
  
  #manage a raster like this vcfGewata[vcfGewata > 100] <- NA
  
  library(RStoolbox)
  
  metaData <- readMeta("E:/Sedimentology/Project FINAL/new scenes without process/LC08_L1TP_009054_20151229_20170331_01_T1/LC08_L1TP_009054_20151229_20170331_01_T1_MTL.txt")
  
  