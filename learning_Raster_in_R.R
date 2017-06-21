
library(raster)
library(rgdal)
library(maptools)

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
  
  #create a blank raster
  
  kmeansraster<-raster(wat)
  
  #fill blank raster cluster column to the raster
  kmeansraster[]<-wat.kmeans$cluster
  
 # kmeansraster2 <- kmeansraster ==4
  
  plot(kmeansraster)
  
  
  #writeRaster(kmeansraster,"p224r63_unsup_5_classif.tif", overwrite=TRUE)
  
  #manage a raster like this vcfGewata[vcfGewata > 100] <- NA
  
  #Making surface reflectance and Top of the atmosphere corrections
  
  library(RStoolbox)
  
  metaData <- readMeta("E:/Sedimentology/Project FINAL/new scenes without process/LC08_L1TP_009054_20151229_20170331_01_T1/LC08_L1TP_009054_20151229_20170331_01_T1_MTL.txt")
  
  #stackMeta reads all the bands associated with the metadata and creates a composite
  lsat <- stackMeta(metaData)
  ## Convert DN to top of atmosphere reflectance and brightness temperature
  
  lsat_ref <- radCor(lsat, metaData = metaData, method = "apref")
  
  plotRGB(lsat_ref, r = 4, g = 5, b = 6, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")
  
  #writeRaster(lsat_ref,"lsat_refDNtoTOA.tif", overwrite=TRUE)
  plotRGB(lsat, r = 4, g = 5, b = 6, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")
  
  ## Correct DN to at-surface-reflecatance with DOS (Chavez decay model)
  lsat_sref2 <- radCor(lsat, metaData = metaData, method = "dos")
  
  #Making a subset of the bands in a stack: subset(raster,2:7) in this case from the bands we are interested in
  lsat_ref <- subset(lsat_ref,2:7)
  
  #create the index
  e <-extent(475000, 500000,  912510,  937500)
  #but first crop s
  lsat_ref<-crop(lsat_ref,e)
  
  plotRGB(lsat_ref, r = 4, g = 5, b = 6, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")
  
  #applying the water index
  wat <- watindex(lsat_ref,2,4)
  
  plot(wat)
  
  #hist(wat) 
  
  #how to count?
  
  #good webpage : http://rspatial.org/spatial/rst/8-rastermanip.html
  
  # subset a raster by cell value: kmeansraster2 <- kmeansraster ==4
  
  # extract values within an area
  # https://artax.karlin.mff.cuni.cz/r-help/library/raster/html/extract.html
  
  
  # se pueden poner varios puntos sobre pixeles que normalmente son agua
  #extraer el valor del kmeans y luego buscar la moda entre estos puntos
  #así se definiría qué es agua y que no...
  