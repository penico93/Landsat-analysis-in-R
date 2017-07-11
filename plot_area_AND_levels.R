#Plot area of ayapel wetland

library(ggplot2)
setwd("E:/Sedimentology/Landsat_files/")

data.l5 <- read.table('table_landsat5.txt')
data.l7 <- read.table('table_landsat7.txt')
data.l8 <- read.table('table_landsat8.txt')

data.ayapel <- rbind(data.l5,data.l7,data.l8)

data.ayapel$Date <- as.Date(data.ayapel$Date)
plot(data.ayapel$Date, data.ayapel$Area)


area_plot = ggplot(data = data.ayapel[data.ayapel$Date< as.Date("2005-01-01","%Y-%m-%d"),], aes(x=Date, y=Area))+
  geom_line() +  geom_line(data = data.ayapel[data.ayapel$Date> as.Date("2005-01-01","%Y-%m-%d"),]) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("Area ("~"m"^"2"*")")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90)) + 
  annotate("rect", xmin=as.Date("2010-04-01","%Y-%m-%d"), xmax = as.Date("2012-03-28","%Y-%m-%d"),ymin= 0, ymax= max(data.ayapel$Area),alpha=.1,fill="blue")

# SOI Data from: https://www.ncdc.noaa.gov/teleconnections/enso/indicators/soi/data.csv


#+
  #scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) #+scale_y_continuous(limits = c(0, 7500))


#Highlight a rectangle in the area of the plot where the LA nina events go

#find the exact dates of rupture of the cauca river


#Ayapel levels obtain by Augusto
data.level <- read.table('ayapel_levels.txt', header = TRUE)
data.level$date <- as.Date(data.level$date, "%d/%m/%Y")

#level_plot = 
  ggplot(data = data.level, aes(x=date, y=level))+
  geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  xlab("Date") +ylab("Wetland level [m] ")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90)) + 
  annotate("rect", xmin=as.Date("2010-04-01","%Y-%m-%d"), xmax = as.Date("2012-03-28","%Y-%m-%d"),ymin= 22, ymax= max(data.level$level),alpha=.1,fill="blue")

  
  
  
  p3 <- ggplot(data = data.level[data.level$year %in% c(1999,2000,2008,2009,2010,2011,2012),] ,aes(x=month,y=level, colour = factor(year), group=year)) + geom_line(size=0.8)+
    theme(text = element_text(size=8), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),panel.border = element_rect(colour="black", fill=NA, size=1),axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Mean monthly wetland water Level")+
    xlab("Month") + ylab("Ayapel Wetland level (cms)")+
    scale_x_discrete(limits = c("January", "February", "March","April","May","June","July","August","September","October","November","December"))
  
  

#Decompose function https://stat.ethz.ch/R-manual/R-devel/library/stats/html/decompose.html
#understand decomp https://anomaly.io/seasonal-trend-decomposition-in-r/
#https://onlinecourses.science.psu.edu/stat510/node/48
  # 
  # Moving averages?
  # 
  
  