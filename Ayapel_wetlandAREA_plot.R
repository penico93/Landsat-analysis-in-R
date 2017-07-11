#Ayapel_area(1990-2015)
#Read Ayapel Area data from lansat processing

setwd("E:/Sedimentology/Landsat_files/")
data.l5 <- read.table('table_landsat5.txt')
data.l7 <- read.table('table_landsat7.txt')
data.l8 <- read.table('table_landsat8.txt')
data.ayapel <- rbind(data.l5,data.l7,data.l8)
data.ayapel$Date <- as.Date(data.ayapel$Date)
data.ayapel<-data.ayapel[,-3]


x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.Ayapel_area <- ggplot(data = data.ayapel[data.ayapel$Date< as.Date("2005-01-01","%Y-%m-%d"),], aes(x=Date, y=Area))+
  geom_line() +  geom_line(data = data.ayapel[data.ayapel$Date> as.Date("2005-01-01","%Y-%m-%d"),])+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Ayapel wetland Area")+
  xlab("")+ ylab("Area ["~"m"^"2"*"]")+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) 
#"Area ["~"m"^"2"*"/s]"
#END