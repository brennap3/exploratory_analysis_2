##some usefull links

##https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
##https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
##https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
##http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
##http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization


library(plyr)
library(dplyr)
library(ggplot2)
library(lattice)
library(magrittr)
library(sqldf)
library(gridExtra)
library(cowplot)

## This first line will likely take a few seconds. Be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

str(SCC)
summary(SCC)
##plot1

##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
##make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

dplyr::glimpse(NEI)
head(NEI)
str(NEI)

emissions.by.year<-NEI %>% group_by(year) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year) 

par(mfcol=c(1,1)) ##use one column and one row
par(mar=c(6,6,6,6) + 0.1) ##set the margins
?plot
plot(emissions.by.year$year,emissions.by.year$sum_by_year, main="Total PM2.5 emission from all sources for each of the \nyears 1999, 2002, 2005, and 2008", type="b",xlab="Year", ylab="Total PM2.5 emission \nfrom all sources",col="red")


##maybe change axis



##plot2


##Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
##from 1999 to 2008? Use the base plotting system to make a plot answering this question.

str(NEI)

emissions.by.year.baltimore<-NEI %>% filter(fips=="24510") %>% group_by(year) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year)

plot(emissions.by.year.baltimore$year,emissions.by.year.baltimore$sum_by_year, main="Total PM2.5 emission from Baltimore for each of the \nyears 1999, 2002, 2005, and 2008", type="b",xlab="Year", ylab="Total PM2.5 emission \nfrom all sources",col="red")


##yes


##plot 3


##Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
##which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
##Which have seen increases in emissions from 1999-2008? 
##Use the ggplot2 plotting system to make a plot answer this question.

emissions.by.year.type.baltimore<-NEI %>% filter(fips=="24510") %>% group_by(year,type) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year)


ggplot(emissions.by.year.type.baltimore, aes(year, sum_by_year,type)) +
  geom_point(aes(shape = type,colour=type))  + ##geom_line(aes(color=type)) +
                               geom_smooth(aes(color=type),method="loess") +
  coord_cartesian() +
  ggtitle("Total PM2.5 emission from Baltimore for each of the \nyears 1999, 2002, 2005, and 2008 by type")+
  xlab("Year")+
  ylab("Total PM2.5 emission \nfrom all sources")+
  theme_bw()



##plot 4
## plot Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
## for it might be clearer using the following plot

str(NEI)
summary(NEI)
utils::View(SCC)

library(data.table)
DT<-data.table(SCC)
DT[tolower(EI.Sector) %like% "coal",]
##DT[tolower(EI.Sector) %like% "coal",SCC,EI.Sector] ##looks to be correct
##SCC.COAL<- as.character(unique(DT[tolower(EI.Sector) %like% "coal",SCC])) ##see below its more neat
SCC.Coal.MAGRIT <- DT[tolower(EI.Sector) %like% "coal",SCC] %>% unique()  %>% as.character()  
##str(SCC.COAL)
SCC.Coal.MAGRIT




str(NEI)
nrow(NEI)
emissions.by.year.coal<-NEI %>% filter(SCC %in% SCC.Coal.MAGRIT ) %>% group_by(year) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year)

emissions.by.year.coal

ggplot(emissions.by.year.coal, aes(year, sum_by_year,type)) +
  geom_point()+  
  geom_smooth(method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Coal Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 by type")+
  xlab("Year")+
  ylab("Total emission from Coal Based Sources")+
  theme_bw()



##How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

str(NEI)
str(SCC)
summary(NEI)
utils::View(SCC)
##we will assume that everything on the road is a motor vehicle
##

##DT[tolower(EI.Sector) %like% "vehicles"   ,SCC,EI.Sector] could use this

emissions.by.year.motor.vehicles.baltimore<-NEI %>% filter(fips=="24510"&type=="ON-ROAD") %>% group_by(year) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year)

ggplot(emissions.by.year.motor.vehicles.baltimore, aes(year, sum_by_year)) +
  geom_point()+  
  geom_smooth(method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in Baltimore")+
  xlab("Year")+
  ylab("Total emission from vehicle Based Sources")+
  theme_bw() ##substantaill fall


##6

##Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
##in Los Angeles County, California (fips == "06037"). 
##Which city has seen greater changes over time in motor vehicle emissions?


emissions.by.year.motor.vehicles.baltimore.la<-NEI %>% filter(fips %in% c("24510","06037")&type=="ON-ROAD") %>% group_by(year,fips) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year) %>% data.frame

str(emissions.by.year.motor.vehicles.baltimore.la)




ggplot(emissions.by.year.motor.vehicles.baltimore.la, aes(year, log(sum_by_year),fips)) +
  geom_point(aes(shape = fips,colour=fips))+  
  geom_smooth(aes(color=fips),method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("log of Total emission from vehicle Based Sources")+
  theme_bw() ##substantaill fall




##6v1
##lets add a legend
library(car)
?recode


fips_name<-function(fips_code){
  fips_nm<-ifelse(fips_code=="06037",'LA County','Baltimore')
}

test
x<-fips_name('06037')




fips_added<-cbind(mapply(fips_name,emissions.by.year.motor.vehicles.baltimore.la$fips),
                  emissions.by.year.motor.vehicles.baltimore.la)

colnames(fips_added)[1]<-"fips_name"

str(fips_added)


ggplot(fips_added, aes(year, log(sum_by_year),fips_name)) +
  geom_point(aes(shape = fips_name,colour=fips_name))+  
  geom_smooth(aes(color=fips_name),method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("log of Total emission from vehicle Based Sources")+
  theme_bw() ##substantaill fall

ggsave("plot6v2.png",width=7,height=7)

ggplot(fips_added, aes(year, (sum_by_year),fips_name)) +
  geom_point(aes(shape = fips_name,colour=fips_name))+  
  geom_smooth(aes(color=fips_name),method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("log of Total emission from vehicle Based Sources")+
  theme_bw() ##substantaill fall

ggsave("plot6v3.png",width=7,height=7)


spgglt1<-ggplot(fips_added, aes(year, (sum_by_year),fips_name)) +
  geom_point(aes(shape = fips_name,colour=fips_name))+  
  geom_smooth(aes(color=fips_name),method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("Total emission from \nvehicle Based Sources")+
  theme_bw() ##substantaill fall

plot(spgglt1)


##or facet
ggplt<-ggplot(fips_added, aes(year, log(sum_by_year))) +
  geom_point()+  
  geom_smooth(method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("log of Total emission from vehicle Based Sources")+
  theme_bw() ##substantaill fall

ggplt+facet_grid(fips_name ~.)



last_plot()

?lag
##get the laggin change or different cityies

##6v4

fips_added_lag_sum_by_year<-data.frame(rbind(filter(fips_added,fips_name=='LA County') %>% arrange(year) %>% dplyr::mutate(lagging_sum_by_year=ifelse(is.na(sum_by_year-lag(sum_by_year,k=1)),0,(sum_by_year-lag(sum_by_year,k=1))))
,filter(fips_added,fips_name=='Baltimore') %>% arrange(year) %>% dplyr::mutate(lagging_sum_by_year=ifelse(is.na(sum_by_year-lag(sum_by_year,k=1)),0,(sum_by_year-lag(sum_by_year,k=1))))))

str(fips_added_lag_sum_by_year)


spggplt2<-ggplot(fips_added_lag_sum_by_year, aes(year, (lagging_sum_by_year),fips_name)) +
  geom_point(aes(shape = fips_name,colour=fips_name))+  
  geom_smooth(aes(color=fips_name),method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources \n- previous years emission (if no previous year value is 0) \n for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("Total emission \n- previous years emssions \nfrom vehicle Based Sources")+
  theme_bw() ##substantaill fall

plot(spggplt2)



##facet plt

ggsplt2<-ggplot(fips_added_lag_sum_by_year, aes(year, lagging_sum_by_year)) +
  geom_point()+  
  geom_smooth(method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources \n- previous years emission (if no previous year value is 0) \nfor each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("Total emission \n- previous \nyears emssions \nfrom vehicle \nBased Sources")+
  theme_bw() ##substantaill fall


ggsplt2+facet_grid(fips_name ~.)

??plot_grid

plot_grid(spgglt1,spggplt2, ncol=1,nrow=2)

