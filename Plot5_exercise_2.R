
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
##plot5

##How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
##they ahve fallen
str(NEI)
str(SCC)
summary(NEI)
utils::View(SCC)
##we will assume that everything on the road is a motor vehicle
##

##DT[tolower(EI.Sector) %like% "vehicles"   ,SCC,EI.Sector] could use this

emissions.by.year.motor.vehicles.baltimore<-NEI %>% filter(fips=="24510"&type=="ON-ROAD") %>% group_by(year) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year)

ggplot5<-ggplot(emissions.by.year.motor.vehicles.baltimore, aes(year, sum_by_year)) +
  geom_point()+  
  geom_smooth(method="loess") +
  coord_cartesian() +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in Baltimore")+
  xlab("Year")+
  ylab("Total emission from vehicle Based Sources")+
  theme_bw() ##substantaill fall

plot(ggplot5)

last_plot()

ggsave("plot5_exploratory_analysis_2.png",width=5,height=5)
