##Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
##which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
##Which have seen increases in emissions from 1999-2008? 
##Use the ggplot2 plotting system to make a plot answer this question.

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


##plot 3


##Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
##which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
##Which have seen increases in emissions from 1999-2008? 
##Use the ggplot2 plotting system to make a plot answer this question.

NEI <- readRDS("summarySCC_PM25.rds")

SCC <- readRDS("Source_Classification_Code.rds")

emissions.by.year.type.baltimore<-NEI %>% filter(fips=="24510") %>% group_by(year,type) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year)

ggplot3<-ggplot(emissions.by.year.type.baltimore, aes(year, sum_by_year,type)) +
  geom_point(aes(shape = type,colour=type))  + ##geom_line(aes(color=type)) +
  geom_smooth(aes(color=type),method="loess") +
  coord_cartesian() +
  ggtitle("Total PM2.5 emission from Baltimore for each of the \nyears 1999, 2002, 2005, and 2008 by type")+
  xlab("Year")+
  ylab("Total PM2.5 emission \nfrom all sources")+
  theme_bw()

plot(ggplot3)

last_plot()

ggsave("plot3_exploratory_analysis_2.png",width=5,height=5)

