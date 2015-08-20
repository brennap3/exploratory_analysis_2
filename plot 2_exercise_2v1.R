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



NEI <- readRDS("summarySCC_PM25.rds")

SCC <- readRDS("Source_Classification_Code.rds")


##Plot 2


##Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
##from 1999 to 2008? Use the base plotting system to make a plot answering this question.

str(NEI)

emissions.by.year.baltimore<-NEI %>% filter(fips=="24510") %>% group_by(year) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year)
png("plot2v1a_exploratory_analysis_2.png", width=480, height=480,res=120)  ##call it plot 1
par(mfcol=c(1,1)) ##use one column and one row
par(mar=c(5,5,4,4) + 0.2) ##set the margins
barplot(emissions.by.year.baltimore$sum_by_year,names=emissions.by.year.baltimore$year, main="Total PM2.5 emission \nfrom Baltimore for each of the \nyears 1999, 2002, 2005, and 2008",xlab="Year", ylab="\n\n\nTotal PM2.5 \nemission \nfrom all sources",col="black",cex.main=0.65,cex.lab=0.65,cex.axis=0.65)
dev.off()
##yes


