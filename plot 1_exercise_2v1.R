##git hub respository is here: https://github.com/brennap3/exploratory_analysis_2
##https://github.com/brennap3/exploratory_analysis_2/blob/master/rmd_assignment_2.html
##https://github.com/brennap3/exploratory_analysis_2/blob/master/rmd_assignment_2.Rmd



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

NEI <- readRDS("C:\\Users\\Peter\\Documents\\summarySCC_PM25.rds")
SCC <- readRDS("C:\\Users\\Peter\\Documents\\Source_Classification_Code.rds")

str(SCC)
summary(SCC)
##plot1

##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
##make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

dplyr::glimpse(NEI)
head(NEI)
str(NEI)
??xaxt
emissions.by.year<-NEI %>% group_by(year) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year) 
png("plot1v1a_exploratory_analysis_2.png", width=480, height=480,res=120)  ##call it plot 1
par(mfcol=c(1,1)) ##use one column and one row
par(mar=c(5,5,4,4) + 0.2) ##set the margins
barplot(emissions.by.year$sum_by_year,names=emissions.by.year$year, main="\n\n\nTotal PM2.5 emission\n from all sources for each of the \nyears 1999, 2002, 2005,\n and 2008", xlab="Year", ylab="\n\nTotal PM2.5\n emission \nfrom all sources",col="black",cex.main=0.65,cex.lab=0.65,cex.axis=0.65)
dev.off()

