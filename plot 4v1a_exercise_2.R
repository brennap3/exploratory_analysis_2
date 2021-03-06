##git hub respository is here: https://github.com/brennap3/exploratory_analysis_2
##https://github.com/brennap3/exploratory_analysis_2/blob/master/rmd_assignment_2.html
##https://github.com/brennap3/exploratory_analysis_2/blob/master/rmd_assignment_2.Rmd


#Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
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
library(data.table)


##plot 4
## plot Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
## for it might be clearer using the following plot
##there is a fair bit of preprocessing I get all the SCC's  that are like coal
## i then checked them
## I then got all emmssions by where the SCC contained these codes


## This first line will likely take a few seconds. Be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

str(SCC)
summary(SCC)

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

ggplot4<-ggplot(emissions.by.year.coal, aes(factor(year), sum_by_year)) +
  geom_bar(stat="identity")+
  ggtitle("Total emission from \nCoal Based Sources \nfor each of the \nyears 1999, 2002, 2005, and 2008")+
  xlab("Year")+
  ylab("Total emission \nfrom Coal Based Sources")+
  theme_bw()+
  theme(plot.title = element_text(size = 4),
        axis.title.x = element_text(size = 4),
        axis.title.y = element_text(size = 4),
        legend.title= element_text(size = 4),
        axis.text.x= element_text(size = 4),
        axis.text.y= element_text(size = 4),
        legend.text= element_text(size = 4)
  )

plot(ggplot4)

last_plot()

ggsave("plot4v1_exploratory_analysis_2.png",width=9,height=9,units="cm")
