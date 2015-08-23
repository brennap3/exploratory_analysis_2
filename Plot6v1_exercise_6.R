##PLot6
##git hub respository is here: https://github.com/brennap3/exploratory_analysis_2
##https://github.com/brennap3/exploratory_analysis_2/blob/master/rmd_assignment_2.html
##https://github.com/brennap3/exploratory_analysis_2/blob/master/rmd_assignment_2.Rmd
##you are only interested in plot6v4_exploratory_analysis_2.png"
##some usefull links

##https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
##https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
##https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
##http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
##http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-softwar...


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

##6

##Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
##in Los Angeles County, California (fips == "06037"). 
##Which city has seen greater changes over time in motor vehicle emissions?


emissions.by.year.motor.vehicles.baltimore.la<-NEI %>% filter(fips %in% c("24510","06037")&type=="ON-ROAD") %>% group_by(year,fips) %>% summarize(sum_by_year=sum(Emissions)) %>% arrange(year) %>% data.frame

str(emissions.by.year.motor.vehicles.baltimore.la)

gpav1<-ggplot(emissions.by.year.motor.vehicles.baltimore.la, aes(factor(year), (sum_by_year),fips)) +
  geom_bar(aes(fill=fips),stat="identity",position="dodge") +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("Total emission from vehicle Based Sources")+
  theme_bw() ##substantaill fall

plot(gpav1)

ggsave("plot6av1_exploratory_analysis_2.png",width=7,height=7)



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

gpav2<-ggplot(fips_added, aes(factor(year), (sum_by_year),fips_name)) +
  geom_bar(aes(fill=fips_name),stat="identity",position="dodge") +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("log of Total emission from vehicle Based Sources")+
  theme_bw() ##substantaill fall

plot(gpav2)

ggsave("plot6av2_exploratory_analysis_2.png",width=7,height=7)


spgglt1<-ggplot(fips_added, aes(factor(year), (sum_by_year),fips_name)) +
  geom_bar(aes(fill=fips_name),stat="identity",position="dodge") +
  ggtitle("Total emission from Motor_Vehicle Based Sources for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("Total emission from \nvehicle Based Sources")+
  theme_bw() +  ##substantaill fall
  theme(plot.title = element_text(size = 4),
        axis.title.x = element_text(size = 4),
        axis.title.y = element_text(size = 4),
        legend.title= element_text(size = 4),
        axis.text.x= element_text(size = 4),
        axis.text.y= element_text(size = 4),
        legend.text= element_text(size = 4)
  )
  
  
  plot(spgglt1)
last_plot()

?lag
##get the laggin change or different cityies

##6v4

fips_added_lag_sum_by_year<-data.frame(rbind(filter(fips_added,fips_name=='LA County') %>% arrange(year) %>% dplyr::mutate(lagging_sum_by_year=ifelse(is.na(sum_by_year-lag(sum_by_year,k=1)),0,(sum_by_year-lag(sum_by_year,k=1))))
                                             ,filter(fips_added,fips_name=='Baltimore') %>% arrange(year) %>% dplyr::mutate(lagging_sum_by_year=ifelse(is.na(sum_by_year-lag(sum_by_year,k=1)),0,(sum_by_year-lag(sum_by_year,k=1))))))


spggplt2<-ggplot(fips_added_lag_sum_by_year, aes(factor(year), (lagging_sum_by_year),fips_name)) +
  geom_bar(aes(fill=fips_name),stat="identity",position="dodge") +
  ggtitle("Total emission from Motor_Vehicle Based Sources \n- previous years emission (if no previous year value is 0) \n for each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("Total emission \n- previous years emssions \nfrom vehicle Based Sources")+
  theme_bw() +  ##substantaill fall
  theme(plot.title = element_text(size = 4),
        axis.title.x = element_text(size = 4),
        axis.title.y = element_text(size = 4),
        legend.title= element_text(size = 4),
        axis.text.x= element_text(size = 4),
        axis.text.y= element_text(size = 4),
        legend.text= element_text(size = 4)
  )

plot(spggplt2)



##facet plt

ggsplt2<-ggplot(fips_added_lag_sum_by_year, aes(year, lagging_sum_by_year)) +
  geom_bar(aes(fill=fips_name),stat="identity",position="dodge") +
  ggtitle("Total emission from Motor_Vehicle Based Sources \n- previous years emission (if no previous year value is 0) \nfor each of the \nyears 1999, 2002, 2005, and 2008 in \nBaltimore(24510) and LA(06037)")+
  xlab("Year")+
  ylab("Total emission \n- previous \nyears emssions \nfrom vehicle \nBased Sources")+
  theme_bw() +  ##substantaill fall
  theme(plot.title = element_text(size = 4),
        axis.title.x = element_text(size = 4),
        axis.title.y = element_text(size = 4),
        legend.title= element_text(size = 4),
        axis.text.x= element_text(size = 4),
        axis.text.y= element_text(size = 4),
        legend.text= element_text(size = 4)
        )

ggsplt2+facet_grid(fips_name ~.)

??plot_grid

plot_grid(spgglt1,spggplt2, ncol=1,nrow=2)
##this is the plot you are interested in
ggsave("plot6v4_exploratory_analysis_2.png",width=8,height=8,units="cm") 
