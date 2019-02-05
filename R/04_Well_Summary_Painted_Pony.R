#extrafont::loadfonts(device="win")
library(data.table)
library(ggplot2)
library(officer)
library(lubridate)
library(extrafont)
library(gridExtra)
library(XML)
library(methods)
library(plyr)
library(flextable)
library(gtools)
library(dplyr)
library(officer)
library(magrittr)
library(pryr)
library(gridExtra)
library(xlsx)
library(zoo)
library(reshape2)

#setwd("~/R/01_Projects_R/Project_Recap")
source('functions/pason_format.R')
source('functions/fluid_property.R')
source('functions/well_information.R')
source('functions/casing_data_xml.R')
source('functions/bit_data_xml.R')
source('functions/time_log_xml.R')

wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-79-H_94-B-09/data"
out_wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-79-H_94-B-09"
setwd(wd)
doc<- xmlTreeParse(file = 'ETS_0WA4_0040_2015_MM_DD_1.xml', useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file='1435664340.csv', header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])  
spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]

pason<-as.data.table(pason)
pason<-pason[Hole.Depth>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]

pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
pason[, Hole.Depth:=as.numeric(Hole.Depth)]
pason[, Hole.Depth.1:=as.numeric(Hole.Depth.1)]
pason[, Bit.Depth:=as.numeric(Bit.Depth)]
pason[, Hole.Depth:=as.numeric(Hole.Depth)]
well1<-pason[,c('Hole.Depth','days_from_spud')]
well1[, WELL_NAME:=well_data[[1]][2]]
well1[, UWI:=well_data[[1]][3]]
well1[, type:='INVERT']
#well2
wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-A79-H_94-B-09/data"
setwd(wd)
doc<- xmlTreeParse(file = 'ETS_0WA4_0040_2015_MM_DD_1.xml', useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file='1437695344.csv', header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])  
spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]

pason<-as.data.table(pason)
pason<-pason[Hole.Depth>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]

pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
pason[, Hole.Depth:=as.numeric(Hole.Depth)]
pason[, Hole.Depth.1:=as.numeric(Hole.Depth.1)]
pason[, Bit.Depth:=as.numeric(Bit.Depth)]
pason[, Hole.Depth:=as.numeric(Hole.Depth)]
well2<-pason[,c('Hole.Depth','days_from_spud')]
well2[, WELL_NAME:=well_data[[1]][2]]
well2[, UWI:=well_data[[1]][3]]
well2<- well2[1:28575,]
well2[, type:='INVERT']

#well3
wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-D79-H_94-B-09/data"
setwd(wd)
doc<- xmlTreeParse(file = 'ETS_0ZG2_0010_2017_MM_DD_1.xml', useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file='1490467200.csv', header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])  
spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]

pason<-as.data.table(pason)
pason<-pason[Hole.Depth..meters.>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]

pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
setnames(pason, old='Hole.Depth..meters.',new='Hole.Depth')

well3<-pason[,c('Hole.Depth','days_from_spud')]
well3[, WELL_NAME:=well_data[[1]][2]]
well3[, UWI:=well_data[[1]][3]]
well3[, type:='INVERT']

wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-E79-H_94-B-09/data"
setwd(wd)
doc<- xmlTreeParse(file = 'ETS_0ZG2_0010_2017_03_DD_1.xml', useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file='1488862200.csv', header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])  
spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]

pason<-as.data.table(pason)
pason<-pason[Hole.Depth..meters.>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]

pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
setnames(pason, old='Hole.Depth..meters.',new='Hole.Depth')

well4<-pason[,c('Hole.Depth','days_from_spud')]
well4[, WELL_NAME:=well_data[[1]][2]]
well4[, UWI:=well_data[[1]][3]]
well4[, type:='INVERT']


wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-F79-H_94-B-09/data"
setwd(wd)
doc<- xmlTreeParse(file = 'ETS_0ZG2_0010_2017_MM_DD_1.xml', useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file='1487041800.csv', header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])  
spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]

pason<-as.data.table(pason)
pason<-pason[Hole.Depth..meters.>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]

pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
setnames(pason, old='Hole.Depth..meters.',new='Hole.Depth')

well5<-pason[,c('Hole.Depth','days_from_spud')]
well5[, WELL_NAME:=well_data[[1]][2]]
well5[, UWI:=well_data[[1]][3]]
well5[, type:='INVERT']

wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-G79-H_94-B-09/data"
setwd(wd)
doc<- xmlTreeParse(file = 'ETS_0ZG2_0010_2017_MM_DD_1.xml', useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file='1485486600.csv', header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])  
spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]

pason<-as.data.table(pason)
pason<-pason[Hole.Depth..meters.>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]

pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
setnames(pason, old='Hole.Depth..meters.',new='Hole.Depth')

well6<-pason[,c('Hole.Depth','days_from_spud')]
well6[, WELL_NAME:=well_data[[1]][2]]
well6[, UWI:=well_data[[1]][3]]
well6[, type:='INVERT']

wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-H79-H_94-B-09 (Brine)/data"
setwd(wd)
doc<- xmlTreeParse(file = 'ETS_0WA4_0040_2018_MM_DD_1.xml', useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file='1535687298.csv', header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])  
spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]

pason<-as.data.table(pason)
pason<-pason[Hole.Depth..meters.>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]

pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
setnames(pason, old='Hole.Depth..meters.',new='Hole.Depth')

well7<-pason[,c('Hole.Depth','days_from_spud')]
well7[, WELL_NAME:=well_data[[1]][2]]
well7[, UWI:=well_data[[1]][3]]
well7[, type:='BRINE']

wd<- "G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/b-I79-H_94-B-09 (Brine)/data"
setwd(wd)
doc<- xmlTreeParse(file = 'ETS_0WA4_0040_2018_MM_DD_1.xml', useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file='1537066959.csv', header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])  
spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]

pason<-as.data.table(pason)
pason<-pason[Hole.Depth..meters.>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]

pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
setnames(pason, old='Hole.Depth..meters.',new='Hole.Depth')

well8<-pason[,c('Hole.Depth','days_from_spud')]
well8[, WELL_NAME:=well_data[[1]][2]]
well8[, UWI:=well_data[[1]][3]]
well8[, type:='BRINE']





well_final<-rbindlist(list(well1,well2,well3,well4,well5,well6, well7, well8))
well_final[, Legend:=paste(UWI, type)]

c_pal<-c('black','#12BC1A','#74FD7B','#132B03','#18FB23','#063F09','#11ED00','#0C7E12','#12BC1A')
         #'#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         #'#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
          #'#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B'


#by brine or invert
p1<- ggplot(data=well_final, aes(x=days_from_spud, y=Hole.Depth, color=factor(type))) + 
  geom_point(size = 0.5)+
  theme_bw() + 
  scale_y_reverse(breaks=seq(0,5000,250))+
  scale_x_continuous(breaks=seq(0,25,2.5))+
  scale_color_manual(name='WELL', values = c_pal)+
  ggtitle('Painted Pony HZ Townsend')+
  ylab('Hole_Depth_m')+
  theme(plot.title=element_text(family ='Neutra Text SC', size = 12))+
  theme(axis.text.x = element_text(family = 'Neutra Text SC', size = 12, hjust = 1))+
  theme(axis.text.y = element_text(family = 'Neutra Text SC', size = 12))+
  theme(axis.title = element_text(family='Neutra Text SC', size = 14))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size = 8, family ='Neutra Text SC'))+
  theme(legend.position = 'bottom')+
  guides(colour = guide_legend(override.aes = list(size=2)))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.border = element_rect(color='grey'))+
  theme(axis.ticks.x = element_line(colour = 'grey'))+
  theme(axis.ticks.y = element_line(colour = 'grey'))
 

p2<- ggplot(data=well_final, aes(x=days_from_spud, y=Hole.Depth, color=factor(UWI), linetype=type)) + 
  geom_line(size = 1.25)+
  theme_bw() + 
  scale_y_reverse(breaks=seq(0,5000,250))+
  scale_x_continuous(breaks=seq(0,25,2.5))+
  ggtitle('Painted Pony HZ Townsend')+
  ylab('Hole_Depth_m')+
  theme(plot.title=element_text(family ='Neutra Text SC', size = 12))+
  theme(axis.text.x = element_text(family = 'Neutra Text SC', size = 12, hjust = 1))+
  theme(axis.text.y = element_text(family = 'Neutra Text SC', size = 12))+
  theme(axis.title = element_text(family='Neutra Text SC', size = 14))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size = 8, family ='Neutra Text SC'))+
  theme(legend.position = 'bottom')+
  guides(colour = guide_legend(override.aes = list(size=2)))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.border = element_rect(color='grey'))+
  theme(axis.ticks.x = element_line(colour = 'grey'))+
  theme(axis.ticks.y = element_line(colour = 'grey'))

setwd(out_wd)
cairo_pdf(paste(Sys.Date(), 'All_WELL_by_TYPE.pdf'), width = 12)
p1
dev.off()

cairo_pdf(paste(Sys.Date(), 'All_WELL_Individual.pdf'), width = 12)
p2
dev.off()
