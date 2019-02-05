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

setwd("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Project_Recap")
source('functions/pason_format.R')
source('functions/fluid_property.R')
source('functions/well_information.R')
source('functions/casing_data_xml.R')
source('functions/bit_data_xml.R')
source('functions/time_log_xml.R')


directory<-'c-D6-F-94-B-16'
pason_file<-'1457954865condensed'
xml_path<-'ETS_0WA4_0040_2016_MM_DD_1'


wd<- paste0("G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/",directory,"/data/")
out_wd<- paste0("G:/My Drive/$Technical$ New/Technical/Project Recaps & Cost Tracking/Painted Pony 2018-2019/PASON Data/",directory)
setwd(wd)
doc<- xmlTreeParse(file = paste0(xml_path,'.xml'), useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file=paste0(pason_file,'.csv'), header = TRUE, stringsAsFactors = FALSE))
well_data<-well_info()
well_table<- data.table(well_data[[1]])
well_table[, spud_date:=as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')]
well_table[, rig_release:=as.POSIXct(rig_release, format ='%Y-%m-%d %H:%M')]

spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rig_release<-well_data[[1]][5]
rig_release<-as.POSIXct(rig_release, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]
pason<- pason_data() #accesses function
fluid<-fluid_property()
casing<-casing_info()
bit_info<-bit_info()
time_log<-time_codes()
fluid<-fluid[Density<5000,]

pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
pason<-pason[Date>=spud_date & Date<=rig_release,]
pason<-pason[!Hole.Depth..meters.=='9999',]

p1 %<a-% {par(mar = c(3,5,4,5))
  with(pason, plot(days_from_spud, Hole.Depth..meters., axes = T, col="black", family='Neutra Text SC',cex = 0.25, #type='l', lwd=0.1 # for line graph
                   panel.first = grid(col='black'),yaxt = 'n',xaxt = 'n',#main='TITLE'
                   #xlab='Days from spud',
                   ylab='Depth (m)',
                   ylim=rev(range(Hole.Depth..meters.))))
  axis(side = 2, las = 2, family='Neutra Text SC',at=c(seq(from=0,to=6000,by=200)))
  axis(side = 1, family ='Neutra Text SC',at=c(seq(from=0,to=100,by=1)))#,cex.axis=1.2,)
  mtext(side = 1, 'Days from Spud', family='Neutra Text SC', col='black', line = 2)
  par(new=TRUE)
  with(fluid[Density>0,], plot(days_from_spud,Density, type='l', lwd = 2, axes = F, ylab=NA, xlab=NA, cex=1.2, col='#12BC1A', pch=16))
  axis(side = 4, family ='Neutra Text SC', col='#12BC1A', col.axis='#12BC1A', las = 2,
       at=c(seq(from=900, to=3000, by=20)))#,cex.axis=1.2,)
  mtext(side = 4, line=3, 'Mud Density (kg/m3)', family='Neutra Text SC', col='#12BC1A')
  mtext(side = 3, well_data[[1]][2], family='Neutra Text SC', col='black', line = 0.1, cex =1.25, adj =0)
}

setwd(out_wd)

cairo_pdf(paste(Sys.Date(), 'Depth_vs_Days.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
p1
dev.off()







