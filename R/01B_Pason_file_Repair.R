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

rm(list=ls(all=TRUE))

#directional_path<-'survey_1526663340'
directory<-'200_a-042-J_094-B-09_00 (PPY, c-G20-I94-B-16, Townsend, Montney HZ)'
wd<- paste0('C:/Users/VHa/Desktop/Painted Pony-Confidential/',directory,'/Recaps/data/')
setwd(wd)
pason_file<-list.files(pattern="*condensed.csv")
pason<-data.table(read.csv(file=paste0(pason_file), header = TRUE, stringsAsFactors = FALSE))
pason2<-pason[,c('YYYY.MM.DD','HH.MM.SS','Hole.Depth..meters.')]
pason2[, date:=as.POSIXct(paste(YYYY.MM.DD, HH.MM.SS), format = '%Y/%m/%d %H:%M')]
pason2

#pason<-pason[!(1:330),]
#write.csv(pason, file = pason_file, row.names = FALSE)

ggplot(data=pason2, aes(x=date, y=Hole.Depth..meters.)) + 
  geom_line()+#, linetype = 'dashed') +#this is for dashed
  theme_bw() + 
  scale_y_reverse()
#View(pason2)








