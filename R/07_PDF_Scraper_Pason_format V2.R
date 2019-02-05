library(data.table)
library(tabulizer)
library(pdftools)
library(reshape2)
library(tidyr)
library(ggplot2)
library(gtools)
library(plyr)
library(dplyr)

locate_areas('single page.pdf')
#setwd("~/R/01_Projects_R/Project_Recap/functions")

#time_codes<-data.table(read.csv(file='time_codes.csv'))


         
a1<-list(c(132.3571, 457.2500, 161.5000, 507.0357  ),
         c(297.5000, 458.4643, 325.4286, 507.0357 ),
         c(461.4286, 457.2500, 490.5714, 507.0357 )) #main table 1

a2<-list(c(26.71429, 210.74997,  36.42857, 252.03569))# date set

setwd("C:/Users/VHa/Desktop")
file<- '102_08-28-048-11W5_00_Vermillion_Pembina_Hz_Ellerslie_2018-08-07_Monobore_159mm.pdf'
length<-32
#length<-length(extract_tables(file))

x<-1:length
j<-1:3
table<-data.table(matrix(nrow = 0, ncol = 6))



for(i in x){
  time_log<-extract_tables(file, pages = c(i,i,i), area = a1, 
                           guess = FALSE)
  time_log<-ldply (time_log, data.table)
  time_log<-as.data.table(time_log)
  
  date<-extract_tables(file, pages = c(i), area = a2, 
                       guess = FALSE)
  
  date<- data.frame(date[[1]])
  date<- paste0(date$X1,'-',date$X2,'-', date$X3)
  time_log[, V6:=date]
  
  table<-smartbind(table,time_log)
  
  
}
table<-as.data.table(table)
#table<-table[!(is.na(V1) & is.na(V2)),]
#table<-table[!is.na(V4),]
table<-separate(data=table, col = V3, into = c('V7','V8'), sep='\\s')



table<-separate(table,col = "X3",into = c("Time", "Code"), sep = " ")
setnames(table, c('From','To','Hours','Code','Details','Date'))
table<-table[,c('Date','From','To','Hours','Code','Details')]
table[, Date:=as.Date(Date, format ='%Y-%m-%d')]
table[, From:=as.character(From)]
table[, To:=as.character(To)]
table[, Hours:=as.numeric(Hours)]
table[, Code:=as.character(Code)]
table[, Details:=as.character(Details)]
table<- merge(table, time_codes[,c('Value','Activity','Code')], by.x=c('Code'), by.y=c('Value'), all.x = TRUE)

time_log_1<-table[, list(total_hrs=sum(Hours, na.rm=TRUE)), by=c('Code.y','Activity')]
time_log_1<-time_log_1[total_hrs>0,]

c_pal<-c('black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B')

bar_t<- ggplot(data=time_log_1, aes(x=Activity, y=total_hrs, fill=as.factor(Code.y))) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=total_hrs), vjust = -0.25, size = 4, family ='Neutra Text SC')+#, position=position_dodge(width=0.9), vjust=-0.25)+
  theme_bw()+
  scale_fill_manual(values = c_pal)+
  ggtitle(paste('Total Activity Breakdown')) +
  theme(plot.title=element_text(family ='Neutra Text SC', size = 16))+
  theme(axis.text.x = element_text(angle = 90, family = 'Neutra Text SC', size = 12, hjust = 1))+
  theme(axis.text.y = element_text(family = 'Neutra Text SC', size = 16))+
  theme(legend.position = 'none')+ # removes legend+
  theme(axis.title = element_text(family='Neutra Text SC', size = 16))+
  xlab('')

