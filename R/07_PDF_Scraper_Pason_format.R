library(data.table)
library(tabulizer)
library(pdftools)
library(reshape2)
library(tidyr)
library(ggplot2)

#locate_areas('double page.pdf')
#setwd("~/R/01_Projects_R/Project_Recap/functions")

time_codes<-data.table(read.csv(file='time_codes.csv'))

a1<-list(c(155.5714, 390.0714, 289.1429, 602.2143  ),
         c(378.7143, 391.6429, 506.0000, 602.2143 ),
         c(589.2857, 390.0714, 727.5714, 603.7857 ))
a2<-list(c( 11.0000, 360.2143,  22.0000, 413.6429 )) 

setwd("C:/Users/VHa/Desktop")
file<- 'double page.pdf'
length<-length(extract_tables(file))

x<-1:length
table<-data.table()

for(i in x){
  time_log<-extract_tables(file, pages = c(i,i,i), area = a1, 
                 guess = FALSE)
  date<-extract_tables(file, pages = c(i), area = a2, 
                       guess = FALSE)
  
    col1<- data.frame(date[[1]])
    col1<- paste0(col1$X1,'-',col1$X2,'-', col1$X3)
    col2<- data.frame(time_log[[1]])
    col3<- data.frame(time_log[[2]])
    col4<- data.frame(time_log[[3]]) 
    col<-rbind(col2,col3,col4)
    col<-as.data.table(col)
    col[,Date:=col1]
    table<-rbind(table,col)
    
  
}

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
