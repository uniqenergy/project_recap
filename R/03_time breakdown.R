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

rm(list=ls(all=TRUE))
setwd("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Project_Recap")
source('functions/fluid_property.R')
source('functions/well_information.R')
source('functions/casing_data_xml.R')
source('functions/bit_data_xml.R')
source('functions/time_log_xml.R')

directory<-'100_04-22-073-24W400 SPL 8-20 HZ SMITH 4-22-73-24'
wd<- paste0("C:/Users/VHa/Desktop/Spur/",directory,"/data/")
out_wd<-paste0("C:/Users/VHa/Desktop/Spur/",directory)
setwd(wd)

xml_path<-list.files(pattern="*xml")

doc<- xmlTreeParse(file = paste0(xml_path), useInternal = TRUE)
rootNode<-xmlRoot(doc)
well_data<-well_info()
well_table<- data.table(well_data[[1]])

spud_date<- well_data[[1]][4]
spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rounded_days<- well_data[[3]][1]
#fluid<-fluid_property()
#casing<-casing_info()
bit_info<-bit_info()
time_log<-time_codes()

c_pal<-c('black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B')

time_log_list<-data.table(unique(time_log[, list(Code, Activity)]))
time_log_activity<-data.table(unique(time_log[, list(Code, TimeCodeNo,Activity,Definition)]))

time_log_1<-time_log[, list(total_hrs=sum(Elapsed_hrs, na.rm=FALSE)), by=c('Code')]
time_log_1<-merge(time_log_1,time_log_list, by.x=c('Code'), by.y=c('Code'), all.x=TRUE)
time_log_1[, label:=paste0(Activity, ": ",total_hrs, " hrs")]

time_log_2<-time_log[, list(total_hrs=sum(Elapsed_hrs, na.rm=FALSE)), by=c('TimeCodeNo')]
time_log_2<-merge(time_log_2,time_log_activity,by.x=c('TimeCodeNo'), by.y=c('TimeCodeNo'), all.x=TRUE)
time_log_2[, Definition:=gsub('Drill cement/drill out cement/drill float&shoe','Drill cement/float&shoe', Definition)]
time_log_2[, label:=paste0(Definition, ": ",total_hrs, " hrs")]

setorder(time_log_1, Code)
setorderv(time_log_2, c('Code','TimeCodeNo'))

time_log_1 <- time_log_1 %>% 
  mutate(end = 2 * pi * cumsum(total_hrs)/sum(total_hrs),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

time_log_2 <- time_log_2 %>% 
  mutate(end = 2 * pi * cumsum(total_hrs)/sum(total_hrs),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

#graphs
wellname<-well_data[[1]][2]
wellname<-gsub("/","_",wellname)

pie_t<- ggplot(time_log_1) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 0.9,
                   start = start, end = end, fill = as.factor(total_hrs))) +
  geom_text(aes(x = 1 * sin(middle), y = 1 * cos(middle), label = label,
                hjust = hjust, vjust = 1), size = 3.5, check_overlap = TRUE, family='Neutra Text SC')  +
  coord_fixed() +
  theme_bw()+
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)+
  scale_fill_manual(values = c_pal)+
  theme(legend.position = 'none')+ # removes legend+
  theme(panel.border = element_blank())+ #removes borders
  ggtitle(paste('Total Activity Breakdown',well_data[[1]][2])) +
  theme(plot.title=element_text(hjust=0.5, family ='Neutra Text SC', size = 16))+
  labs(caption='**Please note: Small Time data sets that have been clustered have been excluded for readability purposes')+
  theme(plot.caption =element_text(family = 'Neutra Text SC', hjust = 0.5))


pie_t2<- ggplot(time_log_2) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 0.9,
                   start = start, end = end, fill = as.factor(total_hrs))) +
  geom_text(aes(x = 1 * sin(middle), y = 1 * cos(middle), label = label,
                hjust = hjust, vjust = 1), size = 3.5, check_overlap = TRUE, family='Neutra Text SC')  +
  coord_fixed() +
  theme_bw()+
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)+
  scale_fill_manual(values = c_pal)+
  theme(legend.position = 'none')+ # removes legend+
  theme(panel.border = element_blank())+ #removes borders
  ggtitle(paste('Total Activity Breakdown',well_data[[1]][2])) +
  theme(plot.title=element_text(hjust=0.5, family ='Neutra Text SC', size = 16))+
  labs(caption='**Please note: Small Time data sets that have been clustered have been excluded for readability purposes')+
  theme(plot.caption =element_text(family = 'Neutra Text SC', hjust = 0.5))


bar_t<- ggplot(data=time_log_1, aes(x=Activity, y=total_hrs, fill=as.factor(Code))) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=total_hrs), vjust = -0.25, size = 4, family ='Neutra Text SC')+#, position=position_dodge(width=0.9), vjust=-0.25)+
  theme_bw()+
  scale_fill_manual(values = c_pal)+
  ggtitle(paste('Total Activity Breakdown',well_data[[1]][2])) +
  theme(plot.title=element_text(family ='Neutra Text SC', size = 16))+
  theme(axis.text.x = element_text(angle = 90, family = 'Neutra Text SC', size = 12, hjust = 1))+
  theme(axis.text.y = element_text(family = 'Neutra Text SC', size = 16))+
  theme(legend.position = 'none')+ # removes legend+
  theme(axis.title = element_text(family='Neutra Text SC', size = 16))+
  xlab('')


setwd(out_wd)

cairo_pdf(paste(wellname, 'Time_breakdown_pie_chart.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
pie_t
dev.off()

cairo_pdf(paste(wellname, 'Time_breakdown_bar_chart.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
bar_t
dev.off()

cairo_pdf(paste(wellname, 'Time_breakdown_pie_chart_high_detail.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
pie_t2
dev.off()

bar_t
