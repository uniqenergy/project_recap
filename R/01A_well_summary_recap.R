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

drive<-'G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Devon_Weekly_Report_Strat/data' #paste drive location here and change backslashes
directory<-'1AB_16-23-075-07W400 DEVON 16B CONKLIN' #paste well location
#MR LORD! Don't touch! Think of this as your penis!#######################################################
#################
#################
#################

wd<-paste0(drive,'/',directory,'/data') #this may require a Recap to go in front '/Recaps/data
out_wd<-paste0(drive,'/',directory)

setwd("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Project_Recap/")
#setwd("~/R/01_Projects_R/Project_Recap")
source('functions/pason_format.R')
source('functions/fluid_property.R')
source('functions/well_information.R')
source('functions/casing_data_xml.R')
source('functions/bit_data_xml.R')
source('functions/time_log_xml.R')

#directional_path<-'survey_1526663340'
#directory<-'102_06-32-062-06W400 (Devon, 6B Bnnyville)'
#wd<- paste0('G:/My Drive/$Technical$ New/Technical/Well Files/W4/062-06/',directory,'/data')
#out_wd<-  paste0('G:/My Drive/$Technical$ New/Technical/Well Files/W4/062-06/',directory)
setwd(wd)

pason_file<-list.files(pattern="*condensed.csv")
xml_path<-list.files(pattern="*xml")

doc<- xmlTreeParse(file = paste0(xml_path), useInternal = TRUE)
rootNode<-xmlRoot(doc)
pason<-data.table(read.csv(file=paste0(pason_file), header = TRUE, stringsAsFactors = FALSE))

well_data<-well_info()
well_table<- data.table(well_data[[1]])
well_table[, spud_date:=as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')]
well_table[, rig_release:=as.POSIXct(rig_release, format ='%Y-%m-%d %H:%M')]
#MBT<- data.table(read.xlsx('Inventory.xlsx', 
#                           sheetName='Report Export', header = TRUE,
#                           colClasses = NA, stringsAsFactors=FALSE))

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
bit_info<-bit_info[Size >0,]
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
pason<-pason[Date>=spud_date & Date<=rig_release,]
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]

#fluid<-fluid[Density>900 & Density < 1600,]
#MBT and Amine 

#MBT1<-dplyr::filter(MBT, grepl('Methylene', Param..Reports))
#MBT2<-MBT[Param..Reports=='Depth M.D. (m)',]
#MBT1<-melt(MBT1, id.vars = 'Param..Reports')
#setnames(MBT1, c('Category','Report','Concentration'))
#MBT2<-melt(MBT2, id.vars = 'Param..Reports')
#MBT2[,Param..Reports:=NULL]
#setnames(MBT2, c('Report','Depth'))
#MBT_summary<- data.table(merge(MBT1, MBT2, by.x=c('Report'), by.y=c('Report'), all.x=TRUE))
#MBT_summary[, Concentration:=as.numeric(Concentration)]
#MBT_summary[, Depth:=as.numeric(Depth)]



#depth vs days and mud density


#pason<-pason[!Hole.Depth..meters.=='9999',]

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


#MBt plot
#MBT_Graph<- ggplot()+
#  geom_line(data=MBT_summary, aes(x=Depth, y=Concentration), color='#12BC1A', size = 1) +
#  theme_bw()+
#  ggtitle(paste('MBT Concentration',well_data[[1]][2]))+
#  scale_y_continuous(breaks=seq(0,100,5))+
#  scale_x_continuous(breaks=seq(0,4000,250))+
#  theme(plot.title=element_text(family ='Neutra Text SC', size = 16, hjust = 0.5))+
#  theme(axis.text.x = element_text(family = 'Neutra Text SC', size = 14, hjust = 1))+
#  theme(axis.ticks = element_blank())+
#  theme(axis.text.y = element_text(family = 'Neutra Text SC', size = 14))+
#  theme(axis.ticks.y=element_blank())+
#  theme(legend.position = 'none')+ # removes legend+
#  theme(axis.title = element_text(family='Neutra Text SC', size = 16))+
#  xlab('')+
#  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
#    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
#    panel.border = element_rect(color='grey'))+
#  labs(caption=well_data[[1]][2])+
#     theme(plot.caption =element_text(family = 'Neutra Text SC'))
   



casing_depths<-unique(casing[, c('Category','Grade','OutsideDiameter','KBToCasingBottom','LinearMass')])
casing_depths<-casing_depths[!Category=='TUBING',]
casing_depths[, OutsideDiameter:=as.numeric(OutsideDiameter)]
casing_depths[, KBToCasingBottom:=as.numeric(KBToCasingBottom)]
casing_depths[,rounded_depth:=round(KBToCasingBottom, digits = 0)]


Depths<- bit_info[, list(Bit_hours = max(CumulativeHoursRun, na.rm = TRUE), Depth_drilled=max(DepthOut, na.rm = TRUE)+0.5), by=c('Size')]
Depths$meters_drilled <- as.numeric(ave(Depths$Depth_drilled, FUN=function(x) c(casing_depths$KBToCasingBottom[[1]], diff(x)))) #this is for surface and intermediate casing
#Depths$meters_drilled <- Depths$Depth_drilled - casing_depths$KBToCasingBottom[[1]]  #this is for no surface casing but intermediate
#Depths$meters_drilled <- Depths$Depth_drilled -84  #this is for single bit drill. #no surface or intermediate casing


pason[,rounded_depth:=round(Hole.Depth..meters., digits = 0)]

#calculation for last entry bit depth
  for(i in 1:nrow(Depths)) {
    section_days<-copy(pason)
    section_days$depth<- Depths[i,Depth_drilled]
    section_days[, depth_difference:=abs(depth - rounded_depth)]
    min_value<-min(section_days$depth_difference)
    Depths[i, deepest_depth_date:= section_days[depth_difference==min_value, max(Date)]]
    
    }
#Days Drilled
Depths$diff_from_spud<- as.numeric(Depths$deepest_depth_date) - as.numeric(spud_date)
Depths$time_drilled_days <- ave(Depths$diff_from_spud, FUN=function(x) c(Depths$diff_from_spud[[1]], diff(x)))/86400
Depths[, All_in_ROP_m_hr:= meters_drilled / time_drilled_days /24]

setDT(bit_info)[, bit_count := uniqueN(SerialNo, na.rm = TRUE), by = Size] # of bits
bit_count<- unique(bit_info[, list(Size, bit_count)])
Depths<- merge(Depths, bit_count, by.x=c('Size'), by.y=c('Size'), all.x=TRUE)
Depths[, m_bit:=meters_drilled/bit_count]
Depths<- Depths[, c('Size','Bit_hours','Depth_drilled','meters_drilled','time_drilled_days','All_in_ROP_m_hr','bit_count','m_bit')]
Depths[,time_drilled_days:=round(time_drilled_days, digits = 2)]
Depths[,All_in_ROP_m_hr:=round(All_in_ROP_m_hr, digits = 2)]
Depths[,Rounded_depth:=round(Depth_drilled, digits = 0)]

setorderv(Depths, c('Depth_drilled'))


#well_info
sequence<- data.table(Depth_m = seq(from = 0, to=max(Depths$Rounded_depth), by=1))
sequence<- merge(sequence, Depths[, c('Size','Rounded_depth')], by.x=c('Depth_m'), by.y=c('Rounded_depth'), all.x=TRUE)
sequence[, Size:=na.locf(Size, fromLast = TRUE)]

sequence2<-data.table(Depth_m = seq(from = 0, to=max(casing_depths$rounded_depth), by=1))
sequence2<- merge(sequence2, casing_depths, by.x=c('Depth_m'), by.y=c('rounded_depth'), all.x=TRUE)
sequence2[, Category:=na.locf(Category, fromLast = TRUE)]
sequence2[, Grade:=na.locf(Grade, fromLast = TRUE)]
sequence2[, OutsideDiameter:=na.locf(OutsideDiameter, fromLast = TRUE)]
sequence2[, KBToCasingBottom:=na.locf(KBToCasingBottom, fromLast = TRUE)]
sequence2[, LinearMass:=na.locf(LinearMass, fromLast = TRUE)]
sequence<- merge(sequence, sequence2, by.x=c('Depth_m'), by.y=c('Depth_m'),all.x = TRUE)

#fluid_recap
fluid_2<- merge(pason[, c('Date','Hole.Depth..meters.','rounded_depth')], fluid, by.x=c('Date'), by.y=c('Date'), all.x = TRUE)
fluid_2<- fluid_2[!is.na(Density) | !is.na(FunnelViscosity),]
fluid_2[, final_depth:=ifelse(is.na(Depth), rounded_depth, Depth)]
fluid_2[, Depth:=NULL]
fluid_2[, Hole.Depth..meters.:=NULL]
#fluid_2<- fluid_2[, c('Date','final_depth','Density','FunnelViscosity','FluidPh','Location','PVT','WaterLoss')]
fluid_2<-merge(fluid_2, sequence, by.x=c('final_depth'), by.y=c('Depth_m'), all.x = TRUE)

fluid_summary<-fluid_2[, list(max_density = max(Density, na.rm = TRUE), 
                             min_density=min(Density, na.rm = TRUE),
                             Avg_density = mean(Density, na.rm = TRUE),
                             Avg_Viscosity = mean(FunnelViscosity, na.rm=TRUE)), by=c('Size')]
fluid_summary<-fluid_summary[!is.na(Size),]

#concatenation

final_summary<- merge(fluid_summary, Depths[,c('Size','Depth_drilled','meters_drilled','time_drilled_days','All_in_ROP_m_hr','bit_count','m_bit')], 
      by.x=c('Size'), by.y=c('Size') )
setorderv(final_summary, c('Size'), order = -1)

final_summary[,max_density:=round(max_density, digits = 0)]
final_summary[,min_density:=round(min_density, digits = 0)]
final_summary[,Avg_density:=round(Avg_density, digits = 0)]
final_summary[,Avg_Viscosity:=round(Avg_Viscosity, digits = 0)]

#printing commands

setwd(out_wd)
well_name<-well_data[[1]][2]
well_name<-gsub("/","_",well_name)
#cairo_pdf(paste(Sys.Date(), well_data[[1]][2], 'Depth_vs_Days.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
cairo_pdf(paste(well_name, 'Depth_vs_Days.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
p1
dev.off()

#cairo_pdf(paste(Sys.Date(), well_data[[1]][2], 'MBT_Data.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
#MBT_Graph
#dev.off()
well_table[,spud_date:=as.character(spud_date)]
well_table[,rig_release:=as.character(rig_release)]
fluid[, Date:=as.character(Date)]
time_log[, Date:=as.character(Date)]
time_log[, FromTime:=as.character(FromTime)]
time_log[, ToTime:=as.character(ToTime)]


write.xlsx(final_summary, file=paste(well_name,'PASON_SUMMARY.xlsx'), sheetName="TO_PASTE", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE, password=NULL)


write.xlsx(well_table, file=paste(well_name,'PASON_SUMMARY.xlsx'), sheetName="Well_Information", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(Depths, file=paste(well_name,'PASON_SUMMARY.xlsx'), sheetName="bit_data", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(casing_depths, file=paste(well_name,'PASON_SUMMARY.xlsx'), sheetName="casing_data", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(fluid_summary, file=paste(well_name,'PASON_SUMMARY.xlsx'), sheetName="Fluid_Property", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

#PASON DETAILED RECAP
write.xlsx(fluid, file=paste(well_name,'PASON_DATA_BACKUP.xlsx'), sheetName="Fluid_Property", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE, password=NULL)

write.xlsx(bit_info, file=paste(well_name,'PASON_DATA_BACKUP.xlsx'), sheetName="Bit_Information", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(casing, file=paste(well_name,'PASON_DATA_BACKUP.xlsx'), sheetName="Casing_Information", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(time_log, file=paste(well_name,'PASON_DATA_BACKUP.xlsx'), sheetName="Time_log", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)









