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
setwd("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Project_Recap")
source('functions/pason_format.R')
source('functions/fluid_property.R')
source('functions/well_information.R')
source('functions/casing_data_xml.R')
source('functions/bit_data_xml.R')
source('functions/time_log_xml.R')

#directional_path<-'survey_1526663340'

directory<-'100_04-25-064-07W400 Devon 4B HZ WOLF'
wd<- paste0("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Shiny/DEVON/data/",directory,"/data/")
out_wd<-paste0("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Shiny/DEVON/data/",directory)
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
#bit_info[ , c('DepthOut')][is.na(bit_info[ , c('DepthOut')]) ] = 1822.03 #accomdates if bit entry not complete

#depth vs days and mud density
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
pason<-pason[Date>=spud_date & Date<=rig_release,]
pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]
fluid<-fluid[Density>900,]


p1 %<a-% {par(mar = c(3,5,4,5))
  with(pason, plot(days_from_spud, Hole.Depth..meters., axes = T, col="black", family='Neutra Text SC',cex = 0.25, #type='l', lwd=0.1 # for line graph
                   panel.first = grid(col='black'),yaxt = 'n',xaxt = 'n',#main='TITLE'
                   #xlab='Days from spud',
                   ylab='Depth (m)',
                   ylim=rev(range(Hole.Depth..meters.))))
  axis(side = 2, las = 2, family='Neutra Text SC',at=c(seq(from=0,to=5000,by=100)))
  axis(side = 1, family ='Neutra Text SC',at=c(seq(from=0,to=100,by=1)))#,cex.axis=1.2,)
  mtext(side = 1, 'Days from Spud', family='Neutra Text SC', col='black', line = 2)
  par(new=TRUE)
  with(fluid[Density>0,], plot(days_from_spud,Density, type='l', lwd = 2, axes = F, ylab=NA, xlab=NA, cex=1.2, col='#12BC1A', pch=16))
  axis(side = 4, family ='Neutra Text SC', col='#12BC1A', col.axis='#12BC1A', las = 2,
       at=c(seq(from=900, to=3000, by=20)))#,cex.axis=1.2,)
  mtext(side = 4, line=3, 'Mud Density (kg/m3)', family='Neutra Text SC', col='#12BC1A')
  mtext(side = 3, well_data[[1]][2], family='Neutra Text SC', col='black', line = 0.1, cex =1.25, adj =0)
}

pason2<-copy(pason)
pason2[,rounded_depth:=round(Hole.Depth..meters., digits = 0)]
pason2<-pason2[,c('Date','days_from_spud','rounded_depth','Hole.Depth..meters.')]
pason2[, UWI:=well_data[[1]][[3]]]
pason2[, well_name:=well_data[[1]][[2]]]
setnames(pason2,old=('Hole.Depth..meters.'), new=c('hole_depth'))

casing_depths<-unique(casing[, c('Category','Grade','OutsideDiameter','KBToCasingBottom','LinearMass')])
casing_depths<-casing_depths[!Category=='TUBING',]
casing_depths[, OutsideDiameter:=as.numeric(OutsideDiameter)]
casing_depths[, KBToCasingBottom:=as.numeric(KBToCasingBottom)]
casing_depths[,rounded_depth:=round((KBToCasingBottom+0.5), digits = 0)]
ICP<-casing_depths[Category=='INTERMEDIATE',max(KBToCasingBottom)]

bit_hours<- unique(bit_info[, list(Date,BitNo,Size,SerialNo,DepthIn,DepthOut, HoursRunToday,CumulativeHoursRun,TourId)])
bit_hours[!is.na(HoursRunToday), total_hours:= cumsum(as.numeric(HoursRunToday)), by=c('Size')]
bit_hours<-bit_hours[, list(bit_hours=max(total_hours, na.rm = TRUE)), by='Size']

Depths<- bit_info[, list(Max_Depth_drilled=max(DepthOut, na.rm = TRUE)), by=c('Size')]
Depths<-merge(Depths, bit_hours, by=c('Size'), all.x = TRUE)
setorderv(Depths, c('Size'), order = -1)
Depths$meters_drilled <- as.numeric(ave(Depths$Max_Depth_drilled, FUN=function(x) c(casing_depths$KBToCasingBottom[[1]], diff(x))))

#this is for if there was conductor or first depth isn't until its into the wellbore
#Depths$meters_drilled <- as.numeric(ave(Depths$Max_Depth_drilled, FUN=function(x) c(casing_depths$KBToCasingBottom[[2]]-casing_depths$KBToCasingBottom[[1]], diff(x))))


#mutlilateral accounting
meterage<-pason[,c('Date','days_from_spud','Hole.Depth..meters.')]
setnames(meterage, old='Hole.Depth..meters.',new='depth')
meterage[, days_from_spud:=round(days_from_spud, digits = 1)]
meterage[, difference:= as.numeric(ave(meterage$depth, FUN=function(x) c(0, diff(x))))]
meterage[, difference:=difference + 100]
meterage<- data.table(mutate(meterage, previous = lag(depth)))
#last leg
last_leg<-tail(meterage[!is.na(depth) & depth > ICP,],1)
meterage2<-meterage[difference<0,]
meterage2<-rbindlist(list(meterage2, last_leg))
meterage2<-data.table(mutate(meterage2, alignment=lag(depth)))
meterage2[1,'alignment']<-ICP
meterage2<-meterage2[,c('Date','days_from_spud','alignment','previous')]
#meterage2<-meterage2[!2,] exclude legs here
#meterage2[1, previous:='2026']
setnames(meterage2, c('Date','days_from_spud','intermediate','TD'))
meterage2[, meterage:=TD-intermediate]
meterage2[, Total_meterage:=sum(meterage)]
meterage2[, Number_of_Legs:= unique(meterage2[, length(meterage)])]
Depths[nrow(Depths),'meters_drilled']<-unique(meterage2[, Total_meterage])
Depths[, rounded_depth:=round(Max_Depth_drilled, digits = 0)]

#calculation for last entry bit depth
pason[,rounded_depth:=round(Hole.Depth..meters., digits = 0)]
for(i in 1:nrow(Depths)) {
  section_days<-copy(pason)
  section_days$depth<- Depths[i,Max_Depth_drilled]
  section_days[, depth_difference:=abs(depth - rounded_depth)]
  min_value<-min(section_days$depth_difference)
  Depths[i, deepest_depth_date:= section_days[depth_difference==min_value, max(Date)]]
  Depths[i, days_from_spud:= section_days[depth_difference==min_value, max(days_from_spud)]]
}
Depths[nrow(Depths),'days_from_spud']<-meterage2[, max(days_from_spud)]
Depths$time_drilled_days <- ave(Depths$days_from_spud, FUN=function(x) c(Depths$days_from_spud[[1]], diff(x)))
Depths[, All_in_ROP_m_hr:= meters_drilled / time_drilled_days /24]


setDT(bit_info)[, bit_count := uniqueN(SerialNo, na.rm = TRUE), by = Size] # of bits
bit_count<- unique(bit_info[, list(Size, bit_count)])
Depths<- merge(Depths, bit_count, by.x=c('Size'), by.y=c('Size'), all.x=TRUE)
Depths[, m_bit:=meters_drilled/bit_count]
Depths<- Depths[, c('Size','bit_hours','Max_Depth_drilled','meters_drilled','time_drilled_days','All_in_ROP_m_hr','bit_count','m_bit')]
Depths[,time_drilled_days:=round(time_drilled_days, digits = 2)]
Depths[,All_in_ROP_m_hr:=round(All_in_ROP_m_hr, digits = 2)]
Depths[,Rounded_depth:=round(Max_Depth_drilled, digits = 0)]
setorderv(Depths, c('Max_Depth_drilled'))

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

final_summary<- merge(fluid_summary, Depths[,c('Size','Max_Depth_drilled','meters_drilled','time_drilled_days','All_in_ROP_m_hr','bit_count','m_bit')], 
                      by.x=c('Size'), by.y=c('Size') )
setorderv(final_summary, c('Size'), order = -1)

final_summary[,max_density:=round(max_density, digits = 0)]
final_summary[,min_density:=round(min_density, digits = 0)]
final_summary[,Avg_density:=round(Avg_density, digits = 0)]
final_summary[,Avg_Viscosity:=round(Avg_Viscosity, digits = 0)]

#printing commands

setwd(out_wd)

well_table[,spud_date:=as.character(spud_date)]
well_table[,rig_release:=as.character(rig_release)]
fluid[, Date:=as.character(Date)]
time_log[, Date:=as.character(Date)]
time_log[, FromTime:=as.character(FromTime)]
time_log[, ToTime:=as.character(ToTime)]

#printing commands

wellname<-well_table[[2]][1]
wellname<-gsub("/","_",wellname)
setwd(out_wd)

#cairo_pdf(paste(Sys.Date(), well_data[[1]][2], 'Depth_vs_Days.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
cairo_pdf(paste(wellname, 'Depth_vs_Days.pdf'), width = 10)#, family ='Neutra Text SC', width = 6)
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


write.xlsx(final_summary, file=paste(wellname,'PASON_SUMMARY.xlsx'), sheetName="TO_PASTE", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE, password=NULL)


write.xlsx(well_table, file=paste(wellname,'PASON_SUMMARY.xlsx'), sheetName="Well_Information", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(Depths, file=paste(wellname,'PASON_SUMMARY.xlsx'), sheetName="bit_data", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(casing_depths, file=paste(wellname,'PASON_SUMMARY.xlsx'), sheetName="casing_data", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(fluid_summary, file=paste(wellname,'PASON_SUMMARY.xlsx'), sheetName="Fluid_Property", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

#PASON DETAILED RECAP
write.xlsx(fluid, file=paste(wellname,'PASON_DATA_BACKUP.xlsx'), sheetName="Fluid_Property", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE, password=NULL)

write.xlsx(bit_info, file=paste(wellname,'PASON_DATA_BACKUP.xlsx'), sheetName="Bit_Information", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(casing, file=paste(wellname,'PASON_DATA_BACKUP.xlsx'), sheetName="Casing_Information", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)

write.xlsx(time_log, file=paste(wellname,'PASON_DATA_BACKUP.xlsx'), sheetName="Time_log", 
           col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE, password=NULL)











