well_info<-function(){



#xmlName(rootNode) #ETS
#names(rootNode  [[1]][[1]])# displays names of rootnodes

license<-xmlToList(rootNode[[1]][[1]][['LicenseNo']][[1]])#xmlToList(rootNode[[1]][[1]][['LicenseNo']][[1]])
well_name<-xmlToList(rootNode[[1]][[1]][['WellName']][[1]])
UWI<- xmlToList(rootNode[[1]][[1]][['UniqueWellId']][[1]])
spud_date<- xmlToList(rootNode[[1]][[1]][['SpudDateTime']][[1]])
spud_date<-gsub('T'," ",spud_date)
#spud_date<-as.POSIXct(spud_date, format ='%Y-%m-%d %H:%M')
rig_release<- xmlToList(rootNode[[1]][[1]][['RigReleaseDateTime']][[1]])
rig_release<-gsub('T'," ",rig_release)
#rig_release<-'2018-10-12 17:59'
#rig_release<-as.POSIXct(rig_release, format ='%Y-%m-%d %H:%M')
well_type<-xmlToList(rootNode[[1]][[1]][['WellType']][[1]])
rig_number<-xmlToList(rootNode[[1]][[1]][['Rig']][[1]][[1]])
rig_company<- xmlToList(rootNode[[1]][[1]][['Contractor']][[2]][[1]])
rig<-paste0(rig_company,' #', rig_number)
days<-as.numeric(as.POSIXct(rig_release, format='%Y-%m-%d %H:%M')-as.POSIXct(spud_date, format='%Y-%m-%d %H:%M'))
rounded_days<-ceiling(days)+10

well_table<-cbind(license, well_name, UWI, spud_date, rig_release, well_type, rig_company, rig_number,days)
well_summary<-list(licence, well_name, UWI, spud_date, rig_release, well_type, rig_company,rig_number , days)
names(well_summary)<- c('licence', 'well_name', 'UWI', 'spud_date', 'rig_release', 'well_type', 'rig_company','rig_numer', 'days')



return(list(well_table, well_summary,rounded_days))


}
