
fluid_property<- function(){

density_table<-data.table()

x<-1:rounded_days
y<-1:15
z<-1:10




for(i in x){
  for (j in y){
    for(k in z){
      
      if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])==TRUE) {next
      }
      tour_date<-xmlToList(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])
      tour_date<-as.Date(tour_date, format ='%Y-%m-%d')
      
      if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j])==TRUE){next
      }
      
     if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j][['Tour']][['MudRecord']][['MudSamples']][[k]])==TRUE){next
     }
          
      
      data<-rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j][['Tour']][['MudRecord']][['MudSamples']][[k]]
      
      data<- xmlToList(data)
      data<-as.data.table(data)
      
      #data<-xmlSApply(data, function(x) xmlSApply(x, xmlValue))
      #data<- data.table(data, keep.rownames = TRUE)
      #data_2<-as.data.table(t(data[,-1]))
      #colnames(data_2)<-data$rn
      data[, Date:=as.Date(tour_date, format='%Y-%m-%d-%H:%M')]
      density_table<- smartbind(density_table, data)
    }
  }
}
density_table<-as.data.table(density_table)
density_table<- density_table[!is.na(Density),]
density_table[, Date:=as.POSIXct(paste(Date, Time), format ='%Y-%m-%d %H:%M:%S')]
density_table<- density_table[!is.na(Date),]
density_table[, days_from_spud:=as.numeric(Date - min(Date))/86400]
density_table<- density_table %>% select(Date, everything()) #moves date column first
density_table[, Density:=as.numeric(Density)]
density_table[, FunnelViscosity:=as.numeric(FunnelViscosity)]
#density_table[, FluidPh:=as.numeric(FluidPh)]  
density_table[, Depth:=as.numeric(Depth)]  
#density_table[, PVT:=as.numeric(PVT)]  
#density_table[, WaterLoss:=as.numeric(WaterLoss)]
density_table[, Time:=NULL]

density_table<- density_table %>% select(Date, everything())

return(density_table)


}
