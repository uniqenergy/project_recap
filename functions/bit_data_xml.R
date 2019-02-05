bit_info<- function(){

bit_table<-data.table()


x<-1:rounded_days
y<-1:15


for(i in x){
  for (j in y){
    
    if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])==TRUE) {next
    }
    tour_date<-xmlToList(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])
    tour_date<-as.Date(tour_date, format ='%Y-%m-%d')
    
    if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Equipment']][['Bits']][j][['Bit']])==TRUE){next
    }
    
    
    data<-rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Equipment']][['Bits']][j][['Bit']]
    data<- xmlToList(data)
    data<-as.data.table(data)
    

    data[, Date:=as.Date(tour_date, format='%Y-%m-%d-%H:%M')]
    #data[, EntryDate:=as.Date(substr(EntryDate,1,10), format ='%Y-%m-%d')]
    #table<-rbind(table, test_2)
    bit_table<- smartbind(bit_table, data)
    }
}
bit_table<-as.data.table(bit_table)
bit_table<-bit_table[!duplicated(bit_table),]
bit_table[, Size:=as.numeric(Size)]
bit_table[, DepthIn:=as.numeric(DepthIn)]
bit_table[, HoursRunToday:=as.numeric(HoursRunToday)]
bit_table[, CumulativeHoursRun:=as.numeric(CumulativeHoursRun)]
bit_table[, TourId:=as.numeric(TourId)]
bit_table[, Date:=as.Date(Date, format='%Y-%m-%d')]
bit_table[, DepthOut:=as.numeric(DepthOut)]
setorderv(bit_table, c('Date','TourId'))
bit_table<- bit_table %>% select(Date, everything())

return(bit_table)
}

