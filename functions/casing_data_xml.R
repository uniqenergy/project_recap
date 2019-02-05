casing_info <- function() {
  
  casing_table<-data.table()
  
  x<-1:rounded_days
  y<-1:10
  
  for(i in x){
    for (j in y){
      if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])==TRUE) {next
      }
      tour_date<-xmlToList(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])
      tour_date<-as.Date(tour_date, format ='%Y-%m-%d')
      
      if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tubular']][['Casings']][[j]])==TRUE){next
      }
      
      data<- rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tubular']][['Casings']][[j]]
      
      
      
      data<- xmlToList(data)
      data<-as.data.table(data)
      data$Date<-tour_date
      casing_table<- smartbind(casing_table, data)
    }
  }
  
  
casing_table<-as.data.table(casing_table)
casing_table<-casing_table[!duplicated(casing_table),]
max_date<- max(casing_table$Date)
casing_table<-casing_table[Date==max_date,]


return(casing_table)
}





