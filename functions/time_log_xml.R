time_codes<- function() {

setwd("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Project_Recap/functions")
time_code<-data.table(read.csv(file='time_codes.csv', header = TRUE, stringsAsFactors = FALSE))

time_log_table<-data.table()

x<-1:rounded_days
y<-1:15
z<-1:20



for(i in x){
  for (j in y){
    for (k in z){
    
      if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])==TRUE) {next
      }
      tour_date<-xmlToList(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])
      tour_date<-as.Date(tour_date, format ='%Y-%m-%d')
      
      if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j])==TRUE){next
      }
      if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j][['Tour']][['TimeLogs']][[k]])==TRUE) {next
      }
      
      data<-rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j][['Tour']][['TimeLogs']][[k]]#[['TimeLog']]
      
      data<- xmlToList(data)
      data<-as.data.table(data)
      
      #data<-xmlSApply(data, function(x) xmlSApply(x, xmlValue))
      #data<- data.table(data, keep.rownames = TRUE)
      #data_2<-as.data.table(t(data[,-1]))
      #colnames(data_2)<-data$rn
      time_log_table<- smartbind(time_log_table, data)
    }
  }
}
time_log_table<-as.data.table(time_log_table)
time_log_table[, FromTime:=substr(FromTime,1,19)]
time_log_table[, ToTime:=substr(ToTime,1,19)]
time_log_table[, FromTime:=gsub('T',' ',FromTime)]
time_log_table[, ToTime:=gsub('T',' ',ToTime)]
time_log_table[, FromTime:=as.POSIXct(FromTime, format ='%Y-%m-%d %H:%M:%S')]
time_log_table[, ToTime:=as.POSIXct(ToTime, format ='%Y-%m-%d %H:%M:%S')]
time_log_table<- merge(time_log_table, time_code, by.x=c('TimeCodeNo'), by.y=c('Value'), all.x=TRUE)
time_log_table[, Date:=as.Date(FromTime, format ='%Y-%m-%d')]
time_log_table[, Elapsed_hrs:=as.numeric(ToTime-FromTime)/60]
time_log_table<-time_log_table[,c('Date','FromTime','ToTime','Elapsed_hrs','Code','TimeCodeNo','Activity','Definition','Detail')]
setorderv(time_log_table, c('Date','FromTime'))

return(time_log_table)

}



