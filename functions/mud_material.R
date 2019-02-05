
mud_materials<- function(){
  
  material_table<-data.table()
  
  x<-1:rounded_days
  y<-1:10
  z<-1:15

  
  
  for(i in x){
    for (j in y){
      for(k in z){
        
        if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])==TRUE) {next
        }
        tour_date<-xmlToList(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][[1]][[1]])
        tour_date<-as.Date(tour_date, format ='%Y-%m-%d')
        
        if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j])==TRUE){next
        }
        
        if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j][['Tour']][['MudRecord']][['MudMaterials']][[k]])==TRUE){next
        }
        
        data<-rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Tours']][j][['Tour']][['MudRecord']][['MudMaterials']][[k]]
        
        data<- xmlToList(data)
        data<-as.data.table(data)
      
        data[, Date:=as.Date(tour_date, format='%Y-%m-%d-%H:%M')]
       
        #if(is.null(rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Equipment']][['Bits']][['Bit']][['Size']][[1]])==TRUE){next
         #}
    
        #bit_size<- rootNode[[1]][[1]][['DayTours']][i][['DayTour']][['Equipment']][['Bits']][j][['Bit']][['Size']][[1]]      
        #bit_size<- xmlToList(bit_size)
        #bit_size<-as.numeric(bit_size)
        #data[,  Bit_size:=bit_size]
        material_table<- smartbind(material_table, data)
    
      }
    }
  }
  material_table<-as.data.table(material_table)
  material_table[, Amount:=as.integer(Amount)]
  
  
return(material_table)
}