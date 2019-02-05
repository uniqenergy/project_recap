library(data.table)

rm(list=ls(all=TRUE))

well<-'103_13-03-033-25W300 (Baytex, HZ) Active not done'

setwd(paste0('C:/Users/VHa/Desktop/Baytex Tempco 6/',well,'/data/'))
file_source <-list.files(pattern="*csv")


source_file<-data.table(read.csv(file=paste0(file_source), header = TRUE, stringsAsFactors = FALSE))
source_file[, Date:=paste(YYYY.MM.DD, HH.MM.SS)]
source_file[, Date:=as.POSIXct(Date, format ='%Y/%m/%d %H:%M:%S')]
source_file <- subset(source_file, format(Date,'%S') %in% c('00'))
source_file[, Date:=NULL]

file_source<-gsub(".csv","",file_source)

write.csv(source_file, paste0(file_source, 'condensed.csv'), row.names = FALSE)


