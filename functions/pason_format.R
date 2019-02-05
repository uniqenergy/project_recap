
pason_data<-function(x){

pason<-as.data.table(pason)
#converts old format to current
convert <- match(colnames(pason), c('Hole.Depth',	'Bit.Depth',	'Rotary.RPM',	'Rotary.Torque','Weight.on.Bit',	'Rate.Of.Penetration',	'Total.Pump.Output',	'Block.Height',	'Differential.Pressure'), nomatch = 0)
colnames(pason)[convert] <- c('Hole.Depth..meters.',	'Bit.Depth..meters.',	'Rotary.RPM..RPM.',	'Convertible.Torque..kN_m.',	'Weight.on.Bit..kDaN.',	'Rate.Of.Penetration..m_per_hr.',	'Total.Pump.Output..m3_per_min.',	'Block.Height..meters.',	'Differential.Pressure..kPa.')
pason<-pason[Hole.Depth..meters.>0,]
pason[,Date:=paste(YYYY.MM.DD, HH.MM.SS)]
pason[, Date:=as.POSIXct(Date, format='%Y/%m/%d %H:%M:%S')]
pason[, YYYY.MM.DD:=NULL]
pason[, HH.MM.SS:=NULL]


#subsets to seconds = 0
pason <- subset(pason, format(Date,'%S') %in% c('00'))
pason[, Hole.Depth..meters.:=as.numeric(Hole.Depth..meters.)]
#pason[,days_from_spud:=as.numeric(Date - min(Date))/86400]

return(pason)

}