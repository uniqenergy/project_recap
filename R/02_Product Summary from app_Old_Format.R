library(xlsx)
library(data.table)
library(reshape2)
library(ggplot2)
library(flextable)
library(officer)
library(scales)
library(ggforce)
library(dplyr)
library(ggrepel)
library(gridExtra)
library(extrafont)
library(fuzzyjoin)
library(stringr)

rm(list=ls(all=TRUE))

directory<-'200_a-042-J_094-B-09_00 (PPY, c-G20-I94-B-16, Townsend, Montney HZ)'
wd<- paste0('G:/My Drive/$Technical$ New/Technical/Well Files/BC/94-B-09/',directory,'/Recaps/data/')
out_wd<- paste0('G:/My Drive/$Technical$ New/Technical/Well Files/BC/94-B-09/',directory,'/Recaps/')
setwd(wd)
xml_file<-list.files(pattern="*xml")

setwd("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Project_Recap")
source('functions/zero_format.R')
source('functions/percent_format.R')
source('functions/currency_format.R')
source('functions/well_information.R')

type<-data.table(read.csv(file='functions/product_type.csv', header = TRUE, stringsAsFactors = FALSE))
type<- unique(type[, c('Product','Product_Type')])

setwd(wd)
doc<- xmlTreeParse(file = paste0(xml_file), useInternal = TRUE)
rootNode<-xmlRoot(doc)

well_data<-well_info() # well info from function
usage<- read.xlsx('Inventory.xlsx', 
                  sheetName='Inventory Report Usage Export', header = TRUE,
                  colClasses = NA, stringsAsFactors=FALSE)
usage<-as.data.table(usage)
usage<-usage[,!c('NA.','NA..1','NA..2','NA..3','NA..4','NA..5','NA..6','NA..7')]
usage[, Price....:=as.numeric(Price....)]
usage<-melt(usage, id.vars = c('Produt.Name','Unit','Price....'))
usage[, value:=as.numeric(value)]
setnames(usage, old=c('Produt.Name','variable','value','Price....'), new=c('ProductName','report','quantity','Unit_Price'))
usage<-usage[!is.na(quantity),]
usage[, quantity:=-(quantity)]
usage[, cost:=quantity*Unit_Price]
usage[, ProductName:= trimws(ProductName, "right")]
usage[, ProductName:= trimws(ProductName, "left")]
usage<- usage[!ProductName %in% c('Pallets', 'Pallet','Drums', 'pallet', 'pallets'),]
usage[, ProductName:=gsub("  "," ",ProductName)]

total<-usage[, list(total=sum(quantity, na.rm=TRUE)), by=c('ProductName', 'Unit','Unit_Price')]
total[, ProductName:=str_to_title (ProductName)]
total[, ProductName:=gsub('drill Af','drill AF', ProductName)]
total[, ProductName:=gsub('204rd','204RD', ProductName)]
total[, ProductName:=gsub('Cp','CP', ProductName)]
total[, ProductName:=gsub('`(22.68Kg)`','', ProductName)]
total[, ProductName:=gsub('Hv','HV', ProductName)]
total[, ProductName:=gsub('Lv','LV', ProductName)]
total[, ProductName:=gsub('Hd','HD', ProductName)]
total[, ProductName:=gsub('A1703d','A1703D', ProductName)]
total[, ProductName:=gsub('A1103d','A1103D', ProductName)]
total[, ProductName:=gsub('Dfi','DFI', ProductName)]
total[, ProductName:=gsub('Desco Ii','Desco II', ProductName)]
total[, ProductName:= trimws(ProductName, "right")]
total[, ProductName:= trimws(ProductName, "left")]
total[, total_cost:=Unit_Price*total]
total<-total[total_cost>0,]
total<-merge(total, type, by.x=c('ProductName'), by.y=c('Product'), all.x = TRUE)

total[, total_cost:=Unit_Price*total]
total[, cost_percentage:= total_cost / sum(total_cost)]
total[, cost_percentage:= sprintf("%.1f%%", 100 * cost_percentage)]
total<-total[total_cost>0,]


total[, cost_label:=paste0(ProductName,", ", dollar_format()(total_cost),", ", cost_percentage)]
total <- total %>% 
  mutate(end = 2 * pi * cumsum(total_cost)/sum(total_cost),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))


total<-as.data.table(total)
setkeyv(total, c('ProductName'))


#tables
my_table<-regulartable(total[,c('ProductName','Unit','Unit_Price','total','total_cost')])
my_table<-bg(my_table, bg='#12BC1A', part='header')#colors header row
my_table<-bold(my_table, part='header')
my_table<-italic(my_table, part='header')
my_table<-font(my_table, part ='header', fontname = 'Neutra Text Light Alt')
my_table<-font(my_table, fontname = 'Neutra Text Light Alt')
my_table<-fontsize(my_table, part = "header", size = 18) 
my_table<-fontsize(my_table, size = 16) 
my_table<-color(my_table, color = "white", part = "header")
my_table<-align(my_table, align = 'center', part = 'header')
my_table<-align(my_table, align = 'center')
my_table<-autofit(my_table)

#apply helper functions
my_table<-set_formatter(my_table, total=zero_format)
my_table<-set_formatter(my_table, Unit_Price=currency_format)
my_table<-set_formatter(my_table, total_cost=currency_format)

#border defintision
big_b <- fp_border(color="gray70", width = 1)
my_table <- border_outer( my_table, border = big_b, part = "all" )
my_table <- border_inner_h( my_table, border = big_b, part = "all" )
my_table <- border_inner_v( my_table, border = big_b, part = "all" )


setorderv(total, c('total_cost'), order = -1)

#by product type
total2<-total[, list(total=sum(total_cost, na.rm=FALSE)), by=c('Product_Type')]
total2[is.na(Product_Type),Product_Type:='MISCELLANEOUS_2']
total2<-total2[, list(total=sum(total, na.rm=FALSE)), by=c('Product_Type')]
setorderv(total2, c('total'))
total2 <- total2 %>% 
  mutate(end = 2 * pi * cumsum(total)/sum(total),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

total2<-as.data.table(total2)
total2[, cost_percentage:= total / sum(total)]
total2[, cost_percentage:= sprintf("%.1f%%", 100 * cost_percentage)]
total2<-total2[total>0,]

total2[, cost_label:=paste0(Product_Type,", ", dollar_format()(total), ", ",cost_percentage)]
total2[, Percent_of_total:= total/sum(total)*100]
#setnames(total2, old=c('total'), new=c('Cost'))
setorderv(total2, c('total'), order = -1)

#table2_______________________________________________________________________
#tables
my_table_2<-regulartable(total2[,c('Product_Type','total')])
my_table_2<-bg(my_table_2, bg='#12BC1A', part='header')#colors header row
my_table_2<-bold(my_table_2, part='header')
my_table_2<-italic(my_table_2, part='header')
my_table_2<-font(my_table_2, part ='header', fontname = 'Neutra Text Light Alt')
my_table_2<-font(my_table_2, fontname = 'Neutra Text Light Alt')
my_table_2<-fontsize(my_table_2, part = "header", size = 18) 
my_table_2<-fontsize(my_table_2, size = 16) 
my_table_2<-color(my_table_2, color = "white", part = "header")
my_table_2<-align(my_table_2, align = 'center', part = 'header')
my_table_2<-align(my_table_2, align = 'center')
my_table_2<-autofit(my_table_2)

#apply helper functions
my_table_2<-set_formatter(my_table_2, total=currency_format)
my_table_2<-set_formatter(my_table_2, Unit_Price=currency_format)
my_table_2<-set_formatter(my_table_2, total_cost=currency_format)

#border defintision
my_table_2 <- border_outer( my_table_2, border = big_b, part = "all" )
my_table_2 <- border_inner_h( my_table_2, border = big_b, part = "all" )
my_table_2 <- border_inner_v( my_table_2, border = big_b, part = "all" )

#color scheme
c_pal<-c('black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B','#D1FED3','#EBFFEC',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B','#D1FED3','#EBFFEC',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B','#D1FED3','#EBFFEC',
         'black','#132B03','#063F09','#0C7E12','#12BC1A','#11ED00','#18FB23','#74FD7B','#D1FED3','#EBFFEC')


#inidivudal product usage
pie<- ggplot(total) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 0.9,
                   start = start, end = end, fill = ProductName)) +
  geom_text(aes(x = 1 * sin(middle), y = 1 * cos(middle), label = cost_label,
                hjust = hjust, vjust = 1), size = 3.5, check_overlap = TRUE, family='Neutra Text SC')  +
  coord_fixed() +
  theme_bw()+
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)+
  scale_fill_manual(values = c_pal)+
  theme(legend.position = 'none')+ # removes legend+
  theme(panel.border = element_blank())+ #removes borders
  ggtitle(paste('Product Usage Breakdown:',well_data[[1]][2])) +
  theme(plot.title=element_text(hjust=0.5, family ='Neutra Text SC', size = 16))+
  labs(caption='**Please note: Small cost items that have been clustered have been excluded for readability purposes')+
  theme(plot.caption =element_text(family = 'Neutra Text SC', hjust = 0.5))

pie2<- ggplot(total2) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 0.9,
                   start = start, end = end, fill = Product_Type)) +
  geom_text(aes(x = 1 * sin(middle), y = 1 * cos(middle), label = cost_label,
                hjust = hjust, vjust = 1, family='Neutra Text SC'), size = 4, check_overlap = TRUE)  +
  coord_fixed() +
  theme_bw()+
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)+
  scale_fill_manual(values = c_pal)+
  theme(legend.position = 'none')+ # removes legend+
  theme(panel.border = element_blank())+ #removes borders
  ggtitle(paste('Product Usage Breakdown:',well_data[[1]][2])) +
  theme(plot.title=element_text(hjust=0.5, family ='Neutra Text SC', size = 16))+
  labs(caption='**Please note: Small cost items that have been clustered have been excluded for readability purposes')+
  theme(plot.caption =element_text(family = 'Neutra Text SC', hjust = 0.5))

unique(total[is.na(Product_Type), list(ProductName)])

setwd(out_wd)

cairo_pdf('Individual_Product_Usage.pdf', width = 11)#, family ='Neutra Text SC', width = 6)
pie
dev.off()

cairo_pdf('Product_Usage_by_Type.pdf', width = 11)#, family ='Neutra Text SC', width = 6)
pie2
dev.off()

read_docx() %>%
  body_add_flextable(my_table) %>%
  print(target=paste0(out_wd,"/",'INDIVIDUAL_PRODUCT_COST_SUMMARY.docx'))

read_docx() %>%
  
  body_add_flextable(my_table_2) %>%
  print(target=paste0(out_wd,"/",'PRODUCT_TYPE_COST_SUMMARY.docx'))
total[is.na(Product_Type),]
