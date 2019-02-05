library(data.table)
library(ggplot2)
library(plot3D)
library(plotly)
library(rgl)
library(scatterplot3d)
library(extrafont)
library(XML)

setwd("G:/My Drive/$Technical$ New/Technical/R_Scripts/01_Projects/Project_Recap")
source('functions/well_information.R')

directory<-'103_04-20-073-24W400 SPL 8-20 HZ 103 SMITH 4-20-73-24'
wd<- paste0("C:/Users/VHa/Desktop/Spur/",directory,"/data/")
out_wd<-paste0("C:/Users/VHa/Desktop/Spur/",directory)
setwd(wd)

survey<-data.table()
survey_file<-list.files(path=wd, full.names=TRUE, recursive=FALSE,pattern="*\\.txt")
for(i in 1:length(survey_file)){
  path<-data.table(read.delim (file=survey_file[[i]], header = TRUE, sep ="\t", skip = 3, stringsAsFactors = FALSE))
  setnames(path, c('MD_MKB','Incl_deg','Azm_deg','Grav_g','Mag_Gs','DipA_deg','TVD_mkb','VS_m','NS_m','EW_m','DLS_deg30','SS_m,','Status'))
  path<-path[!is.na(TVD_mkb),]
survey<-rbindlist(list(survey, path))
}

xml_path<-list.files(pattern="*xml")
doc<- xmlTreeParse(file = paste0(xml_path), useInternal = TRUE)
rootNode<-xmlRoot(doc)

well_data<-well_info()
well_table<- data.table(well_data[[1]])
survey<-survey[,WELL:=well_data[[1]][[2]]]
survey[, MD_MKB:=as.numeric(MD_MKB)]
#survey<-survey[MD_MKB>ICP,]

#3D plot
font.pref <- list(size=12,family="Neutra Text SC",color="black")
x.list <- list(title = "VS_m",titlefont = font.pref, tickfont=font.pref)
y.list <- list(title = "NS_m",titlefont = font.pref,tickfont=font.pref)
z.list <- list(title = "TVD_mkb",titlefont = font.pref,tickfont=font.pref)

directional<- plot_ly(survey, x = ~NS_m, y = ~VS_m, z = ~-TVD_mkb, type = 'scatter3d', mode = 'markers', color = ~WELL,
          marker = list(color = '#12BC1A', size = 5))%>%
  layout(title=paste(well_data[[1]][2],'Directional'),titlefont=list(size=18,family="Neutra Text SC",color="black"),
         scene=list(xaxis = x.list,
                    yaxis = y.list,
                    zaxis = z.list)) %>%
  #add_trace(x=1,y=1) %>%   
  layout(margin = list(l = 0, r = 0, b = 0, t = 30, pad = 2))
    #mode ='lines' or 'markers





