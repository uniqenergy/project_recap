
library(data.table)
library(ggplot2)
library(plot3D)
library(plotly)
library(rgl)
library(scatterplot3d)
library(extrafont)
library(XML)

font.pref <- list(size=16,family="Neutra Text SC",color="black")
x.list <- list(title = "Days from Spud",titlefont = font.pref, tickfont=font.pref,gridcolor = toRGB("gray50"))
y.list <- list(title = "Hole_Depth_m",titlefont = font.pref,tickfont=font.pref, autorange='reversed',
               gridcolor = toRGB("gray50"))

plot_ly(data = pason[Hole.Depth..meters.<5000,], x = ~days_from_spud, y = ~Hole.Depth..meters.,
        marker = list(size = 2,
                      color = '#12BC1A',
                      line = list(color = '#12BC1A',
                                  width = 2))) %>%
  layout(title = paste(well_data[[1]][2],'Depth vs Days'),titlefont=list(size=18,family="Neutra Text SC",color="black"),
         yaxis = y.list,
         xaxis = x.list)

