library(tidyverse)
source("plot_half_rink.R")

# TEMPLATE
plot_half_rink(ggplot())+
  xlim(115,210)+
  ylim(-10, 95)+
  geom_segment(aes(x = 125, y = -1, xend = 200, yend = -1),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"))+
  geom_segment(aes(x = 201, y = 0, xend = 201, yend = 85),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"))+
    annotate("segment", x = 189, xend = 189, y = 39.5, yend = 45.5, linetype = "dashed", color='blue')+ #goal line
    annotate("segment", x = 189, xend = 169, y = 45.5, yend = 64.5, linetype = "dashed", color='blue')+ #goal to Top Faceoff dot
    annotate("segment", x = 169, xend = 154, y = 64.5, yend = 64.5, linetype = "dashed", color='blue')+ #Top Faceoff dot to top of Faceoff Circle
    annotate("segment", x = 154, xend = 154, y = 64.5, yend = 20.5, linetype = "dashed", color='blue')+ #Top of top Faceoff Circle to Top of bottom Faceoff Circle
    annotate("segment", x = 154, xend = 169, y = 20.5, yend = 20.5, linetype = "dashed", color='blue')+ #Top of Bottom Faceoff Circle to Bottom Faceoff Dot
    annotate("segment", x = 169, xend = 189, y = 20.5, yend = 39.5, linetype = "dashed", color='blue')+ #Bottom Faceoff Dot to goal
    annotate("segment", x  =125, xend = 200, y = 42.5, yend = 42.5, linetype = "dashed", color='#22ded0')+ #midline
    annotate("text", x = 127, y = -4, label = "125", fontface = 'bold', size = 4)+
    annotate("text", x = 197, y = -4, label = "200", fontface = 'bold', size = 4)+
    annotate("text", x = 165, y = -4, label = "X-Coordinate", fontface = 'bold', size = 5)+
    annotate("text", x = 204, y = 2, label = "0", fontface = 'bold', size = 4)+
    annotate("text", x = 204, y = 83, label = "85", fontface = 'bold', size = 4)+
    annotate("text", x = 204, y = 42.5, label = "Y-Coordinate", fontface = 'bold', size = 5, angle = -90)+
    annotate("text", x = 139, y = 44, label = "Midline of Ice (y=42.5)", fontface = 'bold', size = 2.5)+
    annotate("text", x = 162, y = 66, label = "'The House'", fontface = 'bold', size = 2.5)+
    annotate("text", x = 160, y = 88, label = "Offensive Zone", fontface = 'bold', size = 6)
  
      
