library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggpubr)
library(stringr)
library(tidyverse)
library(reshape2)

setwd("D:/大四科研/衰减/World")

mydata2 <- read.csv("0409输出属性.csv", sep=",") 
mydata2$Color <- factor(mydata2$Color, levels = c("Japan/South Korea","Europe","China", "United States","South America","India", "Africa","Others"))

reg.dis <- ggplot(mydata2, aes(x=reorder(Color,-Ratio),y=Ratio,fill=Color))+
  stat_boxplot(geom = "errorbar", width=0.5,size =0.2)+ 
  geom_boxplot(size=0.2,outlier.size=0.5, outlier.shape = 1,alpha=0.8)+
  
  annotate("segment", x = 1.62, xend=2.38,y = 0.75679, yend = 0.75679,
           size = 1,color="red")+
  #India
  annotate("segment", x = 5.62, xend=6.38,y = 0.4878, yend = 0.4878,
           size = 1,color="red")+
  #SA
  annotate("segment", x = 4.62, xend=5.38,y = 0.51906, yend = 0.51906,
           size = 1,color="red")+
  #USA
  annotate("segment", x = 2.62, xend=3.38,y = 0.5322, yend = 0.5322,
           size = 1,color="red")+
  #China
  annotate("segment", x = 6.62, xend=7.38,y = 0.4414, yend = 0.4414,
           size = 1,color="red")+
  #Europe
  annotate("segment", x = 3.62, xend=4.38,y = 0.52302, yend = 0.52302,
           size = 1,color="red")+
  #Japan
  annotate("segment", x = 0.62, xend=1.38,y = 0.7094, yend = 0.7094,
           size = 1,color="red")+
  #Legend
  #annotate("segment", x = 4.42, xend=5.18,y = 0.5, yend = 0.5,
  # size = 1,color="red")+
  #annotate("text", x = 6.18 , y = 0.8,
  # label = "Age dependency ratio of\n country/continent", 
  #colour = "black",size = 2.3,lineheight = .7) + 
  geom_hline(yintercept=0.4973, linetype=1, col = 'blue')+
  theme(axis.title.x = element_blank())+
  labs(subtitle = "b")+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 15,          #字体大小
                                     hjust = -0.25,          #字体左右的位置
                                     vjust = -3,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
  labs (y="Age dependency ratio\n (2020)")+
  theme(axis.text=element_text(size =6), 
        axis.title = element_text(size=6))+
  theme(legend.position ="none")+
  theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 1, 
                         unit = "pt")
  )+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  #scale_fill_manual(values=c("#53B400", "#F8766D",  "#A58AFF", "#C49A00", "#FB61D7",
  #  "#00C094","#00B6EB"))
  scale_x_discrete(labels=c("Japan/South Korea"="JS","Europe"="EU", "China"="CHN","United States"="USA","South America"="SA","India"="INA", "Africa"="AF","Others"="OS"))+
  scale_fill_manual(values=c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", 
                             "#fc8d59", "#d73027", "#7f3b08"))



temp.var <- ggplot(mydata2, aes(x=reorder(Color,-ChaRatio),y=ChaRatio,fill=Color))+
  geom_boxplot(size=0.2,outlier.size=0.2, outlier.shape = 1,alpha=0.8)+
  stat_boxplot(geom = "errorbar", width=0.5,size =0.2)+ #添加轴须
  #Africa
  annotate("segment", x = 4.62, xend=5.38,y = -0.10067,yend = -0.10067,
           size = 1,color="red")+
  #India
  annotate("segment", x = 6.62, xend=7.38,y = -0.1662, yend = -0.1662,
           size = 1,color="red")+
  #SA
  annotate("segment", x = 5.62, xend=6.38,y = -0.12313, yend = -0.12313,
           size = 1,color="red")+
  #USA
  annotate("segment", x = 2.62, xend=3.38,y = 0.0229, yend = 0.0229,
           size = 1,color="red")+
  #China
  annotate("segment", x = 3.62, xend=4.38,y = -0.0175, yend = -0.0175,
           size = 1,color="red")+
  #Europe
  annotate("segment", x = 1.62, xend=2.38,y = 0.0395, yend = 0.0395,
           size = 1,color="red")+
  #Japan
  annotate("segment", x = 0.62, xend=1.38,y = 0.235, yend = 0.235,
           size = 1,color="red")+
  geom_hline(yintercept=0.01554, linetype=1, col = 'blue')+
  theme(axis.title.x = element_blank())+
  labs (y="Age dependency ratio\n (2000-2020)")+
  labs(subtitle = "c")+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 15,          #字体大小
                                     hjust = -0.25,          #字体左右的位置
                                     vjust = -3,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
  theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 5, 
                         unit = "pt")
  )+
  theme(axis.text=element_text(size =6), 
        axis.title = element_text(size=6))+
  theme (legend.position ="none")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  scale_x_discrete(labels=c("Japan/South Korea"="JS","Europe"="EU", "China"="CHN","United States"="USA","South America"="SA","India"="INA", "Africa"="AF","Others"="OS"))+
  #scale_fill_manual(values=c("#53B400", "#F8766D",  "#A58AFF", "#C49A00", "#FB61D7",
  # "#00C094","#00B6EB"))
  scale_fill_manual(values=c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", 
                             "#fc8d59", "#d73027", "#7f3b08"))

mydata2 <- read.csv("0331完整输出属性.csv", sep=",") 
mydata3 <- subset(mydata2,Color!="Others")
mydata3$Color <- factor(mydata3$Color, levels = c("Japan/South Korea","Europe","China", "United States","South America","India", "Africa"))
range(mydata3$population)

p3 <- ggplot(mydata3,aes(x =Age, y =Ratio,size=population,color=Color,group=Color))+
  geom_point(shape=21)+
  geom_smooth(method="lm",size=0.5)+ 
  #stat_poly_eq(aes(label = paste(..eq.label..,
  #..adj.rr.label..,
  #sep = '~~~~')),
  #formula = y ~ x, parse = T,size=2.5)+
  scale_size_continuous(name = "Pop. (million)",
                        range = c(0.1, 4), 
                        breaks = c(0.5, 1,5,10,20))+
  theme(legend.box.just = "bottom")+
  theme(legend.direction = "vertical", legend.box = "horizontal")+
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme (legend.background = element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =6),
         legend.spacing = unit(-0.2, 'cm'),
         legend.title = element_text(size=6))+
  
  #scale_colour_discrete(labels = function(x) str_wrap(x, width = 12))
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.75,0.42))+
  xlab("\nAverage age of cities")+
  ylab("Age Dependency Ratio")+
  theme(axis.text=element_text(size =6), 
        axis.title = element_text(size=6))+
  theme(axis.title.x = element_text(vjust = 8))+
  #scale_color_manual(name="",
  #values=c("#53B400", "#F8766D",  "#A58AFF", "#C49A00", "#FB61D7",
  # "#00C094","#00B6EB"))+
  scale_color_manual(values=c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", 
                                                "#fc8d59", "#d73027", "#7f3b08"))+
  labs(subtitle = "d")+
  
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 15,          #字体大小
                                     hjust = -0.25,          #字体左右的位置
                                     vjust = -3,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
  theme(
    plot.margin = margin(t = 0, r = 2, b = 0, l = 2, 
                         unit = "pt")
  )+
  #scale_size_manual(name = "Population\n(million)")+
  guides(size=guide_legend(order = 1,ncol = 2),color=guide_legend(order=0))

tiff(file="913ageRatio3.tif", res = 600, width = 3780, height = 1200, compression = "lzw")
ggarrange(reg.dis,temp.var,p3,
          ncol=3,nrow=1,widths=c(1,1,0.8),
          align = "h")
  #, family = "serif"        
 dev.off()