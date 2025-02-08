
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggpubr)
library(stringr)
library(tidyverse)


setwd("D:/大四科研/衰减/World")

mydata4 <- read.csv("0415quan输出属性.csv", sep=",") 
mydata4 <- mydata4 
mydata4$Color <- factor(mydata4$Color, levels = c("Japan/South Korea","Europe","China", "United States","South America","India", "Africa","Others"))
range(mydata4$population)

####################
p1 <- ggplot(mydata4,aes(x =YoungRatio, y =OldRatio))+
  geom_point(aes(size=population,color=Color),shape=21)+
  #geom_smooth(method="lm")+ 
  scale_y_continuous(expand = c(.05,0),limits = c(0,0.8))+
  scale_x_continuous(expand = c(0,0),limits = c(0,1)) +
  geom_hline(aes(yintercept=mean(OldRatio)),linetype=5,col="red",size=0.5)+
  geom_vline(aes(xintercept=mean(YoungRatio)),linetype=5,col="red",size=0.5)+
  #geom_abline(intercept = 0, slope = 1,
              #size=0.2,linetype=1)+
  
  scale_size_continuous(#name = "Pop. (million)",
                        range = c(0.1, 4), 
                        breaks = c(0.5, 1,5,10,20))+
  theme(legend.box.just = "bottom")+
  theme(legend.direction = "vertical",legend.box = "horizontal")+
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 5))+
  
  #scale_colour_discrete(labels = function(x) str_wrap(x, width = 12))
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(legend.position = "none")+
  ylab("Old Dependency Ratio")+
  xlab("Young Dependency Ratio")+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=8))+

  scale_color_manual(values=c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", 
                             "#fc8d59", "#d73027", "#7f3b08","#FFB84D"))+
  
  #scale_size_manual(name = "Population\n(million)")+
  guides(size=guide_legend(order = 1,ncol = 2),color=guide_legend(order=0))+
  theme(legend.key.height=unit(.2, "cm"))+
  theme (legend.background = element_blank(),
         legend.key.size=unit(4,'mm'),
         legend.text=element_text(size =10,colour = "black"))+
  theme (legend.text=element_text(size =6, colour = "black"))+
  theme(
    legend.background = element_rect(
      fill = "lightblue", # 填充色
      colour = "white", # 框线色
      size = 1.5 ) ) # 线条宽度
  guides(color=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = F,#默认F，表示按照列填充
                            reverse = F))+
  guides(size=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                           byrow = F,#默认F，表示按照列填充
                           reverse = F))+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+

  labs(subtitle = "d")+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 15,          #字体大小
                                     hjust = -0.25,          #字体左右的位置
                                     vjust = -3,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
    theme(legend.box.background = element_blank())+
    theme(
      plot.margin = margin(t = 5, r = 20, b = 5, l = 0, 
                           unit = "pt")
    )+
  theme(legend.title=element_text(size=10,family = "serif")) 

#####################
p2 <- ggplot(mydata4,aes(x =TwoYoung, y =YoungRatio))+
    theme(panel.border = element_rect(color = "black",fill="NA"))+
  geom_point(aes(size=population,color=Color),shape=21)+
  #geom_smooth(method="lm")+ 
  scale_y_continuous(expand = c(.05,0),limits = c(0,0.8))+
  scale_x_continuous(expand = c(0,0),limits = c(0,1)) +
  #geom_hline(aes(yintercept=mean(OldRatio)),linetype=5,col="red",size=0.5)+
  #geom_vline(aes(xintercept=mean(YoungRatio)),linetype=5,col="red",size=0.5)+
  geom_abline(intercept = 0, slope = 1,
              size=0.2,linetype=1)+
  
  scale_size_continuous(range = c(0.1, 4), 
                        breaks = c(0.5, 1,5,10,20))+
  #theme(legend.box.just = "bottom")+
  #theme(legend.direction = "vertical",legend.box = "horizontal")+
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 5))+

  #scale_colour_discrete(labels = function(x) str_wrap(x, width = 12))

  theme(legend.position = none)+
  ylab("Young Dependency Ratio\n(2020)")+
  xlab("Young Dependency Ratio\n(2000)")+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=8))+
  
    scale_color_manual(values=c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", 
                               "#fc8d59", "#d73027", "#7f3b08","#FFB84D"))+
  
  #scale_size_manual(name = "Population\n(million)")+

  guides(color=guide_legend(ncol = 4,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = F,#默认F，表示按照列填充
                            reverse = F))+
  guides(size=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                           byrow = F,#默认F，表示按照列填充
                           reverse = F))+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="white"))+
  labs(subtitle = "b")+
    #theme(panel.border = element_rect(color = "black"))+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 15,          #字体大小
                                     hjust = -0.25,          #字体左右的位置
                                     vjust = -3,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
    scale_size_continuous(name = "Pop. (million)",
                          range = c(0.1, 4), 
                          breaks = c(0.5, 1,5,10,20))+

    theme(legend.box.just = "bottom")+
    theme(legend.direction = "vertical", legend.box = "horizontal")+
    #scale_colour_discrete(labels = function(x) str_wrap(x, width = 5))+
    theme (legend.background = element_blank(),
           legend.key.size=unit(2,'mm'),
           legend.text=element_text(size =6),
           legend.spacing = unit(-0.2, 'cm'),
           legend.title = element_text(size=6))
  #theme(legend.title=element_text(size=10,family = "serif")) 

    theme(legend.key.height=unit(.2, "cm"))+

      guides(color = guide_legend(override.aes = list(linetype = c(1, 0, 0))))


############
p3 <- ggplot(mydata4,aes(x =TwoOld, y =OldRatio))+
  geom_point(aes(size=population,color=Color),shape=21)+
  #geom_smooth(method="lm")+ 
  scale_y_continuous(expand = c(.05,0),limits = c(0,0.8))+
  scale_x_continuous(expand = c(0,0),limits = c(0,1)) +
  #geom_hline(aes(yintercept=mean(OldRatio)),linetype=5,col="red",size=0.5)+
  #geom_vline(aes(xintercept=mean(YoungRatio)),linetype=5,col="red",size=0.5)+
  geom_abline(intercept = 0, slope = 1,
              size=0.2,linetype=1)+
  
  scale_size_continuous(name = "Pop. (million)",
                        range = c(0.1, 4), 
                        breaks = c(0.5, 1,5,10,20))+
  theme(legend.box.just = "bottom")+
  theme(legend.direction = "vertical",legend.box = "horizontal")+
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 5))+
      theme(
        plot.margin = margin(t = 5, r = 10, b = 5, l = 0, 
                             unit = "pt")
      )+
  #scale_colour_discrete(labels = function(x) str_wrap(x, width = 12))
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="white"))+
  theme(legend.position = "none")+
  ylab("Old Dependency Ratio\n(2020)")+
  xlab("Old Dependency Ratio\n(2000)")+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=8))+
  
      scale_color_manual(values=c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", 
                                 "#fc8d59", "#d73027", "#7f3b08","#FFB84D"))+
  
  #scale_size_manual(name = "Population\n(million)")+
  guides(size=guide_legend(order = 1,ncol = 2),color=guide_legend(order=0))+
  theme(legend.key.height=unit(.2, "cm"))+
  theme (legend.background = element_blank(),
         legend.key.size=unit(4,'mm'),
         legend.text=element_text(size =10,colour = "black"))+
  theme (legend.text=element_text(size =6, colour = "black"))+
  guides(color=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = F,#默认F，表示按照列填充
                            reverse = F))+
  guides(size=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                           byrow = F,#默认F，表示按照列填充
                           reverse = F))+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  labs(subtitle = "c")+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 15,          #字体大小
                                     hjust = -0.25,          #字体左右的位置
                                     vjust = -3,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
  theme(legend.title=element_text(size=10,family = "serif")) 




tiff(file="913youngold.tif", res = 600, width = 3780, height = 1500, compression = "lzw")
ggarrange(p2,p3,p1, common.legend = T, 
          legend = "bottom",
          ncol=3,nrow=1,#widths=c(1,1,0.8),
          align = "h")
#ggarrange(p6,p1,p2,heights=c(0,1,1),common.legend = T,legend = "bottom",ncol = 1)
dev.off()
