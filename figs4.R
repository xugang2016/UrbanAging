library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggpubr)

setwd("D:/大四科研/衰减/World") #设置当前工作路径
mydata4 <- read.csv("D:/大四科研/衰减/World/0507/513USA最终结果去异.csv", sep=",") #读取数据
#mydata4<-na.omit(mydata)
#mydata4$nshang<-as.numeric(mydata4$nshang)
#mydata4 = na.omit(mydata4)
#bzc=sd(mydata4$nshang)
#mydata4<-subset(mydata4,population>0)

range(mydata4$population)
size_range <- range(mydata4$population) / max(mydata4$population) *  2
#range(mydata4$GDP)
#size_range <- range(mydata4$GDP) / max(mydata4$GDP) *  10
#mydata4$Color <- factor(mydata4$Color, levels = c("Japan/South Korea","Europe","China", "United States","South America","India", "Africa","Others"))

################
p1<-ggplot(mydata4,aes(x =AREAL, y =age))+
  geom_point(aes(size=population),shape=21)+
  
  
  
  geom_abline(intercept = 0, slope = 1,alpha=0.8,size=1,linetype=1)+
  #scale_y_continuous(limits = c(18, 45))+
  #scale_x_continuous(limits = c(18, 45))+
  #scale_size_continuous(range = size_range)+#size_range
  #facet_wrap(~Fenlei1,ncol=2,scales ="free")+ 
  theme (legend.background = element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =6),
         legend.spacing = unit(-0.2, 'cm'),
         legend.title = element_text(size=6))+
  theme(legend.position = "none")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"),
        plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"))+
  #theme(legend.position = c(0.2,0.8))+
  #scale_color_discrete(name="")+
  #scale_fill_hue(guide = guide_legend(title = NULL))+
  #guides(color="none")+
  #guides(size=guide_legend(title="Ppopulation\n(million)"))+
  ylab("Average age(Worldpop)")+
  xlab("Average age(census data)")+
  
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(legend.title=element_text(size=6,family = "serif")) +
  #face = "italic"
  #labs(subtitle = "c.")+
  #theme(plot.subtitle = element_text(family = "serif", #字体
  #size = 15,          #字体大小
  #hjust = -0.2,          #字体左右的位置
  #vjust = -3,          #字体上下的高度
  #angle = 0,          #字体倾斜的角度
  #))+
  theme(axis.text.x = element_text(family = "serif", colour = "black",size = 7))+
  theme(axis.text.y = element_text(family = "serif",colour = "black", size = 7))+
  theme(axis.title.y = element_text(family = "serif",colour = "black", size = 7))+
  theme(axis.title.x = element_text(family = "serif",colour = "black", size = 7))+
  
  theme(legend.key.height=unit(.2, "cm"))+

  #theme(strip.background = element_rect(color = "white", fill = "white"),
  #panel.grid = element_blank())+
  
  #theme (strip.text.x = element_text(size = 10,family = "serif",
  # margin = margin(1,0,1,0, "pt")),
  #strip.background = element_blank())+
  labs(subtitle = "a. Average age")+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 10,          #字体大小
                                     hjust = 0.1,          #字体左右的位置
                                     vjust = -10,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
  #plot_grid(p1 +
  #guides(color = "none") + 
  #theme(legend.position = c(0.92, 0.8)), 
  #guide_color, nrow = 2, rel_heights = c(10, 1))

theme (legend.text=element_text(size =6,family = "serif", colour = "black"))+
  guides(color=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = F,#默认F，表示按照列填充
                            reverse = F))+
  guides(size=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                           byrow = F,#默认F，表示按照列填充
                           reverse = F))
######################
#mydata5 <- read.csv("D:/大四科研/衰减/World/0507/USA最终结果去异.csv", sep=",") #读取数据


range(mydata4$population)
size_range <- range(mydata4$population/ max(mydata4$population)) *  2
p2<-ggplot(mydata4,aes(x =REAL1, y =Ratio))+
  geom_point(aes(size=population),shape=21)+
  
  
  geom_abline(intercept = 0, slope = 1,alpha=0.8,size=1,linetype=1)+
  #scale_y_continuous(limits = c(18, 45))+
  #scale_x_continuous(limits = c(18, 45))+
  #scale_size_continuous(range = size_range)+#size_range
  #facet_wrap(~Fenlei1,ncol=2,scales ="free")+ 
  theme (legend.background = element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =6),
         legend.spacing = unit(-0.2, 'cm'),
         legend.title = element_text(size=6))+
  theme(legend.position = c(0.7,0.2))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"),
        plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"))+
  #theme(legend.position = c(0.2,0.8))+
  #scale_color_discrete(name="")+
  #scale_fill_hue(guide = guide_legend(title = NULL))+
  #guides(color="none")+
  guides(size=guide_legend(title="Population\n(million)"))+
  ylab("Age dependence Ratio(Worldpop)")+
  xlab("Age dependence Ratio(census data)")+
  labs(subtitle = "b.Age dependence ratio")+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 10,          #字体大小
                                     hjust = 0.1,          #字体左右的位置
                                     vjust = -10,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(legend.title=element_text(size=6,family = "serif")) +
  #face = "italic"
  #labs(subtitle = "c.")+
  #theme(plot.subtitle = element_text(family = "serif", #字体
  #size = 15,          #字体大小
  #hjust = -0.2,          #字体左右的位置
  #vjust = -3,          #字体上下的高度
  #angle = 0,          #字体倾斜的角度
  #))+
  theme(axis.text.x = element_text(family = "serif", colour = "black",size = 7))+
  theme(axis.text.y = element_text(family = "serif",colour = "black", size = 7))+
  theme(axis.title.y = element_text(family = "serif",colour = "black", size = 7))+
  theme(axis.title.x = element_text(family = "serif",colour = "black", size = 7))+
  
  theme(legend.key.height=unit(.2, "cm"))+

  #theme(strip.background = element_rect(color = "white", fill = "white"),
  #panel.grid = element_blank())+
  
  #theme (strip.text.x = element_text(size = 10,family = "serif",
  # margin = margin(1,0,1,0, "pt")),
  #strip.background = element_blank())+
  labs(size="Population\n(million)")+
  #plot_grid(p1 +
  #guides(color = "none") + 
  #theme(legend.position = c(0.92, 0.8)), 
  #guide_color, nrow = 2, rel_heights = c(10, 1))
theme(legend.key = element_rect(color = 'white'))+
theme (legend.text=element_text(size =6,family = "serif", colour = "black"))+
  guides(color=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = F,#默认F，表示按照列填充
                            reverse = F))+
  guides(size=guide_legend(ncol = 2,#根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                           byrow = F,#默认F，表示按照列填充
                           reverse = F))+
  
tiff(file="06251USA.tif", res = 600, width = 3600, height = 1800, compression = "lzw") 
ggarrange(p1,p2)
dev.off()