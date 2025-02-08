library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggpubr)
library(stringr)
library(tidyverse)

library(reshape2)


setwd("D:/大四科研/衰减/World")

mydata2 <- read.csv("0505输出属性.csv", sep=",") 
mydata2$Color <- factor(mydata2$Color, levels = c("Japan/ South Korea","Europe","China", "United States","South America","India", "Africa","Others"))

reg.dis <- ggplot(mydata2, aes(x=reorder(Color,-Age),y=Age,fill=Color))+
  stat_boxplot(geom = "errorbar", width=0.5,size =0.2)+ 
  geom_boxplot(size=0.2,outlier.size=0.5, outlier.shape = 1,alpha=0.8)+
  
  annotate("segment", x = 6.62, xend=7.38,y = 21.1, yend = 21.1,
           size = 1,color="red")+
  #India
  annotate("segment", x = 5.62, xend=6.38,y = 28.7, yend = 28.7,
           size = 1,color="red")+
  #SA
  annotate("segment", x = 4.62, xend=5.38,y = 30.975, yend = 30.975,
           size = 1,color="red")+
  #USA
  annotate("segment", x = 3.62, xend=4.38,y = 38.5, yend = 38.5,
           size = 1,color="red")+
  #China
  annotate("segment", x = 2.62, xend=3.38,y = 38.4, yend = 38.4,
           size = 1,color="red")+
  #Europe
  annotate("segment", x = 1.62, xend=2.38,y = 41.9574, yend = 41.9574,
           size = 1,color="red")+
  #Japan
  annotate("segment", x = 0.62, xend=1.38,y = 45.9, yend = 45.9,
           size = 1,color="red")+
  #Legend
  annotate("segment", x = 4.42, xend=5.18,y = 44, yend = 44,
           size = 1,color="red")+
  annotate("text", x = 6.18 , y = 44,
           label = "Median age of\n country/continent", 
           colour = "black",size = 2.3,lineheight = .7) + 
  geom_hline(yintercept=38.8036, linetype="dashed", 
             color=alpha("steelblue", 0.5), size=0.3)+
  theme(axis.title.x = element_blank())+
  labs (y="Average age of cities\n (2020)")+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=8))+
  theme(legend.position ="none")+
  labs(subtitle = "c")+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 15,          #字体大小
                                     hjust = -0.15,          #字体左右的位置
                                     vjust = -3,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  scale_x_discrete(labels=c("Japan/ South Korea"="JS","Europe"="EU", "China"="CHN","United States"="USA","South America"="SA","India"="INA", "Africa"="AF","Others"="OS"))+
  scale_fill_manual(values=c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", 
                             "#fc8d59", "#d73027", "#7f3b08"))




temp.var <- ggplot(mydata2, aes(x=reorder(Color,-median),y=AgeDif,fill=Color))+
  geom_boxplot(size=0.2,outlier.size=0.2, outlier.shape = 1,alpha=0.8)+
  stat_boxplot(geom = "errorbar", width=0.5,size =0.2)+ #????????
  #Africa
  annotate("segment", x = 6.62, xend=7.38,y = 3.7758, yend = 3.7758,
           size = 1,color="red")+
  #India
  annotate("segment", x = 3.62, xend=4.38,y = 7.145, yend = 7.145,
           size = 1,color="red")+
  #SA
  annotate("segment", x = 2.62, xend=3.38,y = 7.726, yend = 7.726,
           size = 1,color="red")+
  #USA
  annotate("segment", x = 5.62, xend=6.38,y = 4.346, yend = 4.346,
           size = 1,color="red")+
  #China
  annotate("segment", x = 0.62, xend=1.38,y = 9.481, yend = 9.481,
           size = 1,color="red")+
  #Europe
  annotate("segment", x = 4.62, xend=5.38,y = 6.5248, yend = 6.5248,
           size = 1,color="red")+
  #Japan
  annotate("segment", x = 1.62, xend=2.38,y = 7.897, yend = 7.897,
           size = 1,color="red")+
  geom_hline(yintercept=4.2644, linetype="dashed", 
             color=alpha("steelblue", 0.5), size=0.3)+
  theme(axis.title.x = element_blank())+
  labs (y="Average age changes\n (2000-2020)")+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=8))+
  theme (legend.position ="none")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  labs(subtitle = "d")+
  theme(plot.subtitle = element_text(family = "serif", #字体
                                     size = 15,          #字体大小
                                     hjust = -0.15,          #字体左右的位置
                                     vjust = -3,          #字体上下的高度
                                     angle = 0,          #字体倾斜的角度
  ))+
  scale_x_discrete(labels=c("Japan/ South Korea"="JS","Europe"="EU", "China"="CHN","United States"="USA","South America"="SA","India"="INA", "Africa"="AF","Others"="OS"))+
  scale_fill_manual(values=c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", 
                             "#fc8d59", "#d73027", "#7f3b08"))+
  
  #scale_fill_manual(values=c("#FB61D7", "#A58AFF",  "#F8766D", "#00C094", "#00B6EB",
  #"#C49A00","#53B400"))
  
  
  
  tiff(file="9131spatio-temporal.tif", res = 600, width = 3780, height = 1300, compression = "lzw")
ggarrange(reg.dis,temp.var,
          ncol=2,nrow=1,
          align = "v")
dev.off()


