library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggpubr)
library(stringr)
library(tidyverse)

mydata4 <- read.csv("0228country整合.csv", sep=",") 

mydata4<-subset(mydata4,population>0)

range(mydata4$population)
size_range <- range(mydata4$population) / max(mydata4$population) *  2
mydata4$Color <- factor(mydata4$Color, 
                        levels = c("Japan/South Korea",
                                   "Europe",
                                   "China", 
                                   "United States",
                                   "South America",
                                   "India",
                                   "Africa",
                                   "Others"))



p3 <- ggplot(mydata4,aes(x =gghed_gdp, y =medianage2020))+
  geom_point(aes(size=population,color=Color),shape=21)+
  geom_smooth(method="lm")+ 
  stat_poly_eq(aes(label = paste(..eq.label..,
                                 ..adj.rr.label..,
                                 sep = '~~~~')),
               formula = y ~ x, parse = T,size=2.5)+
  theme (legend.position = "none")+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(legend.key.height=unit(.2, "cm"))+
  theme(
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, 
                         unit = "pt")
  )+
  theme (legend.text=element_text(size =7, colour = "black"))+
  guides(color=guide_legend(ncol = 2,
                            byrow = F,
                            reverse = F))+
  theme(legend.key = element_rect(color = 'white'))+
  guides(size=guide_legend(order = 1,ncol = 2),color=guide_legend(order=0))+
  theme(legend.key.height=unit(.1, "cm"))+
  theme (legend.background = element_blank(),
         legend.key.size=unit(4,'mm'),
         legend.text=element_text(size =10,colour = "black"))+
  theme (legend.text=element_text(size =7, colour = "black"))+
  guides(color=guide_legend(ncol = 2,
                            byrow = F,
                            reverse = F))+
  guides(size=guide_legend(ncol = 2,
                           byrow = F,
                           reverse = F))+
  guides(size=guide_legend(ncol = 2,
                           byrow = F,
                           reverse = F))+
  labs (y="Median Age\n(country in 2020)",
        x="Government Health Expenditure as % GDP")+
  scale_color_manual(values=c("#9B0D62",
                              "#F0595B",
                              "#E29526",
                              "#ebeb36",
                              "#AAC7A5",
                              "#3D9CCC",
                              "#4CB11A",
                              "#8E8E8E"))

mydata5 <- subset(mydata4, ext_gdp > 0)
range(mydata4$population)
size_range <- range(mydata4$population) / max(mydata4$population) *  2
mydata5$Color <- factor(mydata5$Color, 
                        levels = c("Japan/South Korea",
                                   "Europe",
                                   "China", 
                                   "United States",
                                   "South America",
                                   "India",
                                   "Africa",
                                   "Others"))
p4 <- ggplot(mydata5,aes(x = log10(ext_gdp), y =medianage2020))+
  geom_point(aes(size=population,color=Color),shape=21)+
  geom_smooth(method="lm")+ 
  stat_poly_eq(aes(label = paste(..eq.label..,
                                 ..adj.rr.label..,
                                 sep = '~~~~')),
               formula = y ~ x, parse = T,size=2.5)+
  theme (legend.position = "0.1")+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  labs (y="Median Age\n(country in 2020)",
        x=expression(paste(Log[10],"[External Health Expenditure as % GDP]")))+
  scale_color_manual(values=c("#F0595B",
                              "#E29526",
                              "#AAC7A5",
                              "#3D9CCC",
                              "#4CB11A",
                              "#8E8E8E"))



mydata5 <- subset(mydata4,Hospitalbeds!="NA")
p5<-ggplot(mydata5,aes(x =log10(Hospitalbeds), y =medianage2020))+
  geom_smooth(method="lm")+ 
  geom_point(aes(size=population,color=Color),shape=21)+
  stat_poly_eq(aes(label = paste(..eq.label..,
                                 ..adj.rr.label..,
                                 sep = '~~~~')),
               formula = y ~ x, parse = T,size=2.5)+
  theme (legend.position = "none")+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  labs (y="Median Age\n(country in 2020)",
        x=expression(paste(Log[10],"[Hospital beds per capita]")))+
  scale_color_manual(values=c("#9B0D62",
                              "#F0595B",
                              "#E29526",
                              "#ebeb36",
                              "#AAC7A5",
                              "#3D9CCC",
                              "#4CB11A",
                              "#8E8E8E"))

mydata6<-subset(mydata4,Physicians!="NA")
p6<-ggplot(mydata6,aes(x =log10(Physicians), y =medianage2020))+
  geom_smooth(method="lm")+ 
  geom_point(aes(size=population,color=Color),shape=21)+
  stat_poly_eq(aes(label = paste(..eq.label..,
                                 ..adj.rr.label..,
                                 sep = '~~~~')),
               formula = y ~ x, parse = T,size=2.5)+
  theme (legend.position = "none")+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  labs (y="Median Age\n(country in 2020)",
        x=expression(paste(Log[10],"[Physicians per capita]")))+
  scale_color_manual(values=c("#9B0D62",
                              "#F0595B",
                              "#E29526",
                              "#ebeb36",
                              "#AAC7A5",
                              "#3D9CCC",
                              "#4CB11A",
                              "#8E8E8E"))

tiff(file="062531correlation analysis.tif", res = 600, width = 3780, height = 2650, compression = "lzw")
ggarrange(p3,p4,p5,p6,
          common.legend = T, 
          legend = "bottom",
          ncol=2,nrow=2,align = "v",
          labels = c("a","b","c","d"))
dev.off()


