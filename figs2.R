





####correlation analysis
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggpubr)
library(stringr)
library(tidyverse)
mydata4 <- read.csv("0228country.csv", sep=",") 

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

p1 <- ggplot(mydata4,aes(x = log10(gdp_pc_usd), 
                       y = medianage2020,
                       size=population))+
  geom_point(aes(color=Color),shape=21)+
  geom_smooth(method="lm")+ 
  stat_poly_eq(aes(label = paste(..eq.label..,
                                 ..adj.rr.label..,
                                 sep = '~~~~')),
               formula = y ~ x, parse = T,size=2.5)+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  labs (y="Median Age\n(country in 2020)",
        x=expression(paste(Log[10],"[GDP per capita ($)]")))+
  theme(legend.position ="none")+
  theme(
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, 
                         unit = "pt")
  )+
  theme (legend.text=element_text(size =7, colour = "black"))+
  guides(color=guide_legend(ncol = 2,
                            byrow = F,
                            reverse = F))+
  guides(size=guide_legend(ncol = 2,
                           byrow = F,
                           reverse = F))+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  scale_color_manual(name="",labels = c("Japan/\nSouth Korea","Europe","China", "United States","South America","India", "Africa","others"),
                     values=c("#FB61D7", "#A58AFF",  "#F8766D", "#00B6EB", "#00C094",
                              "#C49A00","#53B400","#8E8E8E"))
p2 <- ggplot(mydata4,aes(x = log10(Fertility), y = medianage2020))+
  geom_point(aes(size=population,color=Color),shape=21)+
  geom_smooth(method="lm")+ 
  stat_poly_eq(aes(label = paste(..eq.label..,
                                 ..adj.rr.label..,
                                 sep = '~~~~')),
               formula = y ~ x, parse = T,size=2.5)+
  theme (legend.position = "none")+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(legend.position ="none")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  labs (y="Meidan Age\n(country in 2020)",
        x=expression(paste(Log[10],"[Fertility rate]")))+
  scale_color_manual(name="",labels = c("Japan/\nSouth Korea","Europe","China", "United States","South America","India", "Africa"),
                     values=c("#FB61D7", "#A58AFF",  "#F8766D", "#00B6EB", "#00C094",
                              "#C49A00","#53B400","#8E8E8E"))

tiff(file="06252fercorrelation analysis.tif", res = 600, width = 3400, height = 1260, compression = "lzw")
ggarrange(p1,p2, ncol=2,nrow=1,
          labels = c("a","b"))
dev.off()


