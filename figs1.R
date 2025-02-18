library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggpubr)
library(stringr)
library(tidyverse)

mydata1 <- read.csv("Agetimes.csv", sep=",") 

long.term <- ggplot(mydata1,aes(yearid,Age))+
  geom_point(size=0.5)+
  geom_line()+
  scale_x_continuous(breaks=c(1955,1980,2000,2023))+
  labs (x="Year",
        y="Global median age")+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=8))+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))

tiff(file="0625spatio-temporal.tif", res = 600, width = 1800, height = 1300, compression = "lzw")
long.term
dev.off()
