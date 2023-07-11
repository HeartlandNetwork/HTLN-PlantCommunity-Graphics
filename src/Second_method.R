##### Small and Large Saplings in one graph SECOND METHOD #######
library(tidyverse)
library(gridExtra)
library (ggsci)


setwd("C:\\Users\\growell\\work\\MyRProjects\\Mary\\src")


##### Small and Large Saplings in one graph SECOND METHOD #######
PERI_SmallLargeSapling <- read_csv("PERI_SmallLargeSapling.csv", 
                                   col_types = cols(Year = col_factor(levels = c("2007", "2012", "2016", "2021")), 
                                                    `Sapling Size` = col_factor(levels = c("Small Sapling","Large Sapling")))) 
View(PERI_SmallLargeSapling)

ggplot(PERI_SmallLargeSapling, aes(y=Mean, x=Year)) +
  geom_bar(stat="identity", position="dodge", color= "black")+
  scale_fill_manual(breaks = c("Small Saplings", "Large Saplings"),
                    values=c("#d8b365", "#5ab4ac")) +
  geom_errorbar(aes(ymin=pmax(Mean-CI95,0), ymax=Mean+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression("Sapling Density (stems/ha)")) +
  annotate("text", x = 1.0, y = 1210, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1500), breaks=seq(0,1500,250))+
  theme_bw()  

