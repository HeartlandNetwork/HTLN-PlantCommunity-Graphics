##### Loading packages and importing data files #####
library(readr)
library(dplyr)
library(ggplot2)
library (ggsci) #package needed to use the color palette below in ggplot that is inspired by colors used in scientific journals
library(tidyr) # I used this package to convert wide data frame to a long 


WICR_GroundCover <- read_csv("GroundCover.csv", 
  col_types = cols(Year = col_factor(levels = c("1998","1999", "2000", "2003", "2004", "2006", "2007", "2017", "2020")), CoverType = col_factor(levels = c("Bare Soil", "Bare Rock", "Grass Litter", "Leaf Litter", "Woody Debris", "Unvegetated"))))


WICR_GroundCover2 <- group_by(WICR_GroundCover, Year, CoverType) %>%
  summarise(CoverMean = mean(Cover), SD_Cover = sd(Cover))%>%
  mutate(CI95=(1.959964*(SD_Cover/sqrt(4)))) %>%
  mutate(CI95high=CoverMean +(1.959964*(SD_Cover/sqrt(4)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=CoverMean - (1.959964*(SD_Cover/sqrt(4)))) #Calculates lower limit of 95% Confidence Interval
#gives the total mean basal area for each year


ggplot(WICR_GroundCover2, aes(y=CoverMean, x=CoverType, fill=Year)) +
  ylab("Mean Cover (%)") +
  xlab("Cover Type") +
  geom_bar(stat="identity", position="dodge", color= "black" )+
  scale_fill_manual(breaks = c("1998", "1999", "2000", "2003", "2004", "2006", "2007", "2017", "2020"),
                    values=c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f5f5f5", "#c7eae5", "#80cdc1", "#35978f", "#01665e")) +
  geom_errorbar(aes(ymin=pmax(CI95low, 0), ymax=CI95high), width=.2,
                position=position_dodge(.9), color="black")+
  annotate("text", x = 1.3, y = 90, label = "Error Bars: 95% CI")  +
  theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) 











