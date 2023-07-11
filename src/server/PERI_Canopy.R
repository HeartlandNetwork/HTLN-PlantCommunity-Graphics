##### Loading packages and importing data files #####
library(readr)
library(dplyr)
library(ggplot2)

Canopy <- read_csv("Canopy.csv", col_types = cols(Year = col_factor(levels = c("2007", "2012", "2016","2021")))) %>%
  mutate(Canopy1=C1*1.04, Canopy2=C2*1.04, Canopy3=C3*1.04, Canopy4= C4*1.04) %>% 
  group_by (Year, LocationID, Plot) %>%
  mutate(PlotMean = mean(c(Canopy1, Canopy2, Canopy3, Canopy4)))%>%
  group_by(Year, LocationID) %>%
  summarize (SiteMean= mean(PlotMean), SiteSD= sd(PlotMean)) %>%
  group_by(Year) %>%
  summarize (YearMean= mean(SiteMean), SiteSD= sd(SiteMean)) %>%
  mutate(CI95=(1.959964*(SiteSD/sqrt(7)))) %>%
  mutate(CI95high= YearMean +(1.959964*(SiteSD/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= YearMean - (1.959964*(SiteSD/sqrt(7))))

ggplot(Canopy, aes(y=YearMean, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=pmax(YearMean-CI95,0), ymax=YearMean+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 110, 20), expand = c(0, 0), limits = c(0, 110)) +
  annotate("text", x = 1.1, y = 102, label = "Error Bars: 95% CI")+
  labs(y=expression("Canopy Cover (%)"))
  
