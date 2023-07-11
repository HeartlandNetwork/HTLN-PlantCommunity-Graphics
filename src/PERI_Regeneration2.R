##### Loading packages and importing data files #####
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(cowplot)

setwd("C:\\Users\\GRowell\\work\\MyRProjects\\Mary")

PERI_Regeneration <- read_csv("PERI_Regeneration.csv", col_types = cols(Year = col_factor(levels = c("2007", "2012", "2016","2021"))))%>%
  select("Year", "LocationID", "AcceptedSpecies", "USDA_CName", "Family", "SumOfSeedling","SumOfSmallSapling", "SumOfLargeSapling") %>%
  rename(Seedling=SumOfSeedling, SmSapling=SumOfSmallSapling, LgSapling=SumOfLargeSapling) %>%
  mutate(SeedlingHA=Seedling/0.01,
         SmSaplingHA=SmSapling/0.01,
         LgSaplingHA=LgSapling/0.01) #to read in files you only need file name as long as the file is your working directory, otherwise the full file path is required; makes the year and class vectors a factor rather than numeric (I did this to make graph easier later in the code);selects only the columns that are named and the rest are removed in new tab; renames a few columns; converts density to density per hectare
 

##### Total Regeneration Density #####
RegenDensity <- group_by(PERI_Regeneration, Year, LocationID)  %>%
  summarise(Seedling = sum(SeedlingHA),
            SmSapling= sum(SmSaplingHA),
            LgSapling=sum(LgSaplingHA),
            TotalRegen= Seedling + SmSapling + LgSapling)
# Sums the seedling density per Hectare by Year, LocationID, and Regen Size Class (and also for calculates for all regen sizes)


  
##### Small Sapling Regeneration Density #####
SmSaplingDensity <- group_by(RegenDensity, Year)%>%
  summarise(MeanSmSapling = mean(SmSapling), SDSmSapling = sd(SmSapling)) %>%
  mutate(CI95=(1.959964*(SDSmSapling/sqrt(7)))) %>%
  mutate(CI95high= MeanSmSapling +(1.959964*(SDSmSapling/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= MeanSmSapling - (1.959964*(SDSmSapling/sqrt(7))))


##### Large Sapling Regeneration Density #####
LgSaplingDensity <- group_by(RegenDensity, Year)%>%
  summarise(MeanLgSapling = mean(LgSapling), SDLgSapling = sd(LgSapling)) %>%
  mutate(CI95=(1.959964*(SDLgSapling/sqrt(7)))) %>%
  mutate(CI95high= MeanLgSapling +(1.959964*(SDLgSapling/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= MeanLgSapling - (1.959964*(SDLgSapling/sqrt(7))))

##### Small and Large Saplings in one graph #######
p1<- ggplot(SmSaplingDensity, aes(y=MeanSmSapling, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=pmax(MeanSmSapling-CI95,0), ymax=MeanSmSapling+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 1500, 250), expand = c(0, 0), limits = c(0, 1500)) +
  annotate("text", x = 2, y = 1300, label = "Error Bars: 95% CI")+
  labs(y=expression("Small Sapling Density (stems/ha)"))

p2<-ggplot(LgSaplingDensity, aes(y=MeanLgSapling, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=pmax(MeanLgSapling-CI95,0), ymax=MeanLgSapling+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 1500, 250), expand = c(0, 0), limits = c(0, 1500)) +
  annotate("text", x = 2, y = 1300, label = "Error Bars: 95% CI")+
  labs(y=expression("Large Sapling Density (stems/ha)"))



plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1) #

#ggsave(filename = "Sapling Density.png") 


SaplingDensity <- read_csv("SaplingDensity.csv")

SaplingDensity


# rectangle
ggplot(SaplingDensity) +
  geom_bar( aes(x=SaplingSize, y=MeanSapling), stat="identity", fill="skyblue", alpha=0.5) +
  geom_crossbar( aes(x=SaplingSize, y=MeanSapling, ymin=CI95low, ymax=CI95high), width=0.4, colour="orange", alpha=0.9, size=1.3)












