##### Loading packages and importing data files #####
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

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
            TotalRegen= Seedling + SmSapling + LgSapling)# Sums the seedling density per Hectare by Year, LocationID, and Regen Size Class (and also for calculates for all regen sizes)

write.table(RegenDensity, "Text Output/RegenDensity.txt", sep="\t") #Did not need to export data to clean in Excel because all year and locationID combinations were already present in data set
  
TotalRegenDensity <- group_by(RegenDensity, Year)%>%
  summarise(Total = mean(TotalRegen), SDTotal = sd(TotalRegen)) %>%
  mutate(CI95=(1.959964*(SDTotal/sqrt(7)))) %>%
  mutate(CI95high= Total +(1.959964*(SDTotal/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= Total - (1.959964*(SDTotal/sqrt(7))))

write.table(TotalRegenDensity, "Text Output/TotalRegenDensity.txt", sep="\t")

ggplot(TotalRegenDensity, aes(y=Total, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 30000, 5000), expand = c(0, 0), limits = c(0, 30000)) +
  annotate("text", x = 1.1, y = 26000, label = "Error Bars: 95% CI")+
  labs(y=expression("Total Regeneration Density (stems/ha)"))

##### Seedling Regeneration Density #####
SeedlingDensity <- group_by(RegenDensity, Year)%>%
  summarise(MeanSeedling = mean(Seedling), SDSeedling = sd(Seedling)) %>%
  mutate(CI95=(1.959964*(SDSeedling/sqrt(7)))) %>%
  mutate(CI95high= MeanSeedling +(1.959964*(SDSeedling/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= MeanSeedling - (1.959964*(SDSeedling/sqrt(7))))

write.table(SeedlingDensity, "Text Output/SeedlingDensity.txt", sep="\t")

ggplot(SeedlingDensity, aes(y=MeanSeedling, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 30000, 5000), expand = c(0, 0), limits = c(0, 30000)) +
  annotate("text", x = 1.1, y = 26000, label = "Error Bars: 95% CI")+
  labs(y=expression("Seedling Density (stems/ha)"))


##### Small Sapling Regeneration Density #####
SmSaplingDensity <- group_by(RegenDensity, Year)%>%
  summarise(MeanSmSapling = mean(SmSapling), SDSmSapling = sd(SmSapling)) %>%
  mutate(CI95=(1.959964*(SDSmSapling/sqrt(7)))) %>%
  mutate(CI95high= MeanSmSapling +(1.959964*(SDSmSapling/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= MeanSmSapling - (1.959964*(SDSmSapling/sqrt(7))))

write.table(SmSaplingDensity, "Text Output/SmSaplingDensity.txt", sep="\t")

ggplot(SmSaplingDensity, aes(y=MeanSmSapling, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=pmax(MeanSmSapling-CI95,0), ymax=MeanSmSapling+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 1500, 250), expand = c(0, 0), limits = c(0, 1500)) +
  annotate("text", x = 1.1, y = 1300, label = "Error Bars: 95% CI")+
  labs(y=expression("Small Sapling Density (stems/ha)"))

##### Large Sapling Regeneration Density #####
LgSaplingDensity <- group_by(RegenDensity, Year)%>%
  summarise(MeanLgSapling = mean(LgSapling), SDLgSapling = sd(LgSapling)) %>%
  mutate(CI95=(1.959964*(SDLgSapling/sqrt(7)))) %>%
  mutate(CI95high= MeanLgSapling +(1.959964*(SDLgSapling/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= MeanLgSapling - (1.959964*(SDLgSapling/sqrt(7))))

write.table(LgSaplingDensity, "Text Output/LgSaplingDensity.txt", sep="\t")

ggplot(LgSaplingDensity, aes(y=MeanLgSapling, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=pmax(MeanLgSapling-CI95,0), ymax=MeanLgSapling+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 200, 50), expand = c(0, 0), limits = c(0, 200)) +
  annotate("text", x = 1.1, y = 180, label = "Error Bars: 95% CI")+
  labs(y=expression("Large Sapling Density (stems/ha)"))

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

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)

##### Small and Large Saplings in one graph SECOND METHOD #######
PERI_SmallLargeSapling <- read_csv("PERI_SmallLargeSapling.csv", 
                                   col_types = cols(Year = col_factor(levels = c("2007", 
                                                                                 "2012", "2016", "2021")), `Sapling Size` = col_factor(levels = c("Small Sapling", 
                                                                                                                                                  "Large Sapling"))))
View(PERI_SmallLargeSapling)

ggplot(PERI_SmallLargeSapling, aes(y=Mean, x=Year, fill=Size)) +
  geom_bar(stat="identity", position="dodge", color= "black")+
  scale_fill_manual(breaks = c("Small Saplings", "Large Saplings"),
                    values=c("#d8b365", "#5ab4ac")) +
  geom_errorbar(aes(ymin=pmax(Mean-CI95,0), ymax=Mean+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression("Sapling Density (stems/ha)")) +
  annotate("text", x = 1.0, y = 1210, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1500), breaks=seq(0,1500,250))+
  theme_bw()  

##### Total Juniper Regeneration Density #####
Juniper_Regeneration <- read_csv("Juniper_Regeneration.csv", col_types = cols(Year = col_factor(levels = c("2007", "2012", "2016","2021"))))%>%
  select("Year", "LocationID", "AcceptedSpecies", "USDA_CName", "Family", "SumOfSeedling","SumOfSmallSapling", "SumOfLargeSapling") %>%
  rename(Seedling=SumOfSeedling, SmSapling=SumOfSmallSapling, LgSapling=SumOfLargeSapling) %>%
  mutate(SeedlingHA=Seedling/0.01,
         SmSaplingHA=SmSapling/0.01,
         LgSaplingHA=LgSapling/0.01)  #to read in files you only need file name as long as the file is your working directory, otherwise the full file path is required; makes the year and class vectors a factor rather than numeric (I did this to make graph easier later in the code);selects only the columns that are named and the rest are removed in new tab; renames a few columns; converts density to density per hectare

write.table(Juniper_Regeneration, "Text Output/Juniper_Regeneration.txt", sep="\t") # Export to clean the data by adding zeros for all size class, site, and year combinations

Juniper_Regeneration_Clean <- read_csv("Juniper_Regeneration_Clean.csv", col_types = cols(Year = col_factor(levels = c("2007","2012", "2016", "2021"))))%>%
  mutate (TotalRegenHA= SeedlingHA + SmSaplingHA + LgSaplingHA) #reads in cleaned up csv file BA_YearSiteSize_Clean.csv that includes added zeroes. Makes year and size a factor, which helps to graph later in the code;Sums the regen density of all sizes

TotalJuniperRegen <- group_by(Juniper_Regeneration_Clean, Year)%>%
  summarise(Total = mean(TotalRegenHA), SDTotal = sd(TotalRegenHA)) %>%
  mutate(CI95=(1.959964*(SDTotal/sqrt(7)))) %>%
  mutate(CI95high= Total +(1.959964*(SDTotal/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= Total - (1.959964*(SDTotal/sqrt(7))))

write.table(TotalJuniperRegen, "Text Output/TotalJuniperRegen.txt", sep="\t")

ggplot(TotalJuniperRegen, aes(y=Total, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=pmax(Total-CI95,0), ymax=Total+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 800, 200), expand = c(0, 0), limits = c(0, 800)) +
  annotate("text", x = 1.1, y = 770, label = "Error Bars: 95% CI")+
  labs(y=expression("Total Regeneration Density (stems/ha)"))

##### Juniper Seedling Regeneration Density #####
JuniperSeedling <- group_by(Juniper_Regeneration_Clean, Year)%>%
  summarise(Seedling = mean(SeedlingHA), SDSeedling = sd(SeedlingHA)) %>%
  mutate(CI95=(1.959964*(SDSeedling/sqrt(7)))) %>%
  mutate(CI95high= Seedling +(1.959964*(SDSeedling/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= Seedling - (1.959964*(SDSeedling/sqrt(7))))

write.table(JuniperSeedling, "Text Output/JuniperSeedling.txt", sep="\t")

ggplot(JuniperSeedling, aes(y=Seedling, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=pmax(Seedling-CI95,0), ymax=Seedling+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 700, 200), expand = c(0, 0), limits = c(0, 700)) +
  annotate("text", x = 1.9, y = 625, label = "Error Bars: 95% CI")+
  labs(y=expression("Seedling Density (stems/ha)"))


##### Juniper Small Sapling Regeneration Density #####
JuniperSmSapling <- group_by(Juniper_Regeneration_Clean, Year)%>%
  summarise(SmSapling = mean(SmSaplingHA), SDSmSapling = sd(SmSaplingHA)) %>%
  mutate(CI95=(1.959964*(SDSmSapling/sqrt(7)))) %>%
  mutate(CI95high= SmSapling +(1.959964*(SDSmSapling/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low= SmSapling - (1.959964*(SDSmSapling/sqrt(7))))

write.table(JuniperSmSapling, "Text Output/JuniperSmSapling.txt", sep="\t")

ggplot(JuniperSmSapling, aes(y=SmSapling, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=pmax(SmSapling-CI95,0), ymax=SmSapling+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 250, 50), expand = c(0, 0), limits = c(0, 250)) +
  annotate("text", x = 1.1, y = 220, label = "Error Bars: 95% CI")+
  labs(y=expression("Small Sapling Density (stems/ha)"))

##### NO Juniper Large Sapling Regeneration #####
