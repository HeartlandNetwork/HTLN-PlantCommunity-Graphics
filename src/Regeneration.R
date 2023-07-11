##### Loading packages and importing data files #####
library(readr)
library(dplyr)
library(ggplot2)
library (ggsci) #package needed to use the color palette below in ggplot that is inspired by colors used in scientific journals
library(tidyr) # I used this package to convert wide data frame to a long 

WICR_Regeneration <- read_csv("Regen2.csv") #to read in files you only need file name as long as the file is your working directory, otherwise the full file path is required

##### Filtering to keep only the years and sites we need, converting density to hectare, and removing unneeded columns #####
WICR_RegenDensity <- WICR_Regeneration %>%
  filter(Year %in% c("1998", "1999", "2000", "2003", "2004", "2006", "2007", "2017", "2020")) %>%
  filter(LocationID %in% c("WICRVegMon4", "WICRVegMon5", "WICRVegMon6", "WICRVegMon7")) %>%
  mutate(SeedlingHA=SumOfSeedling/0.01,
         SmallSaplingHA=SumOfSmallSapling/0.01,
         LargeSaplingHA=SumOfLargeSapling/0.01) %>%
  mutate(Year=as.factor(Year)) %>% #makes the year vector a factor rather than numeric (I did this to make graph easier later in the code)
  select("Year", "LocationID", "AcceptedSpecies", "USDA_CName", "Family", "SeedlingHA", "SmallSaplingHA", "LargeSaplingHA") 

##### Regeneration Density by Species and Size Class #####
write.table(WICR_RegenDensity, "Output/WICR_RegenDensity.txt", sep="\t") #saves the WICR_RegenDensity table to a text file....I used this to further clean the data in Excel before re-importing as the "WICR_Regeneration2Clean" data frame

WICR_RegenDensityClean <- read_csv("WICR_RegenDensityClean.csv") #reads in the "WICR_RegenDensityClean" data frame that I have saved as csv file

WICR_RegenDensityClean2 <- WICR_RegenDensityClean %>%
  group_by(Year, AcceptedSpecies)%>%
  summarise(MeanSeedling= mean(SeedlingHA), SDSeedling=sd(SeedlingHA), MeanSmallSapling= mean(SmallSaplingHA), SDSmallSapling=sd(SmallSaplingHA), MeanLargeSapling= mean(LargeSaplingHA), SDLargeSapling=sd(LargeSaplingHA))%>%
  mutate(SeedlingCI95=(1.959964*(SDSeedling/sqrt(4))), SmallSaplingCI95=(1.959964*(SDSmallSapling/sqrt(4))), LargeSaplingCI95=(1.959964*(SDLargeSapling/sqrt(4)))) 
#calculates the mean density and 95% confidence interval by year and Accepted Species for each size class

WICR_RegenDensityClean3<- WICR_RegenDensityClean2 %>% 
  gather(key, value, -AcceptedSpecies, -Year) %>%  
  unite(new.col, c(key, Year)) %>%   
  spread(new.col, value) #Converts our long data frame to a wide (eg, Year was its own column, and now the means for each year are their own columns). This is the table I exported and put into excel for final table

write.table(WICR_RegenDensityClean3, "Output/WICR_RegenDensityClean3.txt", sep="\t") #saves the WICR_RegenDensityClean3 table to a text file   


##### Regeneration stems per Hectare grouped by Year, LocationID, and Regen Size Class #####
WICR_Regeneration3 <- group_by(WICR_Regeneration2, Year, LocationID)  %>%
  summarise(Seedling = sum(SeedlingHA),
            SmallSapling= sum(SmallSaplingHA),
            LargeSapling=sum(LargeSaplingHA),
            TotalRegen= Seedling + SmallSapling + LargeSapling)# Sums the seedling density per Hectare by Year, LocationID, and Regen Size Class (and also for calculates for all regen sizes)

WICR_Regeneration4 <- group_by(WICR_Regeneration3, Year)%>%
  summarise(MeanTotalRegen = mean(TotalRegen), SDTotalRegen = sd(TotalRegen)) %>%
  mutate(TotalRegenCI95=(1.959964*(SDTotalRegen/sqrt(4))))

WICR_Regeneration5 <- group_by(WICR_Regeneration3, Year)%>%
  summarise(MeanSeedling = mean(Seedling), SDSeedling = sd(Seedling),
            MeanSmallSapling= mean(SmallSapling), SDSmallSapling= sd(SmallSapling), 
            MeanLargeSapling= mean(LargeSapling), SDLargeSapling= sd(LargeSapling))%>%
  mutate(SeedlingCI95=(1.959964*(SDSeedling/sqrt(4)))) %>%
  mutate(SmallSaplingCI95=(1.959964*(SDSmallSapling/sqrt(4)))) %>%
  mutate(LargeSaplingCI95=(1.959964*(SDLargeSapling/sqrt(4))))
#calculates the mean regen density, standard deviation, and 95% confidence interval by regen size class and year


##### Graphs #####
ggplot(WICR_Regeneration4, aes(y=MeanTotalRegen, x=Year, fill=Year)) +
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(breaks = c("1998", "1999", "2000", "2004", "2006", "2007", "2017", "2020"),
                    values=c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")) +
  geom_errorbar(aes(ymin=MeanTotalRegen-TotalRegenCI95, ymax=MeanTotalRegen+TotalRegenCI95), width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year")+
  scale_y_continuous (name="Mean Density (stems/ha)")+
  annotate("text", x = 8.3, y = 29000, label = "Error Bars: 95% CI") +
  theme_bw()  #creates a graph of the density of Total Regen, error bars are 95% Confidence Interval, Graph with color bars

ggplot(WICR_Regeneration5, aes(y=MeanSeedling, x=Year, fill=Year)) +
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(breaks = c("1998", "1999", "2000", "2004", "2006", "2007", "2017", "2020"),
                    values=c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")) +
  geom_errorbar(aes(ymin=MeanSeedling-SeedlingCI95, ymax=MeanSeedling+SeedlingCI95), width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year")+
  scale_y_continuous (name="Mean Seedling Density (stems/ha)")+
  annotate("text", x = 8.3, y = 29000, label = "Error Bars: 95% CI") +
  theme_bw()  #creates a graph of the density of Seedlings, error bars are 95% Confidence Interval, Graph with color bars

ggplot(WICR_Regeneration5, aes(y=MeanSmallSapling, x=Year, fill=Year)) +
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(breaks = c("1998", "1999", "2000", "2004", "2006", "2007", "2017", "2020"),
                    values=c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")) +
  geom_errorbar(aes(ymin=MeanSmallSapling-SmallSaplingCI95, ymax=MeanSmallSapling+SmallSaplingCI95), width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year")+
  scale_y_continuous (name="Mean Small Sapling Density (stems/ha)")+
  annotate("text", x = 1.6, y = 6200, label = "Error Bars: 95% CI") +
  theme_bw()  #creates a graph of the density of Small Saplings, error bars are 95% Confidence Interval, Graph with color bars

ggplot(WICR_Regeneration5, aes(y=MeanLargeSapling, x=Year, fill=Year)) +
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(breaks = c("1998", "1999", "2000", "2004", "2006", "2007", "2017", "2020"),
                    values=c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")) +
  geom_errorbar(aes(ymin=MeanLargeSapling-LargeSaplingCI95, ymax=MeanLargeSapling+LargeSaplingCI95), width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year")+
  scale_y_continuous (name="Mean Density (stems/ha)")+
  annotate("text", x = 1.6, y = 1600, label = "Error Bars: 95% CI") +
  theme_bw()  #creates a graph of the density of Large Saplings, error bars are 95% Confidence Interval


