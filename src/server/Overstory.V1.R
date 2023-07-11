##### Loading packages and importing data files #####
library(readr)
library(dplyr)
library(ggplot2)
library (ggsci) #package needed to use the color palette below in ggplot that is inspired by colors used in scientific journals
library(tidyr) # I used this package to convert wide data frame to a long 

WICR_Overstory <- read_csv("WICR_Overstory.csv") #to read in files you only need file name as long as the file is your working directory, otherwise the full file path is required
SamplingPeriod <- read_csv("SamplingPeriod.csv")
Canopy <- read_csv("canopy.csv")
Species_LU <- read_csv("Species_LU.csv")

###### Filtering to keep live trees only and calculating Basal Area #####
WICR_Overstory2 <- WICR_Overstory %>%
  filter(Condition == "L", SpeciesCode!= "SNAG")%>%   #filter to include only Live trees
  mutate(SizeClass = case_when(DBH < 15 ~ '1',     
                            DBH < 25 ~ '2',
                            DBH < 35 ~ '3' ,
                            DBH < 45 ~ '4' ,
                            DBH > 45 ~ '5')) %>%  # creates new Size Class column based on DBH
  mutate(BasalArea = (DBH^2)*0.00007854)%>%  # calculates and creates new Basal Area column based on DBH
  mutate(BasalAreaHA = BasalArea/0.1)   # calculates and creates new Basal Area per Hectare column based on Basal Area
  
##### Basal Area per Hectare grouped by Year, LocationID, and Size Class #####

WICR_Overstory3 <- left_join(WICR_Overstory2, SamplingPeriod, by="PeriodID") %>% #Merges the working data frame with the Sampling Period table to be able to group by year in next function
  filter(Year!= 2012) %>% #this filter excludes anything from 2012
  mutate(Year=as.factor(Year)) %>% #makes the year vector a factor rather than numeric (I did this to make graph easier later in the code)
  select("Year", "LocationID",  "PeriodID", "SpeciesCode", "IsCoppic", "DBH", "Condition", "SizeClass", "BasalArea", "BasalAreaHA") #this selects only the columns that are named and the rest are removed in new table

BA_Summary_YearSiteSize <- group_by(WICR_Overstory3, Year, LocationID, SizeClass)  %>%
  summarise(BA = sum(BasalAreaHA)) # Sums the Basal Area per Hectare by Year, LocationID, and Size Class

BA_Summary_YearSite <-group_by(BA_Summary_YearSiteSize, Year, LocationID) %>%
  summarise(SiteBA = sum(BA)) #Sum the Basal Area (of all size classes) for each site by year

BA_Summary_Year <-group_by(BA_Summary_YearSite, Year) %>%
  summarise(YearBA = mean(SiteBA), SD_YearBA = sd(SiteBA)) %>%
  mutate(CI95=(1.959964*(SD_YearBA/sqrt(4)))) %>%mutate(CI95high=YearBA +(1.959964*(SD_YearBA/sqrt(4)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearBA - (1.959964*(SD_YearBA/sqrt(4)))) #Calculates lower limit of 95% Confidence Interval
  #gives the total basal area for each year

write.table(BA_Summary_Year, "BA_Summary_Year.txt", sep="\t") #saves the BA_Summary_Year table to a text file

BA_Summary_YearSize <- group_by(BA_Summary_YearSiteSize, SizeClass, Year) %>%
  summarise(YearBA=mean(BA), SD_YearBA = sd(BA)) %>%
  mutate(CI95high=YearBA +(1.959964*(SD_YearBA/sqrt(4)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearBA - (1.959964*(SD_YearBA/sqrt(4)))) #Calculates lower limit of 95% Confidence Interval
#gives the mean and standard deviation for Basal Area by year and size claSS. In other words, averages each of the four basal area values (from each of the four sites) for each year

ggplot(BA_Summary_YearSize, aes(y=YearBA, x=Year, fill=SizeClass)) +
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                    values=c("#543005", "#bf812d", "#f6e8c3", "#80cdc1", "#01665e")) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2003", "2006", "2017", "2020")) +
  scale_y_continuous ()+
  labs(y=expression("Basal Area"~(m^2/ha))) + 
  annotate("text", x = 3.8, y = 12.2, label = "Error Bars: 95% CI") +
  theme_bw() #creates the graph with year on x-axis and basal area on the y-axis. Graphs shows basal area of each size class for each year

##### Canopy #####

CanopySummary1 <- Canopy %>%
  filter(LocationID != "WICRVegMon14", LocationID!="WICRVegMon15",  LocationID!="WICRVegMon16",  LocationID!="WICRVegMon17", LocationID!="WICRVegMon18", LocationID!="WICRVegMon19")%>% 
  filter(Year!= 2001, Year!= 2012) %>% # 
  rename(C1= `SumOfValue 1`,  C2= `SumOfValue 2`, C3=`SumOfValue 3`, C4=`SumOfValue 4` ) %>%
  mutate(Canopy1=C1*1.04, Canopy2=C2*1.04, Canopy3=C3*1.04, Canopy4= C4*1.04) %>% 
  group_by (Year, LocationID, Plot) %>%
  mutate(PlotMean = mean(c(Canopy1, Canopy2, Canopy3, Canopy4)))%>%
  mutate(Year=as.factor(Year)) #this function starts by filtering out the prairie sites and year 2001 and 2012; renames the canopy values, creates new column by calculating the actual canopy value by multiplying by 1.04; creates new column of plot means; makes the year vector a factor rather than numeric (I did this to make graph easier later in the code)


CanopySummary2 <- CanopySummary1 %>%
  select(Year, LocationID, Plot, PlotMean) %>%
  group_by(Year, LocationID) %>%
  summarize (SiteMean= mean(PlotMean), SiteSD= sd(PlotMean))
  #further cleans data by removing unneeded columns;calculates the site mean and standard deviation in new columns

CanopySummary3 <- CanopySummary2 %>%
  group_by(Year) %>%
  summarize(YearMean= mean(SiteMean), YearSD= sd(SiteMean))%>%  
  mutate(CI95high=YearMean +(1.959964*(YearSD/sqrt(4)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearMean - (1.959964*(YearSD/sqrt(4)))) #Calculates lower limit of 95% Confidence Interval
  #Calculates the mean of four sites for each year in new column

ggplot(CanopySummary3, aes(y=YearMean, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2003", "2006", "2017", "2020"), labels=c("2003", "2006", "2017", "2020"))+
  scale_y_continuous (name="Mean Canopy Closure (%)", breaks=seq(0,100, 25), limits=c(0, 100))+
  theme_bw() +
  annotate("text", x = 0.8, y = 95, label = "Error Bars: 95% CI") #creates a graph of the canopy mean for each year, error bars are 95% Confidence Interval


write.table(CanopySummary1, "CanopySummary1.txt", sep="\t") #saves the CanopySummary1 table to a text file  
write.table(CanopySummary2, "CanopySummary2.txt", sep="\t") #saves the CanopySummary2 table to a text file

##### Tree Density by Size Class #####
TreeDensity1<-WICR_Overstory %>%
  filter(Condition == "L", SpeciesCode!= "SNAG")%>%   #filter to include only Live trees
  mutate(SizeClass = case_when(DBH < 15 ~ '1',     
                               DBH < 25 ~ '2',
                               DBH < 35 ~ '3' ,
                               DBH < 45 ~ '4' ,
                               DBH > 45 ~ '5')) # creates new Size Class column based on DBH

TreeDensity2 <- left_join(TreeDensity1, SamplingPeriod, by="PeriodID")%>% #Merges the working data frame with the Sampling Period table to be able to group by year in next function
  select("Year", "LocationID",  "PeriodID", "SpeciesCode", "IsCoppic", "DBH", "Condition", "SizeClass") %>% #removes unneeded columns
  filter(Year!= 2012)%>% #filters out 2012
  mutate(Year=as.factor(Year)) #makes the year vector a factor rather than numeric (I did this to make graph easier later in the code)

TreeDensity3 <- TreeDensity2  %>%
  group_by(Year, LocationID, SizeClass) %>%
  tally()%>%
  mutate(SiteDensity=n/0.1) #calculates the stem density per hectare by size class and year for each site

TreeDensity4 <- TreeDensity3  %>% 
  group_by(Year, SizeClass) %>%
  summarise(MeanSizeClassDensity=mean(SiteDensity), SD=sd(SiteDensity)) %>%
  mutate(CI95=(1.959964*(SD/sqrt(4)))) %>%
  mutate(CI95high=MeanSizeClassDensity +(1.959964*(SD/sqrt(4)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=MeanSizeClassDensity - (1.959964*(SD/sqrt(4)))) #Calculates lower limit of 95% Confidence Interval
#calculates the mean stem density, standard deviation, and 95% confidence interval by size class and year
  
TreeDensityAllSizes <- TreeDensity3 %>%
  group_by(Year, LocationID)%>%
  summarise(DensityAllSizes=sum(SiteDensity))
  #Total stem density (all size classes combined) for each year and site

write.table(TreeDensityAllSizes, "TreeDensityAllSizes.txt", sep="\t") #saves the TreeDensityAllSizes table to a text file

MeanTreeDensityAllSizes <- TreeDensityAllSizes %>%
  group_by(Year)%>%
  summarise(MeanDensityAllSizes=mean(DensityAllSizes), SD=sd(DensityAllSizes)) %>%
  mutate(CI95=(1.959964*(SD/sqrt(4)))) %>%
  mutate(CI95high=MeanDensityAllSizes +(1.959964*(SD/sqrt(4)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=MeanDensityAllSizes - (1.959964*(SD/sqrt(4)))) #Calculates lower limit of 95% Confidence Interval
#Mean stem density (all size classes combined), standard deviation, and 95% confidence interval for each year

write.table(MeanTreeDensityAllSizes, "MeanTreeDensityAllSizes.txt", sep="\t") #saves the MeanTreeDensityAllSizes table to a text file

##### Tree Density by Species #####
library(reshape2) #need this package to convert wide data frame to long

SpeciesTreeDensity <- TreeDensity2 %>%
  group_by(Year, LocationID, SpeciesCode) %>%
  tally() %>%
  mutate(HADensity=n/0.1) %>%
  select("Year" , "LocationID" , "SpeciesCode" , "HADensity")
  #calculates the stem density per hectare by Year, site, and speciescode


write.table(SpeciesTreeDensity, "SpeciesTreeDensity.txt", sep="\t") #saves the SpeciesTreeDensity table to a text file....I used this to further clean the data in Excel before re-importing as the "SpeciesTreeDensityClean" data frame


SpeciesTreeDensityClean <- read_csv("SpeciesTreeDensityClean.csv") #reads in the "SpeciesTreeDensityClean" data frame that I have saved as csv file

SpeciesTreeDensityClean2 <- SpeciesTreeDensityClean %>%
  group_by(Year, SpeciesCode)%>%
  summarise(MeanDensity= mean(HADensity), SDDensity=sd(HADensity))%>%
  mutate(CI95=(1.959964*(SDDensity/sqrt(4)))) %>%
  mutate(CI95high=MeanDensity +(1.959964*(SDDensity/sqrt(4)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=MeanDensity - (1.959964*(SDDensity/sqrt(4)))) #Calculates lower limit of 95% Confidence Interval
#calculate the mean stem density and 95% confidence interval by year and Speciescode


Species_LU <- read_csv("Species_LU.csv") #imports the the Species lookup table I have saved as a csv file
Species_LU2 <- Species_LU %>%
  select(SpeciesCode=AcceptedSymbol, AcceptedSpecies, ScientificName, USDA_CName, Family) # cleans up the species lookup table by removing the unneeded columns from the table 

SpeciesTreeDensityClean3 <- left_join (SpeciesTreeDensityClean2, Species_LU2, by= "SpeciesCode") # merges the cleaned up species look-up table with the "SpeciesTreeDensityClean2" table, which has the mean stem density for each SpeciesCode and year. The join is done by using the SpeciesCode as common columns from each table

SpeciesTreeDensityClean4 <- SpeciesTreeDensityClean3 %>%
  select("Year", "Species" = "AcceptedSpecies", "MeanDensity", "CI95") #further cleans up the "SpeciesTreeDensityClean3" table by only keeping the columns we need

SpeciesTreeDensityClean5 <- SpeciesTreeDensityClean4 %>% 
  gather(key, value, -Species, -Year) %>%  
  unite(new.col, c(key, Year)) %>%   
  spread(new.col, value) #Converts our long data frame to a wide (eg, Year was its own column, and now the means for each year are their own columns). This is the table I exported and put into excel for final table

write.table(SpeciesTreeDensityClean2, "SpeciesTreeDensityClean2.txt", sep="\t")
write.table(SpeciesTreeDensityClean3, "SpeciesTreeDensityClean3.txt", sep="\t")
write.table(SpeciesTreeDensityClean5, "SpeciesTreeDensityClean5.txt", sep="\t")

##### Tree Density Graphs #####
ggplot(TreeDensity4, aes(y=MeanSizeClassDensity, x=Year, fill=SizeClass)) +
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(breaks = c("1", "2", "3", "4", "5"), 
                   values=c("#543005", "#bf812d", "#f6e8c3", "#80cdc1", "#01665e")) +
  geom_errorbar(aes(ymin=MeanSizeClassDensity-CI95, ymax=MeanSizeClassDensity+CI95), width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2003", "2006", "2017", "2020"))+
  scale_y_continuous (name="Tree Density (stems/ha)")+
  annotate("text", x = 0.9, y = 1400, label = "Error Bars: 95% CI")+
  theme_bw()  #creates a graph of the density of each size class for each year, error bars are 95% Confidence Interval

ggplot(MeanTreeDensityAllSizes, aes(y=MeanDensityAllSizes, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=MeanDensityAllSizes-CI95, ymax=MeanDensityAllSizes+CI95), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2003", "2006", "2017", "2020"), labels=c("2003", "2006", "2017", "2020"))+
  scale_y_continuous (name="Tree Density (stems/ha)") +
  annotate("text", x = 0.8, y = 1450, label = "Error Bars: 95% CI")+
  theme_bw() #creates a graph of the density mean for each year, error bars are 95% Confidence Interval



