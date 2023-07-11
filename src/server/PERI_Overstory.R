##### Loading packages and importing data files #####
library(readr)
library(dplyr)
library(ggplot2)

PERI_LiveOverstory <- read_csv("PERI_LiveOverstory.csv", col_types = cols(Year = col_factor(levels = c("2007", "2012", "2016","2021")), Class = col_factor(levels = c("1", "2", "3", "4", "5"))))%>%
  select("Year", "LocationID", "SpeciesCode","AcceptedSpecies", "USDA_CName", "Family", "Condition", "DBH", "Class") %>%  
  mutate(BasalArea = (DBH^2)*0.00007854) %>%
  mutate(BasalAreaHA = BasalArea/0.1)
#to read in files you only need file name as long as the file is your working directory, otherwise the full file path is required; makes the year and class vectors a factor rather than numeric (I did this to make graph easier later in the code);selects only the columns that are named and the rest are removed in new tab;calculates and creates new Basal Area column based on DBH; calculates basal area per hectare

PERI_DeadOverstory <- read_csv("PERI_DeadOverstory.csv", col_types = cols(Year = col_factor(levels = c("2007", "2012", "2016","2021")), Class = col_factor(levels = c("1", "2", "3", "4", "5"))))%>%
  select("Year", "LocationID", "SpeciesCode","AcceptedSpecies", "USDA_CName", "Family", "Condition", "DBH", "Class") %>%  
  mutate(BasalArea = (DBH^2)*0.00007854) %>%
  mutate(BasalAreaHA = BasalArea/0.1)
#to read in files you only need file name as long as the file is your working directory, otherwise the full file path is required; makes the year and class vectors a factor rather than numeric (I did this to make graph easier later in the code);selects only the columns that are named and the rest are removed in new tab;calculates and creates new Basal Area column based on DBH; calculates basal area per hectare


#####Basal Area (of all size classes) for each site by year#####
BA_YearSiteSize <- group_by(PERI_LiveOverstory, Year, LocationID, Class)  %>%
  summarise(BA = sum(BasalAreaHA)) # Sums the Basal Area per Hectare by Year, LocationID, and Size Class

write.table(BA_YearSiteSize, "Text Output/BA_YearSiteSize.txt", sep="\t") # Export to clean the data by adding zeros for all size class, site, and year combinations

BA_YearSiteSize_Clean <- read_csv("BA_YearSiteSize_Clean.csv", col_types = cols(Year = col_factor(levels = c("2007","2012", "2016", "2021")), Class = col_factor(levels = c("1","2", "3", "4", "5"))))#reads in cleaned up csv file BA_YearSiteSize_Clean.csv that includes added zeroes. Makes year and size a factor, which helps to graph later in the code

BA_YearSite <-group_by(BA_YearSiteSize_Clean, Year, LocationID) %>%
  summarise(SiteBA = sum(BA))%>%
  summarise(YearBA = mean(SiteBA), SD_YearBA = sd(SiteBA))%>%
  mutate(CI95=(1.959964*(SD_YearBA/sqrt(7)))) %>%
  mutate(CI95high=YearBA +(1.959964*(SD_YearBA/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearBA - (1.959964*(SD_YearBA/sqrt(7)))) #Calculates lower limit of 95% Confidence Interval
#gives the total mean basal area for each year

write.table(BA_YearSite, "Text Output/BA_YearSite.txt", sep="\t") 

ggplot(BA_YearSite, aes(y=YearBA, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 30, 10), expand = c(0, 0), limits = c(0, 30)) +
  annotate("text", x = 1, y = 28, label = "Error Bars: 95% CI")+
  labs(y=expression("Basal Area"~(m^2/ha)))#creates a graph of mean total basal area for each year, error bars are 95% Confidence Interval


##### Basal Area per Hectare grouped by Year, LocationID, and Size Class #####
BA_YearSize <-group_by(BA_YearSiteSize_Clean, Class, Year) %>%
  summarise(SizeBA=mean(BA), SD_SizeBA = sd(BA)) %>%
  mutate(CI95high=SizeBA +(1.959964*(SD_SizeBA/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=SizeBA - (1.959964*(SD_SizeBA/sqrt(7))))%>%#Calculates lower limit of 95% Confidence Interval
  mutate(CI95=(1.959964*(SD_SizeBA/sqrt(7))))
#gives the mean and standard deviation for Basal Area by year and size claSS. In other words, averages each of the four basal area values (from each of the four sites) for each year

write.table(BA_YearSize, "Text Output/BA_YearSize.txt", sep="\t") 

ggplot(BA_YearSize, aes(y=SizeBA, x=Year, fill=Class)) +
  geom_bar(stat="identity", position="dodge", color= "black")+
  scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                    values=c("#543005", "#bf812d", "#f6e8c3", "#80cdc1", "#01665e")) +
  geom_errorbar(aes(ymin=pmax(SizeBA-CI95,0), ymax=SizeBA+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression("Basal Area"~(m^2/ha))) +
  annotate("text", x = 1.0, y = 10.8, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12), breaks=seq(0,12,2))+
  theme_bw()  #creates the graph with year on x-axis and basal area on the y-axis. Graphs shows basal area of each size class for each year


##### Density (of all size classes) for each site by year#####
Density_YearSiteSize <- group_by(PERI_LiveOverstory, Year, LocationID, Class)  %>%
  group_by(Year, LocationID, Class) %>%
  tally() %>%
  mutate(DensityHA=n/0.1) #calculates the stem density per hectare by size class and year for each site

write.table(Density_YearSiteSize, "Text Output/Density_YearSiteSize.txt", sep="\t") #saves to a text file to clean the data by adding zeros for all size class, site, and year combinations

Density_YearSiteSize_Clean <- read_csv("Density_YearSiteSize_Clean.csv", col_types = cols(Year = col_factor(levels = c("2007","2012", "2016", "2021")), Class = col_factor(levels = c("1","2", "3", "4", "5"))))#reads in cleaned up csv file Density_YearSiteSize_Clean.csv that includes added zeroes. Makes year and size a factor, which helps to graph later in the code

Density_YearSite <-group_by(Density_YearSiteSize_Clean, Year, LocationID) %>%
  summarise(SiteDensity = sum(DensityHA))%>%
  summarise(YearDensity = mean(SiteDensity), SD_YearDensity = sd(SiteDensity)) %>%
  mutate(CI95=(1.959964*(SD_YearDensity/sqrt(7)))) %>%
  mutate(CI95high=YearDensity +(1.959964*(SD_YearDensity/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearDensity - (1.959964*(SD_YearDensity/sqrt(7)))) #Calculates lower limit of 95% Confidence Interval
#gives the total mean density for each year

write.table(Density_YearSite, "Text Output/Density_YearSite.txt", sep="\t")

ggplot(Density_YearSite, aes(y=YearDensity, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 750, 100), expand = c(0, 0), limits = c(0, 750)) +
  annotate("text", x = 1.1, y = 680, label = "Error Bars: 95% CI")+
  labs(y=expression("Tree Density (stems/ha)"))#creates a graph of mean total basal area for each year, error bars are 95% Confidence Interval


##### Tree Density per Hectare grouped by Year, LocationID, and Size Class #####
Density_YearSize <-group_by(Density_YearSiteSize_Clean, Class, Year) %>%
  summarise(SizeDensity=mean(DensityHA), SD_SizeDensity = sd(DensityHA)) %>%
  mutate(CI95high=SizeDensity +(1.959964*(SD_SizeDensity/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=SizeDensity - (1.959964*(SD_SizeDensity/sqrt(7))))%>%#Calculates lower limit of 95% Confidence Interval
  mutate(CI95=(1.959964*(SD_SizeDensity/sqrt(7))))
#gives the mean and standard deviation for tree density/hectare by year and size claSS. In other words, averages each of the four density values (from each of the four sites) for each year

write.table(Density_YearSize, "Text Output/Density_YearSize.txt", sep="\t")

ggplot(Density_YearSize, aes(y=SizeDensity, x=Year, fill=Class)) +
  geom_bar(stat="identity", position="dodge", color= "black")+
  scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                    values=c("#543005", "#bf812d", "#f6e8c3", "#80cdc1", "#01665e")) +
  geom_errorbar(aes(ymin=pmax(SizeDensity-CI95,0), ymax=SizeDensity+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression("Tree Density (stems/ha)")) +
  annotate("text", x = 1.0, y = 375, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 450), breaks=seq(0,450,100))+
  theme_bw()  #creates the graph with year on x-axis and basal area on the y-axis. Graphs shows basal area of each size class for each year

##### SNAG Dead Basal Area (of all size classes) for each site by year#####
DeadBA_YearSiteSize <- group_by(PERI_DeadOverstory, Year, LocationID, Class)  %>%
  summarise(DeadBA = sum(BasalAreaHA)) # Sums the Basal Area per Hectare by Year, LocationID, and Size Class

write.table(DeadBA_YearSiteSize, "Text Output/DeadBA_YearSiteSize.txt", sep="\t") # Export to clean the data by adding zeros for all size class, site, and year combinations

DeadBA_YearSiteSize_Clean <- read_csv("DeadBA_YearSiteSize_Clean.csv", col_types = cols(Year = col_factor(levels = c("2007","2012", "2016", "2021")), Class = col_factor(levels = c("1","2", "3", "4", "5"))))#reads in cleaned up csv file DeadBA_YearSiteSize_Clean.csv that includes added zeroes. Makes year and size a factor, which helps to graph later in the code

DeadBA_YearSite <-group_by(DeadBA_YearSiteSize_Clean, Year, LocationID) %>%
  summarise(SiteBA = sum(DeadBA))%>%
  summarise(YearBA = mean(SiteBA), SD_YearBA = sd(SiteBA))%>%
  mutate(CI95=(1.959964*(SD_YearBA/sqrt(7)))) %>%
  mutate(CI95high=YearBA +(1.959964*(SD_YearBA/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearBA - (1.959964*(SD_YearBA/sqrt(7)))) #Calculates lower limit of 95% Confidence Interval
#gives the total mean dead snag basal area for each year

write.table(DeadBA_YearSite, "Text Output/DeadBA_YearSite.txt", sep="\t")

ggplot(DeadBA_YearSite, aes(y=YearBA, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 6, 2), expand = c(0, 0), limits = c(0, 6)) +
  annotate("text", x = 1.0, y = 5.5, label = "Error Bars: 95% CI")+
  labs(y=expression("Basal Area"~(m^2/ha)))#creates a graph of mean total basal area for each year, error bars are 95% Confidence Interval


##### SNAG Dead Density (of all size classes) for each site by year#####
DeadDensity_YearSiteSize <- group_by(PERI_DeadOverstory, Year, LocationID, Class)  %>%
  group_by(Year, LocationID, Class) %>%
  tally() %>%
  mutate(DensityHA=n/0.1) #calculates the stem density per hectare by size class and year for each site

write.table(DeadDensity_YearSiteSize, "Text Output/DeadDensity_YearSiteSize.txt", sep="\t") #saves the WICR_Overstory4 table to a text file to clean the data by adding zeros for all size class, site, and year combinations

DeadDensity_YearSiteSize_Clean <- read_csv("DeadDensity_YearSiteSize_Clean.csv", col_types = cols(Year = col_factor(levels = c("2007","2012", "2016", "2021")), Class = col_factor(levels = c("1","2", "3", "4", "5"))))#reads in cleaned up csv file DeadDensity_YearSiteSize_Clean.csv that includes added zeroes. Makes year and size a factor, which helps to graph later in the code

DeadDensity_YearSite <-group_by(DeadDensity_YearSiteSize_Clean, Year, LocationID) %>%
  summarise(SiteDensity = sum(DensityHA))%>%
  summarise(YearDensity = mean(SiteDensity), SD_YearDensity = sd(SiteDensity)) %>%
  mutate(CI95=(1.959964*(SD_YearDensity/sqrt(7)))) %>%
  mutate(CI95high=YearDensity +(1.959964*(SD_YearDensity/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearDensity - (1.959964*(SD_YearDensity/sqrt(7)))) #Calculates lower limit of 95% Confidence Interval
#gives the total mean density for each year

write.table(DeadDensity_YearSite, "Text Output/DeadDensity_YearSite.txt", sep="\t")

ggplot(DeadDensity_YearSite, aes(y=YearDensity, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 200, 50), expand = c(0, 0), limits = c(0, 200)) +
  annotate("text", x = 1.1, y = 160, label = "Error Bars: 95% CI")+
  labs(y=expression("Tree Density (stems/ha)"))#creates a graph of mean total basal area for each year, error bars are 95% Confidence Interval


##### SNAG Dead Basal Area per Hectare grouped by Year, LocationID, and Size Class #####
DeadBA_YearSize <-group_by(DeadBA_YearSiteSize_Clean, Class, Year) %>%
  summarise(SizeBA=mean(DeadBA), SD_SizeBA = sd(DeadBA)) %>%
  mutate(CI95high=SizeBA +(1.959964*(SD_SizeBA/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=SizeBA - (1.959964*(SD_SizeBA/sqrt(7))))%>%#Calculates lower limit of 95% Confidence Interval
  mutate(CI95=(1.959964*(SD_SizeBA/sqrt(7))))
#gives the mean and standard deviation for Basal Area by year and size claSS. In other words, averages each of the four basal area values (from each of the four sites) for each year

write.table(DeadBA_YearSize, "Text Output/DeadBA_YearSize.txt", sep="\t")

ggplot(DeadBA_YearSize, aes(y=SizeBA, x=Year, fill=Class)) +
  geom_bar(stat="identity", position="dodge", color= "black")+
  scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                    values=c("#543005", "#bf812d", "#f6e8c3", "#80cdc1", "#01665e")) +
  geom_errorbar(aes(ymin=pmax(SizeBA-CI95,0), ymax=SizeBA+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression("Basal Area"~(m^2/ha))) +
  annotate("text", x = 1.0, y = 2.2, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2.5), breaks=seq(0,2.5,1))+
  theme_bw()  #creates the graph with year on x-axis and basal area on the y-axis. Graphs shows basal area of each size class for each year


##### SNAG Dead Tree Density per Hectare grouped by Year, LocationID, and Size Class #####
DeadDensity_YearSize <-group_by(DeadDensity_YearSiteSize_Clean, Class, Year) %>%
  summarise(SizeDensity=mean(DensityHA), SD_SizeDensity = sd(DensityHA)) %>%
  mutate(CI95high=SizeDensity +(1.959964*(SD_SizeDensity/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=SizeDensity - (1.959964*(SD_SizeDensity/sqrt(7))))%>%#Calculates lower limit of 95% Confidence Interval
  mutate(CI95=(1.959964*(SD_SizeDensity/sqrt(7))))
#gives the mean and standard deviation for tree density/hectare by year and size claSS. In other words, averages each of the four density values (from each of the four sites) for each year

write.table(DeadDensity_YearSize, "Text Output/DeadDensity_YearSize.txt", sep="\t")

ggplot(DeadDensity_YearSize, aes(y=SizeDensity, x=Year, fill=Class)) +
  geom_bar(stat="identity", position="dodge", color= "black")+
  scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                    values=c("#543005", "#bf812d", "#f6e8c3", "#80cdc1", "#01665e")) +
  geom_errorbar(aes(ymin=pmax(SizeDensity-CI95,0), ymax=SizeDensity+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression(" Density (stems/ha)")) +
  annotate("text", x = 1.0, y = 110, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 125), breaks=seq(0,125,50))+
  theme_bw()  #creates the graph with year on x-axis and basal area on the y-axis. Graphs shows basal area of each size class for each year


##### Juniper Basal Area (of all size classes) for each site by year#####
PERI_LiveJuniper <- read_csv("PERI_LiveJuniper.csv", col_types = cols(Year = col_factor(levels = c("2007", "2012", "2016","2021")), Class = col_factor(levels = c("1", "2", "3", "4", "5")))) %>%
  select("Year", "LocationID", "SpeciesCode","AcceptedSpecies", "USDA_CName", "Family", "Condition", "DBH", "Class") %>%  
  mutate(BasalArea = (DBH^2)*0.00007854) %>%
  mutate(BasalAreaHA = BasalArea/0.1)

JuniperBA_YearSiteSize <- group_by(PERI_LiveJuniper, Year, LocationID, Class)  %>%
  summarise(BA = sum(BasalAreaHA)) # Sums the Basal Area per Hectare by Year, LocationID, and Size Class

write.table(JuniperBA_YearSiteSize, "Text Output/JuniperBA_YearSiteSize.txt", sep="\t") # Export to clean the data by adding zeros for all size class, site, and year combinations

JuniperBA_YearSiteSize_Clean <- read_csv("JuniperBA_YearSiteSize_Clean.csv", col_types = cols(Year = col_factor(levels = c("2007","2012", "2016", "2021")), Class = col_factor(levels = c("1","2", "3", "4", "5"))))#reads in cleaned up csv file JuniperBA_YearSiteSize_Clean.csv that includes added zeroes. Makes year and size a factor, which helps to graph later in the code

JuniperBA_YearSite <-group_by(JuniperBA_YearSiteSize_Clean, Year, LocationID) %>%
  summarise(SiteBA = sum(BA))%>%
  summarise(YearBA = mean(SiteBA), SD_YearBA = sd(SiteBA))%>%
  mutate(CI95=(1.959964*(SD_YearBA/sqrt(7)))) %>%
  mutate(CI95high=YearBA +(1.959964*(SD_YearBA/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearBA - (1.959964*(SD_YearBA/sqrt(7)))) #Calculates lower limit of 95% Confidence Interval
#gives the total mean basal area for each year

write.table(JuniperBA_YearSite, "Text Output/JuniperBA_YearSite.txt", sep="\t") # Export to clean the data by adding zeros for all size class, site, and year combinations

ggplot(JuniperBA_YearSite, aes(y=YearBA, x=Year)) + 
  geom_bar(stat="identity", position="dodge", color= "black", fill="gray", width= 0.5)+
  geom_errorbar(aes(ymin=pmax(YearBA-CI95,0), ymax=YearBA+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression("Basal Area"~(m^2/ha))) +
  annotate("text", x = 1.0, y = 1.8, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2), breaks=seq(0,2,0.5))+
  theme_bw()  #creates the graph with year on x-axis and basal area on the y-axis. Graphs shows basal area of each size class for each year

  
##### Juniper Basal Area per Hectare grouped by Year, LocationID, and Size Class #####
JuniperBA_YearSize <-group_by(JuniperBA_YearSiteSize_Clean, Class, Year) %>%
  summarise(SizeBA=mean(BA), SD_SizeBA = sd(BA)) %>%
  mutate(CI95high=SizeBA +(1.959964*(SD_SizeBA/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=SizeBA - (1.959964*(SD_SizeBA/sqrt(7))))%>%#Calculates lower limit of 95% Confidence Interval
  mutate(CI95=(1.959964*(SD_SizeBA/sqrt(7))))
#gives the mean and standard deviation for Basal Area by year and size claSS. In other words, averages each of the four basal area values (from each of the four sites) for each year

write.table(JuniperBA_YearSize, "Text Output/JuniperBA_YearSize.txt", sep="\t") # Export to clean the data by adding zeros for all size class, site, and year combinations

ggplot(JuniperBA_YearSize, aes(y=SizeBA, x=Year, fill=Class)) +
  geom_bar(stat="identity", position="dodge", color= "black")+
  scale_fill_manual(breaks = c("1", "2", "3", "4"),
                    values=c("#543005", "#bf812d", "#f6e8c3", "#80cdc1", "#01665e")) +
  geom_errorbar(aes(ymin=pmax(SizeBA-CI95,0), ymax=SizeBA+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression("Basal Area"~(m^2/ha))) +
  annotate("text", x = 1.0, y = 0.65, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.7), breaks=seq(0,0.7,0.2))+
  theme_bw()  #creates the graph with year on x-axis and basal area on the y-axis. Graphs shows basal area of each size class for each year


##### Juniper Density (of all size classes) for each site by year#####
JuniperDensity_YearSiteSize <- group_by(PERI_LiveJuniper, Year, LocationID, Class)  %>%
  group_by(Year, LocationID, Class) %>%
  tally() %>%
  mutate(DensityHA=n/0.1) #calculates the stem density per hectare by size class and year for each site

write.table(JuniperDensity_YearSiteSize, "Text Output/JuniperDensity_YearSiteSize.txt", sep="\t") #saves the WICR_Overstory4 table to a text file to clean the data by adding zeros for all size class, site, and year combinations

JuniperDensity_YearSiteSize_Clean <- read_csv("JuniperDensity_YearSiteSize_Clean.csv", col_types = cols(Year = col_factor(levels = c("2007","2012", "2016", "2021")), Class = col_factor(levels = c("1","2", "3", "4", "5"))))#reads in cleaned up csv file Density_YearSiteSize_Clean.csv that includes added zeroes. Makes year and size a factor, which helps to graph later in the code

JuniperDensity_YearSite <-group_by(JuniperDensity_YearSiteSize_Clean, Year, LocationID) %>%
  summarise(SiteDensity = sum(DensityHA))%>%
  summarise(YearDensity = mean(SiteDensity), SD_YearDensity = sd(SiteDensity)) %>%
  mutate(CI95=(1.959964*(SD_YearDensity/sqrt(7)))) %>%
  mutate(CI95high=YearDensity +(1.959964*(SD_YearDensity/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=YearDensity - (1.959964*(SD_YearDensity/sqrt(7)))) #Calculates lower limit of 95% Confidence Interval
#gives the total mean density for each year

write.table(JuniperDensity_YearSite, "Text Output/JuniperDensity_YearSite.txt", sep="\t") 

ggplot(JuniperDensity_YearSite, aes(y=YearDensity, x=Year)) + 
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  geom_errorbar(aes(ymin=pmax(YearDensity-CI95,0), ymax=YearDensity+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", breaks=c("2007", "2012", "2016", "2021"), labels=c("2007", "2012", "2016", "2021"))+
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 100, 20), expand = c(0, 0), limits = c(0, 100)) +
  annotate("text", x = 1.1, y = 85, label = "Error Bars: 95% CI")+
  labs(y=expression("Tree Density (stems/ha)"))#creates a graph of mean total basal area for each year, error bars are 95% Confidence Interval

##### Juniper Tree Density per Hectare grouped by Year, LocationID, and Size Class #####
JuniperDensity_YearSize <-group_by(JuniperDensity_YearSiteSize_Clean, Class, Year) %>%
  summarise(SizeDensity=mean(DensityHA), SD_SizeDensity = sd(DensityHA)) %>%
  mutate(CI95high=SizeDensity +(1.959964*(SD_SizeDensity/sqrt(7)))) %>%  #Calculates higher limit of 95% Confidence Interval
  mutate(CI95low=SizeDensity - (1.959964*(SD_SizeDensity/sqrt(7))))%>%#Calculates lower limit of 95% Confidence Interval
  mutate(CI95=(1.959964*(SD_SizeDensity/sqrt(7))))
#gives the mean and standard deviation for tree density/hectare by year and size claSS. In other words, averages each of the four density values (from each of the four sites) for each year

write.table(JuniperDensity_YearSize, "Text Output/JuniperDensity_YearSize.txt", sep="\t") 

ggplot(JuniperDensity_YearSize, aes(y=SizeDensity, x=Year, fill=Class)) +
  geom_bar(stat="identity", position="dodge", color= "black")+
  scale_fill_manual(breaks = c("1", "2", "3", "4"),
                    values=c("#543005", "#bf812d", "#f6e8c3", "#80cdc1", "#01665e")) +
  geom_errorbar(aes(ymin=pmax(SizeDensity-CI95,0), ymax=SizeDensity+CI95),width=.2,position=position_dodge(.9))+
  scale_x_discrete(name="Year", labels=c("2007", "2012", "2016", "2021")) +
  labs(y=expression("Tree Density (stems/ha)")) +
  annotate("text", x = 3.5, y = 55, label = "Error Bars: 95% CI") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks=seq(0,60,20))+
  theme_bw()  #creates the graph with year on x-axis and basal area on the y-axis. Graphs shows basal area of each size class for each year
