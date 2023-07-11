##### Loading packages and importing data files #####
library(readr)
library(dplyr)
library(ggplot2)

WICR_Fuels <- read_csv("FFI/WICRSurfaceFuels.csv") #to read in files you only need file name as long as the file is your working directory, otherwise the full file path is required

WICR_Fuels2 <- WICR_Fuels %>%
mutate(Status=as.factor(Status))

ggplot(WICR_Fuels2, aes(y=FuelLoad, x=Status)) +
  ylab("Mean Fuel Load (tons/acre)") +
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Status", breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), labels=c("2003 post
  tornado", "2004 1Yr
  post tornado", "2005 2yr
  post tornado", "2006
  post burn", "2009 3yr
  post burn", "2009
  post burn", "2017
  8yr post burn", "2019
  post burn"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
  annotate("text", x = 1.7, y = 78, label = "Error Bars: 95% CI")+
  theme_bw() 

##### Fuel Load with 2022 added #####
WICR2022_Fuels <- read_csv("WICR_FuelLoad.csv", 
                          col_types = cols(Status = col_factor(levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")))) 

ggplot(WICR2022_Fuels, aes(y=FuelLoad, x=Status)) +
  ylab("Mean Fuel Load (tons/acre)") +
  geom_bar(stat="identity", position = position_dodge(1.5), color="black", fill="gray", width= 0.5) +
  geom_errorbar(aes(ymin=CI95low, ymax=CI95high), width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(name="Status", breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), labels=c("2003 post
  tornado", "2004 1Yr
  post tornado", "2005 2yr
  post tornado", "2006
  post burn", "2009 3yr
  post burn", "2009
  post burn", "2017
  8yr post burn", "2019
  post burn", "2022
  post burn"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
  annotate("text", x = 1.7, y = 78, label = "Error Bars: 95% CI")+
  theme_bw() 










