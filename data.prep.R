setwd("C:/Users/Maiko/Downloads/university/Statistical Programming Languages/seminar")

#import data
data= read.csv("SPL_data.csv", sep = ";", dec = '.', header = TRUE, stringsAsFactors = TRUE)

#data1 = data 1997-2002
data1 = data[data$JAHR>= 1997 & data$JAHR<=2002,]

# data2= data 2003 onwards to get the industry class of each company
data2= data[data$JAHR>=2003,]

# extracting industry class
data2$Ind.Klasse = substring(data2$VAR26, 1, 1)

# subsetting companies of interest - data3
data3 = data2[data2$Ind.Klasse == c ("C", "F", "G", "L"), ]

library(dplyr)

#___Grouping companies by industry class___
GroupC = filter(data3, Ind.Klasse == "C")
GroupF = filter(data3, Ind.Klasse == "F")
GroupG = filter(data3, Ind.Klasse == "G")
GroupL = filter(data3, Ind.Klasse == "L")

#### Selecting companies from data1 belonging to ind.class of interest

#First select the companies' IDs for each Ind.class
ID.C = GroupC$ID
ID.F = GroupF$ID
ID.G = GroupG$ID
ID.L = GroupL$ID

#Then select companies from data1 and assign them the respective industry class

GroupC1 = data1 [data1$ID %in% ID.C,]
GroupC1$Ind.Klasse = "C"

GroupF1 = data1 [data1$ID %in% ID.F,]
GroupF1$Ind.Klasse = "F"

GroupG1 = data1 [data1$ID %in% ID.G,]
GroupG1$Ind.Klasse = "G"

GroupL1 = data1 [data1$ID %in% ID.L,]
GroupL1$Ind.Klasse = "L"


# Gathering all selected companies into one dataframe (data.97.02)
data.97.02 = rbind(GroupC1, GroupF1, GroupG1, GroupL1)
