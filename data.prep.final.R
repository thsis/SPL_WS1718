#set working directory
setwd("C:/Users/Maiko/Downloads/university/Statistical Programming Languages/seminar")

#import data
data= read.csv("SPL_data.csv", sep = ";", dec = '.', header = TRUE, 
               stringsAsFactors = TRUE)

#Due to missing insolvencies in 1996 and missing data from 2003 onwards,
# we choose only the data of the period 1997-2002
#data1 = data 1997-2002
data1 = data[data$JAHR>= 1997 & data$JAHR<=2002,]

#Extract the industry class of companies 
data1$Ind.Klasse = substring(data1$VAR26, 1, 2)

# As we are only interested in companies with high percentage in the industry 
#  composition we choose only companies belonging to the following sectors
#  (according to German Classification of Economic Activities Standards (WZ 1993)):
#    1. Manufacturing (Man)
#    2. Wholesale and Retail (WaR)
#    3. Construction (Con)
#    4. Real Estate (RE)

Man = data1[data1$Ind.Klasse %in% as.character(15:37),]
Man$Ind.Klasse = "Man"

WaR = data1[data1$Ind.Klasse %in% as.character (50:52),]
WaR$Ind.Klasse = "WaR"

Con = data1[data1$Ind.Klasse == "45",]
Con$Ind.Klasse = "Con"

RE = data1[data1$Ind.Klasse %in% as.character(70:74),]
RE$Ind.Klasse = "RE"

#Remove data and data1 & bind the above subsets to get one data set containing
# only companies of interest

rm(data, data1)

data = rbind(Man, WaR, Con, RE)

#Furthermore we choose only companies whose total assets (VAR6) are in the range
# 10^5 - 10^7

data = data[data$VAR6 >= 10^5 & data$VAR6<= 10^7,]

############################################################################
#Eliminate observations with 0 value for variables used as denominators 
# in calculation of financial ratios to be used in classification

#total assets (VAR6)
data= data[!data$VAR6 == 0,]

#total sales(VAR16)
data = data[!data$VAR16 == 0,]

#cash (VAR1)
data = data[!data$VAR1 == 0,]

#inventories (VAR2)
data = data[!data$VAR2 == 0,]

#current liabilities (VAR12)
data = data[!data$VAR12 == 0,]

#total liabilities (VAR12 + VAR13)
data = data[!(data$VAR12+data$VAR13)==0,]

#total assets - intangibe assets - cash - lands and buildings
# (VAR6 - VAR5 - VAR1 - VAR8)
data = data[!(data$VAR6 - data$VAR5 - data$VAR1 - data$VAR8)==0,]

