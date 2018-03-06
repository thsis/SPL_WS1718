library("dplyr")

#import data
data= read.csv("Data/SPL_data.csv", sep = ";", dec = '.', header = TRUE, 
               stringsAsFactors = TRUE)

# Due to missing insolvencies in 1996 and missing data from 2003 onwards,
# we choose only the data of the period 1997-2002
#data1 = data 1997-2002
data1 = filter(data, JAHR>= 1997 & JAHR<=2002)

# Extract the industry class of companies 
data1$Ind.Klasse = substring(data1$VAR26, 1, 2)

# As we are only interested in companies with high percentage in the industry 
# composition we choose only companies belonging to the following sectors
# (according to German Classification of Economic Activities Standards (WZ 1993)):
#    1. Manufacturing (Man)
#    2. Wholesale and Retail (WaR)
#    3. Construction (Con)
#    4. Real Estate (RE)
Man = filter(data1, Ind.Klasse %in% as.character(15:37))
Man$Ind.Klasse = "Man"

WaR = filter(data1, Ind.Klasse %in% as.character (50:52))
WaR$Ind.Klasse = "WaR"

Con = filter(data1, Ind.Klasse == "45")
Con$Ind.Klasse = "Con"

RE = filter(data1, Ind.Klasse %in% as.character(70:74))
RE$Ind.Klasse = "RE"

# Remove data and data1 & bind the above subsets to get one dataset containing
# only companies of interest

rm(data, data1)

data = rbind(Man, WaR, Con, RE)

# Furthermore we choose only companies whose total assets (VAR6) are in the range
# 10^5 - 10^8

data = data[data$VAR6 >= 10^5 & data$VAR6<= 10^8,]

############################################################################
# Eliminate observations with 0 value for the following variables used as denominators 
# in calculation of financial ratios to be used in classification:

# total assets (VAR6)
# total sales(VAR16)
# cash (VAR1)
# inventories (VAR2)
# current liabilities (VAR12)
# total liabilities (VAR12 + VAR13)
# total assets-intangibe assets-cash-lands and buildings (VAR6 - VAR5 - VAR1 - VAR8)
# interest expenses (VAR19)


data_clean = data %>% filter(VAR6 != 0,
                             VAR16 != 0,
                             VAR1 != 0,
                             VAR2 != 0,
                             VAR12 != 0,
                             VAR12 + VAR13 != 0,
                            VAR6 - VAR5 - VAR1 - VAR8 != 0,                            
                            VAR19 != 0) 

# Get insolvent firms:
insolvent = data_clean[data_clean$T2 == 1, "ID"]

# Get solvent firms:
solvent = data_clean[data_clean$T2 == 0, "ID"]

#Show table with number of solvent/insolvent firms
table(data_clean$JAHR, data_clean$T2)

# Define Function that overwrites outliers:
# '...' : anonymous arguments that get passed to the quantile function.
replace_extreme_values = function(x, ...){
  # The point of this is to allow more flexibility for the user.
  quantiles = quantile(x, ...)
  
  x = ifelse(x <= quantiles[1], quantiles[1], x)
  x = ifelse(x >= quantiles[2], quantiles[2], x)
  
  return(x)
}


# add columns with financial ratios for each firm to the dataset
test_data = data_clean %>%
  mutate(x1 = VAR22/VAR6, # NetIncome/TotalAssets,
         x2 = VAR22/VAR16, # NetIncome/TotalSales,
         x3 = VAR21/VAR6, # OperatingIncome/TotalAssets,
         x4 = VAR21/VAR16, # OperatingIncome/TotalSales,
         x5 = VAR20/VAR6, # EBIT/TotalAssets,
         x6 = (VAR20+VAR18)/VAR6, # (EBIT+AmortizationDepreciation)/TotalAssets,
         x7 = VAR20/VAR16, # EBIT/TotalSales,
         x8 = VAR9/VAR6, # OwnFunds/TotalAssets,
         x9 = (VAR9-VAR5)/(VAR6-VAR5-VAR1-VAR8), # (OwnFunds-IntangibleAssets)/
                            #(TotalAssets-IntangibleAssets-Cash-LandsAndBuildings),
         x10 = VAR12/VAR6, # CurrentLiabilities/TotalAssets,
         x11 = (VAR12-VAR1)/VAR6, # (CurrentLiabilities-Cash)/TotalAssets,
         x12 = (VAR12+VAR13)/VAR6, # TotalLiabilities/TotalAssets,
         x13 = VAR14/VAR6, # Debt/TotalAssets,
         x14 = VAR20/VAR19, # EBIT/InterestExpense,
         x15 = VAR1/VAR6, # Cash/TotalAssets,
         x16 = VAR1/VAR12, # Cash/CurrentLiabilities,
         x17 = (VAR3-VAR2)/VAR12, # QuickAssets(=Cur.Assets-Invent,)/CurrentLiabilities,
         x18 = VAR3/VAR12, # CurrentAssets/CurrentLiabilities,
         x19 = (VAR3-VAR12)/VAR6, # WorkingCapital(=Cur.Assets-Cur.Liab.)/TotalAssets,
         x20 = VAR12/(VAR12+VAR13), # CurrentLiabilities/TotalLiabilities,
         x21 = VAR6/VAR16, # TotalAssets/TotalSales,
         x22 = VAR2/VAR16, # Inventories/TotalSales,
         x23 = VAR7/VAR16, # AccountsReceivable/TotalSales,
         x24 = VAR15/VAR16, # AccountsPayable/TotalSales,
         x25 = log(VAR6), # Log(TotalAssets),
         x26 = VAR23/VAR2, # IncreaseDecreaseInventories/Inventories,
         x27 = VAR24/(VAR12+VAR13), # IncreaseDecreaseLiabilities/TotalLiabilities,
         x28 = VAR25/VAR1) # IncreaseDecreaseCashFlow/Cash)

# Prepare data frame containing relative variables:
#   ID, T2, JAHR and financial ratios to be used for classification

test_data_rel = select(test_data,
                       ID, T2, JAHR, x1:x28)

# Prepare subsets of solvent and insolvent companies
# and replace extreme values 
test_data_insolvent = test_data_rel %>% filter(T2 == 1)
test_data_solvent = test_data_rel %>% filter(T2 == 0)

test_data_insolvent[, -c(1:3)] = apply(X = test_data_insolvent[, -c(1:3)], 
                                       MARGIN = 2,
                                       FUN = replace_extreme_values, 
                                       probs = c(0.05, 0.95))

test_data_solvent[, -c(1:3)] = apply(X = test_data_solvent[, -c(1:3)],
                                     MARGIN = 2,
                                     FUN = replace_extreme_values,
                                     probs = c(0.05, 0.95))

# Print descriptive statistics 
print("Solvent:")
print(round(t(apply(X = test_data_solvent[, -c(1:3)],
              MARGIN = 2, 
              quantile, 
              probs = c(0.05, 0.5, 0.95))), digits = 2))

print("Insolvent:")
print(round(t(apply(X = test_data_insolvent[, -c(1:3)], 
          MARGIN = 2,
          quantile,
          probs = c(0.05, 0.5, 0.95))), digits = 2))

data_clean = rbind(
  test_data_insolvent,
  test_data_solvent
)

# Remove unnecessary variables from working environment:
rm(list = ls()[!ls() == "data_clean"])


# Result: test_data_rel contains 9591 solvent and 783 insolvent firms and their fin.ratios
#       Almost same result as Zhang, Hardle (2010)
print(table(factor(data_clean$T2,
            levels = c(0, 1),
            labels = c("Solvent", "Insolvent"))))
