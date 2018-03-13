# ==============================================================================
# Harmonise Industry Classification Codes
# ------------------------------------------------------------------------------
# Problem outset: 
# Some records for VAR26 (industry classification) seem to use different coding 
# standards. Which indicates that the standard for industry codes changed bet-
# ween 1996 and 2007.
# Our tasks will be to:
#     1. Identify the breaking points.
#     2. Reconcile both standards into one time invariant industry-code
#        which we'll use for further analysis.
# ==============================================================================

library("dplyr")

setwd("~/SPL_WS1718/Data/")
data = read.csv("SPL_data.csv",
                sep = ";",
                dec = ".")
summary(data)
glimpse(data)

# Select only relevant variables and order them for 
ind_codes = data %>%
  select(c("ID", "JAHR", "VAR26")) %>% 
  arrange(ID, JAHR)

# Find a company with sufficient observations (i.e. a long-term-history).
# Group by ID and calculate min and max year of observed data.
lth_comp = ind_codes %>% 
  group_by(ID) %>% 
  summarise(
    min_year = min(JAHR),
    max_year = max(JAHR),
    n_observed = n()) %>% 
  filter(min_year == min(data$JAHR) & max_year == max(data$JAHR)) %>%
  arrange(desc(n_observed))

# Find one company whith a long history and highest number of observations
lth_id = unlist(lth_comp[1, "ID"])

# Print result for a company with a long history
print(ind_codes[ind_codes$ID == lth_id, ])

# Contingency table of (coarse) category per year
table(
  substring(data[, "VAR26"], 1, 1),
  data[,"JAHR"])

# New problem: there exist Industry-categories which are not listed in WZ 03
# This might have two reasons:
#     - Either: the standard WZ 08 is used (consistently).
#     - Or: the standard changes over the years.
# Do industry categories change between 2003 and 2007?
# If they do, this is a strong indication for the second case.
 
data_before_2003 = data %>%
  filter(JAHR < 2003)
data_after_2003 = data %>%
  filter(JAHR >= 2003)

cat_change = data_after_2003[, c("ID", "VAR26")] %>%
  group_by(ID) %>%
  summarise(
    n_categories = n_distinct(
                     substring(VAR26, 1, 1))) %>%
  filter(n_categories > 1) %>%
  select(ID)

# Companies with Change in Category after 2003
ccc03 = data_after_2003[data_after_2003$ID %in% unlist(cat_change),
                        c("ID", "JAHR", "VAR26")]

ccc03 = ccc03 %>%
  select(ID, JAHR, VAR26) %>%
  arrange(ID, JAHR)

# The categories change once in 2003, again in 2005, and then stay constant. 
head(ccc03, 12)

# Total number of companies with category change
length(unique(ccc03$ID))
# Total number of companies in dataset
length(unique(data$ID))

ccc03_jumps = ccc03 %>%
  group_by(ID) %>%
  mutate(
    change_to_previous = !(lag(VAR26, 1) == VAR26),
    coarse_cat = substring(VAR26, 1, 1)) %>%
  filter(change_to_previous | is.na(change_to_previous)) 

ccc03_breaks = ccc03_jumps %>%
  summarise(
    min_year = min(JAHR),
    max_year = max(JAHR)
  )

summary(ccc03_breaks)
# ==============================================================================
# Result 1:
# The first change seems to have happend at 2003 (which coincides with the his-
# tory of the German Classification of Industrial Branches - WZ03 vs WZ93).
# After 2003 however, there seems to be another change in the way of coding,
# since there appear categories which according to WZ 03 should not exist 
# (namely categories R-U).
# However, some companies seem to use the coding standard of WZ08 even during
# the years 2003-2007 (cp. ID: 16902)
# https://de.wikipedia.org/wiki/Klassifikation_der_Wirtschaftszweige
# ==============================================================================

# Refactor industry classes:
# Define dictionary of transitions (Labeling Dictionary).
# 1993 -> 2008
# NOTE! --- This dictionary takes into account only the divisions according to 
#           the first two numbers of industry classification code
#    Possible class changes of subdivisions are not taken into account
ld_WZ93_WZ08 = list(
  "A" = as.character(c(01, 02, 05)),
  "B" = as.character(10:14),
  "C" = as.character(c(15:21, 23:36)),
  "D" = "40",
  "E" = c("37", "41","90"),
  "F" = "45",
  "G" = as.character(50:52),
  "H" = as.character(60:64),
  "I" = "55",
  "J" = c("22","72"),
  "K" = as.character(65:67),
  "L" = "70",
  "M" = c("73", "74"),
  "N" = "71",
  "O" = "75",
  "P" = "80",
  "Q" = "85",
  "R" = "92",
  "S" = c("91", "93") ,
  "T" = "95",
  "U" = "99"
  
)

# Refactoring-Function
# One-dimensional input
refactor = function(label, dict){
  # Auxiliary function to be used in a apply-call.
  infun = function(x, vec){
    return(x %in% vec)
  }
  ind = unlist(
    lapply(dict, infun, x = label))
  return(names(dict)[ind])
}

# Column as input
recat = function(data, dictionary){
  new_cat = lapply(data, refactor, dict = dictionary)
  return(unlist(new_cat))
}

# TEST REFACTORING FUNCTION
data_before_2003$VAR26 = recat(substring(data_before_2003$VAR26, 1, 2), dictionary = ld_WZ79_WZ03)
data_after_2003$VAR26 = substring(data_after_2003$VAR26, 1, 1)
# Check if there are inconsistencies in the way of reporting assets/liabilities.

data_before_2003 = data %>% 
  filter(JAHR < 2003)
data_after_2003 = data %>%
  filter(JAHR >= 2003)

summary(data_before_2003)
summary(data_after_2003)

# All the companies after 2003 are solvent.
table(data_before_2003[, 2:3])
table(data_after_2003[, 2:3])

# After 2003 there is a difference in reporting.
summary(data_before_2003$VAR1)
summary(data_after_2003$VAR1)

# ==============================================================================
# Result 2:
# The companies which are observed after 2003 seem to report their values in
# units of 1000 Euros.
# ==============================================================================