# Exploratory Data Analysis

library("ggplot2")
library("GGally")
library("ggmosaic")

library("andrews")

setwd("~/SPL_WS1718/")
data = read.csv("Data/SPL_data.csv",
                sep = ";",
                dec = ".")

get_column_type = function(data, type = c("numeric", "integer",
                                          "factor", "character")){
  cols = sapply(data, function(x) {return(class(x) == type)})
  return(cols)
}

int_cols = get_column_type(data = data, type = "integer")
num_cols = get_column_type(data = data, type = "numeric")

# Set flags for ID, T2 and Year to FALSE.
# They are not interpretable as nuumeric.

numeric_vars = int_cols | num_cols
numeric_vars[1:3] = FALSE

data_numeric = data[, numeric_vars]
data_scaled = cbind(
  data[, !numeric_vars],
  scale(data_numeric)
)

# Assets pairwise
ggpairs(data = data_scaled, columns = 1:8 + 3)

# Liabilities pairwise
ggpairs(data = data_scaled, columns = 1:7 + 11)


ggplot(data = data_scaled[, c("T2", "VAR27")]) +
  geom_mosaic(aes(x = product(VAR27),
                  fill = factor(VAR27))) +
  facet_grid(T2~.) +
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank()) +
  labs(x = "", y = "Insolvent",
       title = "Mosaic Plot of categorical Variables")

# Andrews Curves for random sample
set.seed(142)

sample_andrews = function(data, size = 50, folds = 1, ...){
  par(mfrow = c(folds, 1))
  for (i in 1:folds) {
    sample_set = sample(1:dim(data)[1], size = size)
    andrews(data[sample_set, ], ...)
  }
  par(mfrow = c(1, 1))
}

complete_cases = data[complete.cases(data_scaled[, -8]), ]

sample_andrews(complete_cases, size = 100, folds = 4, clr=1)

# Parallel-Coordinate Plots
ggparcoord(data = data_scaled) +
  geom_line()
