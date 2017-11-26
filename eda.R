# Exploratory Data Analysis

setwd("~/SPL_WS1718/")
source("data_prep.R")

library("ggplot2")
library("GGally")
library("ggmosaic")

library("andrews")

# Assets pairwise
ggpairs(data = data_scaled, columns = 1:8 + 3)

# Liabilities pairwise
ggpairs(data = data_scaled, columns = 1:7 + 11)


ggplot(data = data_scaled[, c("Insolvent", "X.VAR27.")]) +
  geom_mosaic(aes(x = product(X.VAR27.),
                  fill = factor(X.VAR27.))) +
  facet_grid(Insolvent~.) +
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank()) +
  labs(x = "", y = "Insolvent",
       title = "Mosaic Plot of categorical Variables")

# Andrews Curves for random sample
set.seed(42)

sample_andrews = function(data, size = 50, folds = 1, ...){
  par(mfrow = c(folds, 1))
  for (i in 1:folds) {
    sample_set = sample(1:dim(data)[1], size = size)
    andrews(data[sample_set, ], ...)
  }
  par(mfrow = c(1, 1))
}

sample_andrews(data_scaled[, c(3:19, 21:25, 36)], size = 100, folds = 4, clr=22)

# Parallel-Coordinate Plots
ggparcoord(data = data_scaled) +
  geom_line()
