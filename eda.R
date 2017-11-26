# Exploratory Data Analysis

setwd("~/SPL_WS1718/")
source("data_prep.R")

library("ggplot2")
library("GGally")
library("ggmosaic")

library("scagnostics")
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

# Andrews Curves
