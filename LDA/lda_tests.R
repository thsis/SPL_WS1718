library("ggplot2")
source("LDA/utils.R")
source("LDA/lda.R")
# Test the Linear Discriminant Analysis Classifier
# Use the iris dataset, where we know that the setosa-species
# is linearly seperable (i.e. it is obvious on a non-transformed space).

data("iris")
iris$is_setosa = factor(iris$Species == "setosa",
                        levels = c(TRUE, FALSE),
                        labels = c("yes", "no"))
iris_test = iris[, -5]
summary(iris_test)

ggplot(iris_test, aes(x     = Sepal.Length,
                      y     = Sepal.Width,
                      color = is_setosa)) +
  geom_point() +
  ggtitle("Setosa vs. others") +
  theme_bw()

# We see that the data is already linear separable.
# with one observation being very close on the seperating line.
# From that we would expect to get a missclassification-rate of 0%.

irisLDA = lda(iris_test, "is_setosa")
plot(irisLDA)

predictions = predict(irisLDA, iris_test)
predictions = factor(predictions,
                     levels = c(1, 2),
                     labels = c("yes", "no"))

table(predictions, iris_test$is_setosa)
