# SVM - Benchmark
library("e1071")
set.seed(42)

setwd("~/SPL_WS1718/")
data = read.csv("Data/SPL_data.csv",
                sep = ";",
                dec = ".")
data = data[complete.cases(data[, -36]), -36]
summary(data)
data$Insolvent = factor(data$T2, levels = c(0, 1), labels = c("solvent", "insolvent"))

train_indices = sample(1:dim(data)[1],
                       30000,
                       replace = FALSE)
test_indices = (1:dim(data)[1])[!(1:dim(data)[1] %in% train_indices)]

stopifnot(sum(length(train_indices), length(test_indices)) == dim(data)[1],
          !any(train_indices %in% test_indices),
          !any(test_indices %in% train_indices))

train_data = data[train_indices, ]
test_data = data[test_indices,]

if(!file.exists("Data/svm_creditreform.RData")){
  start = Sys.time()
  model = svm(Insolvent ~ ., data = train_data)
  save(model, "Data/svm_creditreform.RData")
  summary(model)
  (duration = Sys.time() - start)
} else {
  load("Data/svm_creditreform.RData")
}

pred = predict(model, test_data)
table(pred, test_data$T2)


###############################################################################

data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)
