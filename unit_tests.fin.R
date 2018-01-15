## Unit tests & assorted things
# Test the evaluation function using a logit model of the Boston-Housing dataset.

source("Evaluate_Predictions.R")

library("pROC")
library("MASS")

data("Boston")

test_eval_fun = function(formula){
  model = glm(data = Boston, formula = formula, family = binomial(link = 'logit'))
  par(mfrow = c(1,2))
  custom_auc = evaluate_model(model$fitted.values, Boston$chas)
  plot.roc(Boston$chas, model$fitted.values)
  r_auc = auc(Boston$chas, model$fitted.values)
  
  par(mfrow = c(1,1))
  # Is the difference between AUCs large?
  test = abs(r_auc - custom_auc$auc) <= 0.01
  return(test)
}

# Take every single variable as predictor for the Charles-River-Index (except for chas).
models = paste("chas ~", names(Boston)[-4])
tests = sapply(models, test_eval_fun)
print(all(tests))

if(!all(tests)) {
  print(tests)
  sum(tests) / length(tests)}