#=====================================================================
# Evaluate Predictions
#
## Intended Outcome:
## Function that takes labels and predictions as inputs
## and returns the following KPI's/Plots/Tables:
##    - Confusion Matrix
##        - true positives
##        - true negatives
##        - false positives
##        - false negatives
##    - ROC + AUC
##    - Accuracy
##    - Precision
##    - sensitivity
#=====================================================================
setwd("~/SPL_WS1718/Random/")
# Get toy example from unit-tests.R

get_prediction = function(fitted_probs, threshold){
  predictions = ifelse(fitted_probs > threshold,
                       1, 0)
  return(predictions)
}

evaluate_predictions = function(labels, predictions, verbose = FALSE){
  ct = table(factor(x = labels, levels = c(0, 1)),
             factor(x = predictions, levels = c(0,1)))
  
  if(any(dim(ct) != 2)) stop("Labels or Predictions contain more than 2 classes.")
  
  TN = ct[1, 1]
  TP = ct[2, 2]
  FP = ct[1, 2]
  FN = ct[2, 1]
  
  # Calculate measures
  reports = list(
    sensitivity = TP / (TP + FN),
    specificity = TN / (FP + TN),
    precision = TP / (TP + FP),
    accuracy = (TP + TN) / sum(ct))
  
  if(verbose) print(data.frame(reports), digits = 3)
  return(reports)
}

evaluate_model = function(fitted_probs, labels){
  threshold_list = seq(from = 0, to = 1, by = 0.0001)
  pred_list = lapply(threshold_list,
                     get_prediction,
                     fitted_probs = fitted_probs)
  report_list = lapply(pred_list,
                       evaluate_predictions,
                       labels = labels)
  
  reports = unlist(report_list)
  
  # Calculate ROC:
  sensitivities = reports[names(reports) == "sensitivity"]
  specificities = reports[names(reports) == "specificity"]
  # Calculate AUC:
  # Use trapezoidal rule by taking the average of the "right" and "left" y's
  get_auc = function(x, y){
    abs(sum(diff(x) * (head(y, -1) + tail(y, -1)))/2)}
  auc = get_auc(1-specificities, sensitivities)
  
  # Chose optimal threshold, specificity, sensitivity, predictions and confusion matrix:
  opt_ind = which.max(sensitivities + specificities - 1)
  opt_sensitivity = sensitivities[opt_ind]
  opt_specificity = specificities[opt_ind]
  opt_threshold = seq(from = 0, to = 1, by = 0.0001)[opt_ind]
  opt_predictions = get_prediction(
    fitted_probs = fitted_probs,
    threshold = opt_threshold)
  ct = table(factor(x = labels, levels = c(0, 1)),
             factor(x = opt_predictions, levels = c(0,1)))

  plot(x = 1 - specificities,
       y = sensitivities,
       main = "ROC-Curve",
       xlab = "False Positive Rate (1 - specificity)",
       ylab = "True Positive Rate (sensitivity)",
       xlim = c(0, 1),
       ylim = c(0, 1),
       asp = TRUE,
       type = "l")
  abline(c(0, 0), c(1,1), col = "grey")
  return(list(sensitivity = opt_sensitivity,
              specificity = opt_specificity,
              threshold = opt_threshold,
              predictions = opt_predictions,
              auc = auc,
              contingency_table = ct))
}
