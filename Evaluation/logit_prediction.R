source("Evaluation/Evaluate_Predictions.fin.R")

logit=read.csv("Logit/logit_pred.csv", row.names = NULL)

evaluate_model(fitted_probs = logit$X1, labels=logit$label)
