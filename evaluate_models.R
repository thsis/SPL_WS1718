source("Evaluation/Evaluate_Predictions.fin.R")
rf=read.csv("Random_Forest/rf_pred.csv", row.names = NULL)

evaluate_model(fitted_probs = rf$X1, labels=rf$label)
