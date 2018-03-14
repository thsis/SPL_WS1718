source("Evaluation/Evaluate_Predictions.fin.R")

rf=read.csv("Random_Forest/rf_pred.csv")
cart = read.csv("CART/cart_pred.csv")
logit = read.csv("Logit/logit_pred.csv")

evaluate_model(fitted_probs = rf$X1, labels = rf$label)
evaluate_model(fitted_probs = cart$X1, labels = cart$label)
evaluate_model(fitted_probs = logit$X1, labels = logit$labels)
