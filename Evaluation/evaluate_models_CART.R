source("Evaluation/Evaluate_Predictions.fin.R")
cart=read.csv("CART/cart_pred.csv", row.names = NULL)

evaluate_model(fitted_probs = cart$X1, labels=cart$label)
