#==============================================================
## Building and applying logistic regression model for insolvecy
##
## steps to be taken:
## split dataset into training and test set
## apply bootstrapping technique and re-balance the training set by downsampling solvent firms
## fit the model and evaluate the outcomes
## build a confusion matrix and store the predictions for further analysis
#========================================================

source("Preparation/data.preparation.R")

library("caret")

### build training and test set:
data_clean$T2 = factor(data_clean$T2, levels = c(0,1), labels = c(0,1))

training = subset(data_clean, data_clean$JAHR<2000)
validierung = subset(data_clean, data_clean$JAHR>=2000)

validierung_logit           = validierung
names(validierung_logit)[2] = "status"
validierung_logit           = validierung_logit[, c(-1, -3)]

#Bootstrapping:

training_insolvent=training[training$T2==1,-c(1,3)]
training_solvent_full=training[training$T2==0, -c(1,3)]

TN = 0
TP = 0
FP = 0
FN = 0
set.seed(1234) #pseudo-random number generation to make work reproducible and results comparable
for (i in 1:30){
    randum_numb       = round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$T2==0,])))
    training_solvent  = training_solvent_full[randum_numb,]
    training_complete = rbind(training_insolvent,training_solvent)

    #fit the model:
    names(training_complete)[1] = "status"
    glm_mod=train(as.factor(status)~., data = training_complete, method = "glm", family = "binomial")
    # define target variable as categorical
    # specify distribution function and link function
    glm_pred       = predict(glm_mod, newdata = validierung_logit)
    conf_mat_logit = table(glm_pred, validierung_logit$status)
    TN             = TN + conf_mat_logit[1, 1]
    FP             = FP + conf_mat_logit[1, 2]
    TP             = TP + conf_mat_logit[2, 2]
    FN             = FN + conf_mat_logit[2, 1]
}
# evaluate the model:
pred_logit = predict(glm_mod, newdata = validierung_logit, type = "prob")
write.csv(cbind(labels = validierung_logit$status, pred_logit), file = "Logit/logit_pred.csv")
