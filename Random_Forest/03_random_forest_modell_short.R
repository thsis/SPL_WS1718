#Modelle bilden:

source("Preparation/data.preparation.R")

library("randomForest")

### train the model:
data_clean$T2 = factor(data_clean$T2, levels = c(0,1), labels = c(0,1))

training=subset(data_clean,data_clean$JAHR<2000) 
validierung=subset(data_clean,data_clean$JAHR>=2000) 

###################################################################
###################################################################
### random forest model (package randomForest):
validierung_rf=validierung
names(validierung_rf)[2]="status"
validierung_cart=validierung_rf[,c(-1,-3)]

#Bootstrapping:

training_insolvent=training[training$T2==1,-c(1,3)]
training_solvent_full=training[training$T2==0, -c(1,3)]

TN=0
TP=0
FP=0
FN=0

set.seed(1234)
for (i in 1:30){
  randum_numb=round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$T2==0,])))
  training_solvent=training_solvent_full[randum_numb,]
  training_complete=rbind(training_insolvent,training_solvent)
  
  
  #train the model:
  names(training_complete)[1]="status"
  mod_forest_I=randomForest(as.factor(status)~.,data=training_complete,importance=T,ntree = 2000, maxnodes= 100, norm.votes = F)
  
  #model predictions:
  
  pred_rf=as.data.frame(predict(mod_forest_I, validierung_rf))
  conf_mat_rf=table((as.numeric(unlist(pred_rf))-1),validierung_rf$status)
  TN=TN+conf_mat_rf[1,1]
  FP=FP+conf_mat_rf[1,2]
  TP=TP+conf_mat_rf[2,2]
  FN=FN+conf_mat_rf[2,1]
}
#variable importance 
VarImp_rf=importance(mod_forest_I)
varImpPlot(mod_forest_I)
write.csv(imp,file="Random_Forest/importance.csv")

#calculation of probabilities 
pred_rf=as.data.frame(predict(mod_forest_I, validierung_rf, type = "prob"))
write.csv(cbind(label=validierung_rf$status,pred_rf),file="Random_Forest/rf_pred.csv")
