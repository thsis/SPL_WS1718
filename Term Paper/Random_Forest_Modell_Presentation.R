######################################################################################
# build Random Forest model:
### create a subset to train the model and a subset to test the resulting model
training=subset(test_data_ratio,test_data_ratio$X.JAHR.<2000)
#create valitation dataset for CART_Model
validierung=subset(test_data_ratio,test_data_ratio$X.JAHR.>=2000) 

#Number of firms in each dataset (solvent vs. insolvent)
nrow(training[training$X.T2.==0,])
nrow(training[training$X.T2.==1,])
nrow(validierung[validierung$X.T2.==0,])
nrow(validierung[validierung$X.T2.==1,])

## Result:density of insolvent firms is rather low in training and validation subset

#########################################################################################################
#########################################################################################################
### random forest Modell I (package randomForest):
validierung_rf=validierung
names(validierung_rf)[2]="status"
validierung_rf=validierung_rf[,-1]
validierung_rf=validierung_rf[,-2]

#Bootstrapping:
#create a subset from training dataset, which only includes insolvent firms 
training_insolvent=training[training$X.T2.==1,]
#create a subset, which only includes solvent firms 
training_solvent_full=training[training$X.T2.==0,]

## create matrix, for performance evaluation of two-class classication, filled later with true positive and 
## true negative rates 
conf_mat_true_rf=0
## create a matrix, which will later include the sum of all prediction outcomes 
conf_mat_sum_rf=0

## for each bootstrap sample use all insolvent firms in the training set and randomly sample the same 
## number of solvent firms from the training set
## build and validate the model 30x with ramdomly drawn solvent firms

for (i in 1:30){
  randum_numb=round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$X.T2.==0,])))
  training_solvent=training_solvent_full[randum_numb,]
  training_complete=rbind(training_insolvent,training_solvent)
  training_complete=training_complete[,-1]
  training_complete=training_complete[,-2]
  
  ## training the model:
  names(training_complete)[1]="status"
  mod_forest_I=randomForest(as.factor(status)~.,
                            data=training_complete,
                            importance=T,
                            ntree = 2000, 
                            maxnodes= 100,
                            norm.votes = F)
  ## Validation/Test phase: in order to estimate how well our model has been trained
  pred_rf=as.data.frame(predict(mod_forest_I, validierung_rf))
  conf_mat_rf=table((as.numeric(unlist(pred_rf))-1),validierung_rf$status)
  conf_mat_true_rf=sum(conf_mat_true_rf,conf_mat_rf[1,1],conf_mat_rf[2,2],na.rm=T)
  conf_mat_sum_rf=sum(conf_mat_sum_rf+sum(conf_mat_rf,na.rm=T),na.rm=T)
}
## calculate accuracy rate
AR_cart=conf_mat_true_rf/conf_mat_sum_rf 
#AR_RF: ~72% Korrektheitsrate


### alternativ random forest Modell II (package party):
mod_forest_II=cforest(as.factor(status)~.,data=training_complete,controls=cforest_unbiased(ntree=2000, mtry=3))
mod_forest_pred = predict(mod_forest_II,OOB=TRUE, type = "response")
mod_forest_table=table(validierung_complete$status,mod_forest_pred)
AR_forest_II=(mod_forest_table[1,1]+mod_forest_table[2,2])/(sum(mod_forest_table))
# accuracy rate: ~ 73%