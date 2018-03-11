#########################################################################################################
#Modelle bilden:
### create a subset to train the model and a subset to test the resulting model
training=subset(test_data_ratio,test_data_ratio$X.JAHR.<2000) 
validierung=subset(test_data_ratio,test_data_ratio$X.JAHR.>=2000) 

nrow(training[training$X.T2.==0,])
nrow(training[training$X.T2.==1,])
nrow(validierung[validierung$X.T2.==0,])
nrow(validierung[validierung$X.T2.==1,])

#########################################################################################################
### logit model:
validierung_logit=validierung
names(validierung_logit)[2]="status"
validierung_logit=validierung_logit[,-1]
validierung_logit=validierung_logit[,-2]

#Bootstrapping:
training_insolvent=training[training$X.T2.==1,]
training_solvent_full=training[training$X.T2.==0,]

conf_mat_true_logit=0
conf_mat_sum_logit=0

for (i in 1:30){
  randum_numb=round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$X.T2.==0,])))
  training_solvent=training_solvent_full[randum_numb,]
  training_complete=rbind(training_insolvent,training_solvent)
  training_complete=rbind(training_insolvent,training_solvent)
  training_complete=training_complete[,-1]
  training_complete=training_complete[,-2]
  
  ## training the model:
  names(training_complete)[1]="status"
  glm_mod=train(as.factor(status)~.,data=training_complete, method="glm", family="binomial")
  glm_pred = predict(glm_mod, newdata=validierung_logit)
  conf_mat_logit=table(glm_pred, validierung_logit$status)
  conf_mat_true_logit=sum(conf_mat_true_logit,conf_mat_logit[1,1],conf_mat_logit[2,2],na.rm=T)
  conf_mat_sum_logit=sum(conf_mat_sum_logit+sum(conf_mat_logit,na.rm=T),na.rm=T)
}
AR_logit=conf_mat_true_logit/conf_mat_sum_logit 
#accuracy rate:~71 % 

