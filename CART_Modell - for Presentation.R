####################################################################################
####################################################################################
#build CART model:
### create a subset to train the model and a subset to test the resulting model

#create training dataset for CART_Model
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
### CART-Modell:

validierung_cart=validierung
names(validierung_cart)[2]="status"
validierung_cart=validierung_cart[,-1]
validierung_cart=validierung_cart[,-2]


## because the density of insolvent firms is rather low, we need to oversample the insolvent firms 
## in order for the models to pick up the patterns predictive of insolvency - use bootstrap technique

#Bootstrapping:
#create a subset from training dataset, which only includes insolvent firms 
training_insolvent=training[training$X.T2.==1,]
#create a subset, which only includes solvent firms 
training_solvent_full=training[training$X.T2.==0,]

## create matrix, for performance evaluation of two-class classication, filled later with true positive and 
## true negative rates 
conf_mat_true_cart=0
## create a matrix, which will later include the sum of all prediction outcomes 
conf_mat_sum_cart=0


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
  modfit=train(status~.,method="rpart",data=training_complete)

  
## Validation/Test phase: in order to estimate how well our model has been trained
  pred.cart=predict(modfit,newdata=validierung_cart)
## Illustration of accuracy and misclassication between the model and test dataset 
  conf_mat_cart=table(pred.cart,validierung_cart$status)
##fill the matrix with true positive and true negative rates
  conf_mat_true_cart=sum(conf_mat_true_cart,conf_mat_cart[1,1],conf_mat_cart[2,2],na.rm=T)
##fill the matrix with value of the sum of all prediction outcomes 
  conf_mat_sum_cart=sum(conf_mat_sum_cart+sum(conf_mat_cart,na.rm=T),na.rm=T)
}
## calculate accuracy rate
AR_cart=conf_mat_true_cart/conf_mat_sum_cart 
#AR_cart: ~68% 

