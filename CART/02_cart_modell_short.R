#Modelle bilden:
source("Preparation/data.preparation.R") 

library("raster")
library("rpart")
library("rpart.plot")
library("rattle")
library("caret")
library("corrplot")

### building training and test set:
data_clean$T2 = factor(data_clean$T2, levels = c(0,1), labels = c(0,1))

training=subset(data_clean,data_clean$JAHR<2000) 
validierung=subset(data_clean,data_clean$JAHR>=2000) 
#############################################
### CART model:

validierung_cart=validierung
names(validierung_cart)[2]="status" 
validierung_cart=validierung_cart[,c(-1,-3)]

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

  training_solvent=training_solvent_full[randum_numb, ]
  training_complete=rbind(training_insolvent,training_solvent)
  
  #train the model:
  names(training_complete)[1]="status"
  modfit=train(status~.,method="rpart",data=training_complete)
  
  #calculate predictions:
  pred.cart=predict(modfit,newdata=validierung_cart)
  conf_mat_cart=table(pred.cart,validierung_cart$status)
  TN=TN+conf_mat_cart[1,1]
  FP=FP+conf_mat_cart[1,2]
  TP=TP+conf_mat_cart[2,2]
  FN=FN+conf_mat_cart[2,1]
}

correlations=round(cor(training_complete[, -1]),2)
pdf("CorrPlot.pdf")
corrplot(correlations, order = "hclust")
dev.off()

pdf("varImp.pdf")
plot(varImp(modfit), main="Variable Importance")
dev.off()

# we use rpart for plotting fancy decision tree and predictions 
mod_fit=rpart(as.factor(status)~.,data=training_complete)

pdf("fancyRpartPlot.pdf")
fancyRpartPlot(mod_fit)
dev.off()

pred_cart=predict(mod_fit,newdata=validierung_cart,type="prob")
write.csv(cbind(label=as.numeric(as.character(validierung_cart$status)),pred_cart),file="CART/cart_pred.csv")
