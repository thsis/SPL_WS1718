#Modelle bilden:
source("Preparation/data.preparation.R") 

library("raster")
library("rpart")
library("rpart.plot")
library("rattle")
library("caret")
library("corrplot")

### Trainings- und Testsubset erstellen:
data_clean$T2 = factor(data_clean$T2, levels = c(0,1), labels = c(0,1))

training=subset(data_clean,data_clean$JAHR<2000) 
validierung=subset(data_clean,data_clean$JAHR>=2000) 
####################################################
### CART-Modell I:

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
  
  #Traine the model: 
  names(training_complete)[1]="status"
  
  #mod_fit_help=rpart(as.factor(status)~.,data=training_complete)
  #summary(mod_fit_help)
  #printcp(mod_fit_help)
  #mod_fit_help_2=prune.rpart(mod_fit_help,cp=0.5)
  #modfit=train(mod_fit_help)#Modellbildung: status in abh. aller anderen Variablen des Datensatzes, Methode "rpart" = CART, Datensatz=Trainingsdatensatz mit gleicher Anzahl solventer und insolventer Firmen.
  #summary(modfit)
  #print(modfit)
  modfit=train(status~.,method="rpart",data=training_complete)
  
  #fancyRpartPlot(modfit$finalModel)#Plot des Modells zur ?bersicht.
  #train.cart=predict(modfit,newdata=training_complete)#Vorhersagefunktion, um die G?te des Modells zu messen: Das Modell wird auf den gleichen Datensatz angewendet, aus dem es entstanden ist.
  #table(train.cart,training_complete$status)#Anzeigen der ?bereinstimmungen und Missinterpretationen zwischen Modell und Datensatz in einer Tabelle.
  
  #Anwenden des Modells:
  #Da die Anwendung des Modell auf den Datensatz, aus dem es entstanden es, ein Zirkelschluss w?re, wird das Modell nun auf den Validierungsdatensatz angewendet:
  pred.cart=predict(modfit,newdata=validierung_cart)
  conf_mat_cart=table(pred.cart,validierung_cart$status)
  TN=TN+conf_mat_cart[1,1]
  FP=FP+conf_mat_cart[1,2]
  TP=TP+conf_mat_cart[2,2]
  FN=FN+conf_mat_cart[2,1]
  }

mod_fit=rpart(as.factor(status)~.,data=training_complete)
pdf("fancy_plot.pdf")
fancyRpartPlot(mod_fit)
dev.off()
print(varImp(modfit))
pdf("varImpPlot.pdf")
plot(varImp(modfit))
dev.off()
correlation = cor(training_complete[,-1])
correlation = round(correlation, 2)
highlyCorrelated = findCorrelation(correlation, cutoff=0.75)
print(highlyCorrelated)

pred_cart=predict(mod_fit,newdata=validierung_cart,type="prob")
write.csv(cbind(label=as.numeric(as.character(validierung_cart$status)), pred_cart),file="CART/cart_pred.csv")
