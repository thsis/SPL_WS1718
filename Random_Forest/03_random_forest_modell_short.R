#Modelle bilden:

source("Preparation/data.preparation.R")

library("party")#
library("randomForest")#
library("SDMTools")#
library("ROCR")#
library("raster")
library("rpart")
library("rpart.plot")
library("rattle")
library("caret")

### Trainings- und Testsubset erstellen:
data_clean$T2 = factor(data_clean$T2, levels = c(0,1), labels = c(0,1))

training=subset(data_clean,data_clean$JAHR<2000) #Trainingsdatensatz für das CART-Modell erstellen
validierung=subset(data_clean,data_clean$JAHR>=2000) #Validierungsdatensatz für das CART-Modell erstellen

#########################################################################################################
#########################################################################################################
### random forest Modell I (package randomForest):
validierung_rf=validierung#Da für das logit-Modell kein Training benötigt wird, wird gleich der Validierungsdatensatz genutzt
names(validierung_rf)[2]="status" #Umbenennen der ersten Spalte, damit kein Punkt im Spaltennamen vorkommt
validierung_cart=validierung_rf[,c(-1,-3)]

#Bootstrapping:

training_insolvent=training[training$T2==1,-c(1,3)] #Subset erstellen, das nur die insolventen Firmen aus dem Trainingsdatensatz enthält
training_solvent_full=training[training$T2==0, -c(1,3)]#Ein Subset des Trainingsdatensatzes wird erstellt, das nur die solventen Firmen enthält.

TN=0
TP=0
FP=0
FN=0
for (i in 1:30){#30x Modell mit Zufallszahlen bilden:
  randum_numb=round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$T2==0,])))
  training_solvent=training_solvent_full[randum_numb,]#Aus diesem Subset wird ein weiteres Subset erstellt, dass nur die solventen Firmen enthält, die als Indices die zuvor gezogenen Zufallszahlen haben.
  training_complete=rbind(training_insolvent,training_solvent)#Die gleich langen Datensätze der solventen und insolventen Firmen werden verbunden.
  
  
  #Trainieren des Modells:
  names(training_complete)[1]="status" #Die erste Spalte der Trainingsmatrix wird umbenannt, weil das Modell Punkte im Spaltennamen nicht handlen kann.
  mod_forest_I=randomForest(as.factor(status)~.,data=training_complete,importance=T,ntree = 2000, maxnodes= 100, norm.votes = F)#Random Forest Modell erstellen: Status in Abh. aller andere Variablenmodfit=train(status~.,method="rpart",data=training_complete)#Modellbildung: status in abh. aller anderen Variablen des Datensatzes, Methode "rpart" = CART, Datensatz=Trainingsdatensatz mit gleicher Anzahl solventer und insolventer Firmen.
  
  #Anwenden des Modells:
  
  pred_rf=as.data.frame(predict(mod_forest_I, validierung_rf))#Anwendung des Modell auf den Validierungsdatensatz
  conf_mat_rf=table((as.numeric(unlist(pred_rf))-1),validierung_rf$status)##Anzeigen der Übereinstimmungen und Missinterpretationen zwischen Modell und Datensatz in einer Tabelle.
  TN=TN+conf_mat_rf[1,1]
  FP=FP+conf_mat_rf[1,2]
  TP=TP+conf_mat_rf[2,2]
  FN=FN+conf_mat_rf[2,1]
  }
pred_rf=as.data.frame(predict(mod_forest_I, validierung_rf, type = "prob"))
write.csv(cbind(label=validierung_rf$status,pred_rf),file="rf_pred.csv")
