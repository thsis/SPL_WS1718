#Modelle bilden:
source("Preparation/data.preparation.R") 

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
### CART-Modell I:

validierung_cart=validierung#Da für das logit-Modell kein Training benötigt wird, wird gleich der Validierungsdatensatz genutzt
names(validierung_cart)[2]="status" #Umbenennen der zweiten Spalte, damit kein Punkt im Spaltennamen vorkommt
validierung_cart=validierung_cart[,c(-1,-3)]#Entfernen der Spalte ID, Jahr und X

#Bootstrapping:
training_insolvent=training[training$T2==1,-c(1,3)] #Subset erstellen, das nur die insolventen Firmen aus dem Trainingsdatensatz enthält
training_solvent_full=training[training$T2==0, -c(1,3)]#Ein Subset des Trainingsdatensatzes wird erstellt, das nur die solventen Firmen enthält.

TN=0
TP=0
FP=0
FN=0
for (i in 1:30){#30x Modell mit Zufallszahlen bilden:
  randum_numb=round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$T2==0,])))
  #Es werden gerundete Zufallszahlen generiert, und zwar so viele wie es insolvente Firmen in der Trainingsmatrix gibt.
  #Diese 389 Werte liegen zwischen 1 und der Anzahl der solventen Firmen im Trainingsvektor.
  #Diese Zufallszahlen werden gleich als Indices benutzt, um zufällige solventen Firmen herauszufiltern.
  training_solvent=training_solvent_full[randum_numb, ]#Aus diesem Subset wird ein weiteres Subset erstellt, dass nur die solventen Firmen enthält, die als Indices die zuvor gezogenen Zufallszahlen haben.
  training_complete=rbind(training_insolvent,training_solvent)#Die gleich langen Datensätze der solventen und insolventen Firmen werden verbunden.
  
  #Trainieren des Modells:
  names(training_complete)[1]="status" #Die erste Spalte der Trainingsmatrix wird umbenannt, weil das Modell Punkte im Spaltennamen nicht handlen kann.
  
  #mod_fit_help=rpart(as.factor(status)~.,data=training_complete)
  #summary(mod_fit_help)
  #printcp(mod_fit_help)
  #mod_fit_help_2=prune.rpart(mod_fit_help,cp=0.5)
  #modfit=train(mod_fit_help)#Modellbildung: status in abh. aller anderen Variablen des Datensatzes, Methode "rpart" = CART, Datensatz=Trainingsdatensatz mit gleicher Anzahl solventer und insolventer Firmen.
  #summary(modfit)
  #print(modfit)
  modfit=train(status~.,method="rpart",data=training_complete)#Modellbildung: status in abh. aller anderen Variablen des Datensatzes, Methode "rpart" = CART, Datensatz=Trainingsdatensatz mit gleicher Anzahl solventer und insolventer Firmen.
  varImp(modfit)
  
  #fancyRpartPlot(modfit$finalModel)#Plot des Modells zur Übersicht.
  #train.cart=predict(modfit,newdata=training_complete)#Vorhersagefunktion, um die Güte des Modells zu messen: Das Modell wird auf den gleichen Datensatz angewendet, aus dem es entstanden ist.
  #table(train.cart,training_complete$status)#Anzeigen der Übereinstimmungen und Missinterpretationen zwischen Modell und Datensatz in einer Tabelle.
  
  #Anwenden des Modells:
  #Da die Anwendung des Modell auf den Datensatz, aus dem es entstanden es, ein Zirkelschluss wäre, wird das Modell nun auf den Validierungsdatensatz angewendet:
  pred.cart=predict(modfit,newdata=validierung_cart)#Anwendung des Modell auf den Validierungsdatensatz
  conf_mat_cart=table(pred.cart,validierung_cart$status)##Anzeigen der Übereinstimmungen und Missinterpretationen zwischen Modell und Datensatz in einer Tabelle.
  TN=TN+conf_mat_cart[1,1]
  FP=FP+conf_mat_cart[1,2]
  TP=TP+conf_mat_cart[2,2]
  FN=FN+conf_mat_cart[2,1]
  }

mod_fit=rpart(as.factor(status)~.,data=training_complete)
pred_cart=predict(mod_fit,newdata=validierung_cart,type="prob")#Anwendung des Modell auf den Validierungsdatensatz
write.csv(cbind(label=as.numeric(levels(validierung_cart$status)),pred_cart),file="CART/cart_pred.csv")
