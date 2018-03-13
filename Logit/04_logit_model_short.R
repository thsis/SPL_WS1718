#Modelle bilden:

source("Preparation/data.preparation.R")

library("raster")
library("rpart")
library("rpart.plot")
library("rattle")
library("caret")

### Trainings- und Testsubset erstellen:
data_clean$T2 = factor(data_clean$T2, levels = c(0,1), labels = c(0,1))

training=subset(data_clean,data_clean$JAHR<2000) 
validierung=subset(data_clean,data_clean$JAHR>=2000) 

#########################################################################################################
#########################################################################################################
### logit Modell:
validierung_logit=validierung#Da für das logit-Modell kein Training benötigt wird, wird gleich der Validierungsdatensatz genutzt
names(validierung_logit)[2]="status" #Umbenennen der ersten Spalte, damit kein Punkt im Spaltennamen vorkommt
validierung_logit=validierung_logit[,c(-1,-3)]

#Bootstrapping:
#Trennen des Datensatzes nach solventen und insolventen Firmen:
training_insolvent=training[training$T2==1,-c(1,3)] 
training_solvent_full=training[training$T2==0, -c(1,3)]

TN=0
TP=0
FP=0
FN=0

for (i in 1:30){#30x Modell mit Zufallszahlen bilden:
  randum_numb=round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$T2==0,]))) #Zufallszahlen erstellen, die später als Indices dienen, um genauso viele solvente wie insolvente Firmen zu haben. (wie oben)
  training_solvent=training_solvent_full[randum_numb,]#neuen Datensatz erstellen, der nur die solventen Firmen enthält, auf die die zufällig gezogenen Indices zutreffen.
  #training_complete=rbind(training_insolvent,training_solvent)#Die gleich langen Datensätze der solventen und insolventen Firmen werden verbunden.
  training_complete=rbind(training_insolvent,training_solvent)#Datensatz erstellen, der die gleiche Anzahl solventer und insolver Firmen enthält.
  
  
  #Trainieren des Modells:
  names(training_complete)[1]="status"
  glm_mod=train(as.factor(status)~.,data=training_complete, method="glm", family="binomial")#logistisches Modell erstellen
  #Anwenden des Modells: Da die Anwendung des Modell auf den Datensatz, aus dem es entstanden es, ein Zirkelschluss wäre, wird das Modell nun auf den Validierungsdatensatz angewendet:
  glm_pred = predict(glm_mod, newdata=validierung_logit)#Anwendung des Modell auf den Validierungsdatensatz
  #Korrektheitsrate (AR) berechnen:
  conf_mat_logit=table(glm_pred, validierung_logit$status)#Anzeigen der Übereinstimmungen und Missinterpretationen zwischen Modell und Datensatz in einer Tabelle.
  TN=TN+conf_mat_logit[1,1]
  FP=FP+conf_mat_logit[1,2]
  TP=TP+conf_mat_logit[2,2]
  FN=FN+conf_mat_logit[2,1]
}

pred_logit=predict(glm_mod, newdata=validierung_logit,type="prob")
write.csv(cbind(labels=validierung_logit$status,pred_logit),file="logit_pred.csv")
