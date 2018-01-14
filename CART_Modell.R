### Libraries installieren und starten
#install.packages("ROCR")
library("party")#
library("randomForest")#
library("randomForest")#
library("ROCR")#
library("raster")
library("rpart")
library("rpart.plot")
library("rattle")
library("caret")


my_data=read.csv("dataset.csv",sep=";",header=T) #Einlesen des Datensatzes vom Arbeitspfad
#my_data=read.delim("dataset.txt",sep=";",header=T) #Einlesen des originalen txt-Datensatzes vom Arbeitspfad

### Datensatz aufbereiten: Hochstriche entfernen: ###
for (i in 1:ncol(my_data)){ #Schleife aufbauen, die von der ersten bis zur letzten Spalte läuft und ...
  my_data[,i]=gsub("[']", "", my_data[,i])#...sämtliche Hochstriche entfernt
}

### Firmen nach ihrer uniquen ID filtern: ###
subs_my_data_ID=subset(my_data,my_data$X.ID.==unique(my_data$X.ID.)) #genau 21000 unique IDs (=Firmen) vorhanden
subs_my_data_ID_solvent=subset(subs_my_data_ID,subs_my_data_ID$X.T2.==0)#genau 20000 --> solvente Firmen
subs_my_data_ID_insolvent=subset(subs_my_data_ID,subs_my_data_ID$X.T2.==1)#genau 1000 --> insolvente Firmen

###Herausfiltern der Branchen, die nicht geringfügig vertreten sind (n>1000): ###
### gering vertretene Branchenblöcke aus dem Datensatz herausfiltern und... ###
manufacturing =subset(my_data,my_data$X.VAR26.>=15000 & my_data$X.VAR26.<=37205)#5730
construction  =subset(my_data,my_data$X.VAR26.>=45000 & my_data$X.VAR26.<=45500)#3173
wholesale     =subset(my_data,my_data$X.VAR26.>=50000 & my_data$X.VAR26.<=52742)#5157
real_estate   =subset(my_data,my_data$X.VAR26.>=70000 & my_data$X.VAR26.<=74848)#3472

### ...und die restlichen 4 Blöcke wieder zu einer Tabelle zusammenfassen: ###
test_data=as.data.frame(rbind(manufacturing,construction,wholesale,real_estate))

### unique Firmen nach ihrer ID filtern: ###
subs_my_data_ID_small=subset(test_data,test_data$X.ID.==unique(test_data$X.ID.)) #genau 17532
subs_my_data_ID_solvent_small=subset(subs_my_data_ID_small,subs_my_data_ID_small$X.T2.==0)#16583
subs_my_data_ID_insolvent_small=subset(subs_my_data_ID_small,subs_my_data_ID_small$X.T2.==1)#949

### Umlaufvermögen filtern ###
test_data_2=subset(test_data,as.numeric(test_data$X.VAR3.)>10^5 & as.numeric(test_data$X.VAR3.)<10^8)#übrig:16010

### Firmen aus dem Jahr 1996 herausfiltern, da keine insolvente dabei ist:

unique(test_data_2$X.JAHR.)#unique Jahreswerte anzeigen: 1996:2002
subs_insolvent_1996=subset(test_data_2,test_data_2$X.JAHR.==1996 & test_data_2$X.T2.==1)#-->keine insolventen Firmn im Jahr 1996

### also neue Tabelle erstellen, die alle Daten enthält - außer aus dem Jahr 1996: ###
test_data_3=subset(test_data_2,test_data_2$X.JAHR.!=1996)

###Ausreißer entfernen: Alles außerhalb des 5%- und 95%-Quantils wird entfernt
test_data_4=test_data_3 #Der Datensatz wird kopiert
for (i in 1:26){ #...und für jede Spalte des Datensatzes...
  for (j in 1:nrow(test_data_4)){ #...und jede Zeile des Datensatzes...
    if (is.na(test_data_4[j,i+3]==F)){ #...wird zunächst geprüft, ob das Element NA ist, und wenn nicht...
      if(as.numeric(test_data_4[j,i+3])<as.numeric(quantile(as.numeric(test_data_3[,i+3]), probs = c(0.05),na.rm=T))){ #...wird jedes Element der Spalte geprüft, ob es unterhalb des 5%-Quantils liegt. Und wenn ja, ...
        test_data_4[j,i+3]=NA}#...dann wird dieses element auf NA gesetzt. (Der Vergleich erfolgt immer mit dem jeweiligen Element aus test_data_4 mit der Spalte aus test_data_3, um das Quantil konstant zu halten).
      if(as.numeric(test_data_4[j,i+3])>as.numeric(quantile(as.numeric(test_data_3[,i+3]), probs = c(0.95),na.rm=T))){#Analog wird mit dem 95%-Quantil vorgegangen. Wenn das Element in test_data_4 > als das 95%-Quantil der Spalte in test_data_3 ist, dann...
        test_data_4[j,i+3]=NA}#...wird es auf NA gesetzt.
    }}}
test_data_3=test_data_4 #Dann wird der ursprüngliche Datensatz test_data_3 durch den bereinigten Datensatz test_data_4 ersetzt

### Alle Zeilen excludieren, bei denen eine der folgenden Variablen 0 ist: Zielgröße ca. 10366, d.h. ca. 4390 müssen aussortiert werden
#Dieser Filter wurde a posteriori gesetzt, nachdem die Tabelle mit den Ratios (s.u.) aufgebaut wurde, da erst danach klar war, welche Quotienten hinsichtlich vorkommender Nullen bzw. NAs bearbeitet werden müssen.
test_data_3=test_data_3[test_data_3$X.VAR16.!=0,]#sales --> entfernen der Nullen
test_data_3=test_data_3[is.na(test_data_3$X.VAR20.)==F,]#Ebit --> NAs entfernen
test_data_3=test_data_3[is.na(test_data_3$X.VAR19.)==F,]#Inte --> NAs entfernen
test_data_3=test_data_3[test_data_3$X.VAR20.!=0,]#Ebit --> entfernen der Nullen
test_data_3=test_data_3[test_data_3$X.VAR19.!=0,]#Inte --> entfernen der Nullen
test_data_3=test_data_3[test_data_3$X.VAR12.!=0,]#CL --> entfernen der Nullen
test_data_3=test_data_3[test_data_3$X.VAR2.!=0,]#Inv --> entfernen der Nullen
test_data_3=test_data_3[test_data_3$X.VAR1.!=0,]#Cash --> entfernen der Nullen

#Anzahl der Firmen (solvent und insolvent) anzeigen:
nrow(test_data_3[test_data_3$X.T2.==0,])
nrow(test_data_3[test_data_3$X.T2.==1,])

### Tabelle mit den Ratios erstellen:
test_data_ratio=test_data_3[,1:3]
test_data_ratio[,4]=as.numeric(test_data_3$X.VAR22.)/as.numeric(test_data_3$X.VAR6.)#NI/TA
test_data_ratio[,5]=as.numeric(test_data_3$X.VAR22.)/as.numeric(test_data_3$X.VAR16.)#NI/Sale
test_data_ratio[,6]=as.numeric(test_data_3$X.VAR21.)/as.numeric(test_data_3$X.VAR6.)#OI/TA
test_data_ratio[,7]=as.numeric(test_data_3$X.VAR21.)/as.numeric(test_data_3$X.VAR16.)#OI/Sale
test_data_ratio[,8]=as.numeric(test_data_3$X.VAR20.)/as.numeric(test_data_3$X.VAR6.)#EBIT/TA
test_data_ratio[,9]=(as.numeric(test_data_3$X.VAR20.)+as.numeric(test_data_3$X.VAR18.))/as.numeric(test_data_3$X.VAR6.)#(EBIT+AD)/TA
test_data_ratio[,10]=as.numeric(test_data_3$X.VAR20.)/as.numeric(test_data_3$X.VAR16.)#EBIT/Sale
test_data_ratio[,11]=as.numeric(test_data_3$X.VAR9.)/as.numeric(test_data_3$X.VAR6.)#OF/TA
test_data_ratio[,12]=(as.numeric(test_data_3$X.VAR9.)-as.numeric(test_data_3$X.VAR5.))/(as.numeric(test_data_3$X.VAR6.)-as.numeric(test_data_3$X.VAR5.)-as.numeric(test_data_3$X.VAR1.)-as.numeric(test_data_3$X.VAR8.))#(OF-ITGA)/(TA-ITGA-CASH-LB)
test_data_ratio[,13]=as.numeric(test_data_3$X.VAR12.)/as.numeric(test_data_3$X.VAR6.)#CL/TA
test_data_ratio[,14]=(as.numeric(test_data_3$X.VAR12.)-as.numeric(test_data_3$X.VAR1.))/as.numeric(test_data_3$X.VAR6.)#(CL-CASH)/TA
test_data_ratio[,15]=(as.numeric(test_data_3$X.VAR12.)+as.numeric(test_data_3$X.VAR13.))/as.numeric(test_data_3$X.VAR6.)#TL/TA
test_data_ratio[,16]=as.numeric(test_data_3$X.VAR14.)/as.numeric(test_data_3$X.VAR6.)#Dept/TA
test_data_ratio[,17]=as.numeric(test_data_3$X.VAR20.)/as.numeric(test_data_3$X.VAR19.)#EBIT/Inte
test_data_ratio[,18]=as.numeric(test_data_3$X.VAR1.)/as.numeric(test_data_3$X.VAR6.)#Cash/TA
test_data_ratio[,19]=as.numeric(test_data_3$X.VAR1.)/as.numeric(test_data_3$X.VAR12.)#Cash/CL
test_data_ratio[,20]=(as.numeric(test_data_3$X.VAR3.)-as.numeric(test_data_3$X.VAR2.))/as.numeric(test_data_3$X.VAR12.)#QA/CL
test_data_ratio[,21]=as.numeric(test_data_3$X.VAR3.)/as.numeric(test_data_3$X.VAR12.)#CA/CL
test_data_ratio[,22]=(as.numeric(test_data_3$X.VAR3.)-as.numeric(test_data_3$X.VAR12.))/as.numeric(test_data_3$X.VAR6.)#WC/TA
test_data_ratio[,23]=as.numeric(test_data_3$X.VAR12.)/(as.numeric(test_data_3$X.VAR12.)+as.numeric(test_data_3$X.VAR13.))#CL/TL
test_data_ratio[,24]=as.numeric(test_data_3$X.VAR6.)/as.numeric(test_data_3$X.VAR16.)#TA/Sale
test_data_ratio[,25]=as.numeric(test_data_3$X.VAR2.)/as.numeric(test_data_3$X.VAR16.)#Inv/Sale
test_data_ratio[,26]=as.numeric(test_data_3$X.VAR7.)/as.numeric(test_data_3$X.VAR16.)#AR/Sale
test_data_ratio[,27]=as.numeric(test_data_3$X.VAR15.)/as.numeric(test_data_3$X.VAR16.)#AP/Sale
test_data_ratio[,28]=log(as.numeric(test_data_3$X.VAR6.))#Log(TA)
test_data_ratio[,29]=as.numeric(test_data_3$X.VAR23.)/as.numeric(test_data_3$X.VAR2.)#IDINV/INV
test_data_ratio[,30]=as.numeric(test_data_3$X.VAR24.)/(as.numeric(test_data_3$X.VAR12.)+as.numeric(test_data_3$X.VAR13.))#IDL/TL
test_data_ratio[,31]=as.numeric(test_data_3$X.VAR25.)/as.numeric(test_data_3$X.VAR1.)#IDCASH/Cash

#########################################################################################################
#########################################################################################################
#Modelle bilden:
### Trainings- und Testsubset erstellen:
training=subset(test_data_ratio,test_data_ratio$X.JAHR.<2000) #Trainingsdatensatz für das CART-Modell erstellen
validierung=subset(test_data_ratio,test_data_ratio$X.JAHR.>=2000) #Validierungsdatensatz für das CART-Modell erstellen

#Anzahl der Firmen im jeweiligen Datensatz (solvent vs. insolvent)
#Die Zahlen weichen etwas von denen im Paper ab. Das kann einen Grund z.B. bei dem Aussortieren von Ausreißern haben.
nrow(training[training$X.T2.==0,])#Training/solvent:3604
nrow(training[training$X.T2.==1,])#Training/insolvent:389
nrow(validierung[validierung$X.T2.==0,])#Test/solvent:6352
nrow(validierung[validierung$X.T2.==1,])#Test/insolvent:394
#Ergebnis: Ungleichgewicht zwischen der Anzahl der solventen und insolventen Firmen.

#########################################################################################################
### CART-Modell I:

validierung_cart=validierung#
names(validierung_cart)[2]="status" #Umbenennen der zweiten Spalte, damit kein Punkt im Spaltennamen vorkommt
validierung_cart=validierung_cart[,-1]#Entfernen der Spalte ID
validierung_cart=validierung_cart[,-2]#Entfernen der Spalte Jahr

#Bootstrapping:
training_insolvent=training[training$X.T2.==1,] #Subset erstellen, das nur die insolventen Firmen aus dem Trainingsdatensatz enthält
training_solvent_full=training[training$X.T2.==0,]#Ein Subset des Trainingsdatensatzes wird erstellt, das nur die solventen Firmen enthält.

conf_mat_true_cart=0#Matrix, die später die Vergleichswerte zwischen Modell und Datensatz beinhaltet (true positives).
conf_mat_sum_cart=0#Matrix, die später die Vergleichswerte zwischen Modell und Datensatz beinhaltet (all values).

for (i in 1:30){#30x Modell mit Zufallszahlen bilden:
  randum_numb=round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$X.T2.==0,])))
  #Es werden gerundete Zufallszahlen generiert, und zwar so viele wie es insolvente Firmen in der Trainingsmatrix gibt.
  #Diese 389 Werte liegen zwischen 1 und der Anzahl der solventen Firmen im Trainingsvektor.
  #Diese Zufallszahlen werden gleich als Indices benutzt, um zufällige solventen Firmen herauszufiltern.
  training_solvent=training_solvent_full[randum_numb,]#Aus diesem Subset wird ein weiteres Subset erstellt, dass nur die solventen Firmen enthält, die als Indices die zuvor gezogenen Zufallszahlen haben.
  training_complete=rbind(training_insolvent,training_solvent)#Die gleich langen Datensätze der solventen und insolventen Firmen werden verbunden.
  training_complete=training_complete[,-1]#Die Spalte "ID" wird entfernt, weil sie für die weitere Modellbildung nicht notwendig ist.
  training_complete=training_complete[,-2]#Die Spalte "Jahr" wird entfernt, weil sie für die weitere Modellbildung nicht notwendig ist.
  
  #Trainieren des Modells:
  names(training_complete)[1]="status" #Die erste Spalte der Trainingsmatrix wird umbenannt, weil das Modell Punkte im Spaltennamen nicht handlen kann.
  modfit=train(status~.,method="rpart",data=training_complete)#Modellbildung: status in abh. aller anderen Variablen des Datensatzes, Methode "rpart" = CART, Datensatz=Trainingsdatensatz mit gleicher Anzahl solventer und insolventer Firmen.
  #fancyRpartPlot(modfit$finalModel)#Plot des Modells zur Übersicht.
  #train.cart=predict(modfit,newdata=training_complete)#Vorhersagefunktion, um die Güte des Modells zu messen: Das Modell wird auf den gleichen Datensatz angewendet, aus dem es entstanden ist.
  #table(train.cart,training_complete$status)#Anzeigen der Übereinstimmungen und Missinterpretationen zwischen Modell und Datensatz in einer Tabelle.
  
  #Anwenden des Modells:
  #Da die Anwendung des Modell auf den Datensatz, aus dem es entstanden es, ein Zirkelschluss wäre, wird das Modell nun auf den Validierungsdatensatz angewendet:
  pred.cart=predict(modfit,newdata=validierung_cart)#Anwendung des Modell auf den Validierungsdatensatz
  conf_mat_cart=table(pred.cart,validierung_cart$status)##Anzeigen der Übereinstimmungen und Missinterpretationen zwischen Modell und Datensatz in einer Tabelle.
  conf_mat_true_cart=sum(conf_mat_true_cart,conf_mat_cart[1,1],conf_mat_cart[2,2],na.rm=T)#Anzahl der Übereinstimmungen aus der Tabelle übernehmen --> wird mit jedem Schleifendurchlauf um das weitere Modellergebnis ergänzt
  conf_mat_sum_cart=sum(conf_mat_sum_cart+sum(conf_mat_cart,na.rm=T),na.rm=T)#Gesamtsumme der Werte übernehmen --> wird mit jedem Schleifendurchlauf um das weitere Modellergebnis ergänzt
}
AR_cart=conf_mat_true_cart/conf_mat_sum_cart #Korrektheitsrate berechnen.
#AR_cart: ~68% Korrektheitsrate


#Visualisierung der Modellgüte - nur von dem Modell, das zuletzt in der Schleife trainiert wurde:
pred_cart=predict(modfit,newdata=validierung_cart,type="prob")#Anwendung des Modell auf den Validierungsdatensatz
pred_cart_2=prediction(pred_cart[,2],validierung_cart$status)#Vorhersagefunktion: Modell auf den kompletten Datensatz anwenden
perf_cart=performance(pred_cart_2,"tpr","fpr")#extrahieren der true positive rate und false positive rate
plot(perf_cart,col="red",lwd=2)#Plot des Modells
abline(0,1,lty=2,col="gray")#Referenzlinie, wenn der Vergleich zufällig vorgenommen worden wäre.

#Fläche unter der Kurve berechnen:
korr=performance(pred_cart_2, measure = "auc")
korr=korr@y.values[[1]]
korr #-->Korrektheitsrate (Fläche unter der Kurve: ~0.69

