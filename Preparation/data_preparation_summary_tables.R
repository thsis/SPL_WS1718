### Libraries installieren und starten
#install.packages("randomForest")
library("party")#
library("randomForest")#
library("SDMTools")#
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
### summary tables erstellen (entsprechend des Papers):
#any(is.na(...)) --> Falls ein Wert (Ratio) NA ist --> kommt vor, wenn Ratios gebildet werden, dann erstmal überspringen
summary_table_solvent=matrix(NA,28,6)
for (i in 1:28){
  #if (any(is.na(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)])))==F){
  summary_table_solvent[i,1]=round(min(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)])),2) #Minimum
  summary_table_solvent[i,2:4]=round(quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)]), probs = c(0.25,0.5,0.75)),2) #25%-, 50%- und 75%-Quantile abspeichern
  summary_table_solvent[i,5]=round(quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)]), probs = c(0.75))-quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)]), probs = c(0.25)),2) #Interquartilsabstand abspeichern
  summary_table_solvent[i,6]=round(max(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)])),2)#Maximum abspeichern
}#}
colnames(summary_table_solvent)=c("Min","q0.25","q0.5","q0.75","IQR","Max") #Vergeben der Spaltenüberschriften
write.csv(summary_table_solvent,file="summary_table_solvent.csv")#Schreiben der Summarytabellen in eine csv-Datei

#Analog zur Tabelle der solventen Firmen (oben) wird hier mit den insolventen Firmen verfahren:
summary_table_insolvent=matrix(NA,28,6)
for (i in 1:28){
  #if (any(is.na(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)])))==F){
  summary_table_insolvent[i,1]=round(min(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)])),2)
  summary_table_insolvent[i,2:4]=round(quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)]), probs = c(0.25,0.5,0.75)),2)
  summary_table_insolvent[i,5]=round(quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)]), probs = c(0.75))-quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)]), probs = c(0.25)),2)
  summary_table_insolvent[i,6]=round(max(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)])),2)
}#}
colnames(summary_table_insolvent)=c("Min","q0.25","q0.5","q0.75","IQR","Max")
write.csv(summary_table_insolvent,file="summary_table_insolvent.csv")