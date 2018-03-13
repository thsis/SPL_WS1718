### Environment leeren und Grafiken löschen:
rm(list=ls()) 
dev.off()

### Libraries installieren und starten
#install.packages("RStoolbox")
#library("RStoolbox")
#
install.packages("caret")
library("raster")
library("rpart")
library("rpart.plot")
library("rattle")
library("caret")

### Datenpfad setzen:
my_path <- "C:/Users/Valeryia/Desktop/Valeryia/"
setwd(my_path) #setzen des Arbeitspfades (working directory)
my_data=read.csv("dataset.csv",sep=";",header=T)

### Datensatz aufbereiten: Hochstriche entfernen: ###
for (i in 1:ncol(my_data)){
my_data[,i]=gsub("[']", "", my_data[,i])
}

### unique Firmen nach ihrer ID filtern: ###
subs_my_data_ID=subset(my_data,my_data$X.ID.==unique(my_data$X.ID.)) #genau 21000
subs_my_data_ID_solvent=subset(subs_my_data_ID,subs_my_data_ID$X.T2.==0)#genau 20000
subs_my_data_ID_insolvent=subset(subs_my_data_ID,subs_my_data_ID$X.T2.==1)#genau 1000

## Paper data preparation steps: 
## clean the data of firms whose characteristics are very different from the others...
## ...eliminate firms within industries with small percentage in the industry composition (firms that belong 
## to the 'other' industries in the insolvent and solvent databases)
## only four main types of industry (Construction, Manufacturing, Wholesale & Retail Trade and Real Estate) remain in the study
## exclude Smallest and largest firms, because of their asset size
## clean the database to ensure that the value of some variables, such as the denominator when calculating the ratios, should not be zero
## exclude the firms solvent in 1996 because of missing insolvency values for this year
## 

###Herausfiltern der Branchen, die nicht geringfügig vertreten sind ###


#Histogramm der Häufigkeiten:
hist(as.numeric(my_data$X.VAR26.),freq=T)
hist(as.numeric(subs_my_data_ID$X.VAR26.),freq=T)
#Histogramm der Häufigkeiten der Branchen --> Veranschaulicht wie oft ist im Datensatz welche Branche vertreten; Codierung entsprechend WZ93.

#Häufigkeiten der einzelnen Branchen: Tabelle erstellen, in der jeder Branche ihre Häufigkeit zugeordnet ist: ###
#
branchenzugehoerigkeit=matrix(NA,length(unique(subs_my_data_ID$X.VAR26.)),2)
branchenzugehoerigkeit[,1]=unique(subs_my_data_ID$X.VAR26.)
branchenzugehoerigkeit[,2]=0
for (i in 1:nrow(branchenzugehoerigkeit)){
help_2=as.numeric(branchenzugehoerigkeit[i,1])==as.numeric(subs_my_data_ID$X.VAR26.)
branchenzugehoerigkeit[i,2]=length(help_2[help_2==TRUE])
}

### gering vertretene Branchenblöcke aus dem Datensatz herausfiltern und... ###
manufacturing =subset(my_data,my_data$X.VAR26.>=15000 & my_data$X.VAR26.<=37205)#5730
construction  =subset(my_data,my_data$X.VAR26.>=45000 & my_data$X.VAR26.<=45500)#3173
wholesale     =subset(my_data,my_data$X.VAR26.>=50000 & my_data$X.VAR26.<=52742)#5157
real_estate   =subset(my_data,my_data$X.VAR26.>=70000 & my_data$X.VAR26.<=74848)#3472

### ...und die restlichen Blöcke wieder zu einer Tabelle zusammenfassen: ###
test_data=as.data.frame(rbind(manufacturing,construction,wholesale,real_estate))

### unique Firmen nach ihrer ID filtern: ###
subs_my_data_ID_main=subset(test_data,test_data$X.ID.==unique(test_data$X.ID.)) #genau 17532
subs_my_data_ID_solvent_main=subset(subs_my_data_ID_small,subs_my_data_ID_small$X.T2.==0)#16583
subs_my_data_ID_insolvent_main=subset(subs_my_data_ID_small,subs_my_data_ID_small$X.T2.==1)#949
### we are left with 949 insolvent firms and 16583 solvent firms in four main industries:
### Construction, Manufacturing, Wholesale & Retail Trade and Real Estate

### Umlaufvermögen filtern ###

### exclude smallest and largest firms, because of their asset size ###
### exclude those firms whose asset size is less than 10^5 EUR or greater than 10^8 EUR, because the credit 
## quality of small firms often depends as much on the finances of a key individual as on the firm itself 
## and largest firms rarely go bankrupt in Germany ###

test_data_2=subset(test_data,as.numeric(test_data$X.VAR3.)>10^5 & as.numeric(test_data$X.VAR3.)<10^8)#übrig:16010

### Firmen aus dem Jahr 1996 herausfiltern, da keine insolvente dabei ist:
unique(test_data_2$X.JAHR.)#1996:2002

subs_insolvent_1996=subset(test_data_2,test_data_2$X.JAHR.==1996 & test_data_2$X.T2.==1)#-->keine insolventen Firmn im Jahr 1996
#--> tatsächliche keine Insolvente Firma im Jahr 1996

### also neue Tabelle erstellen, die alle Daten enthält - außer aus dem Jahr 1996: ###
test_data_3=subset(test_data_2,test_data_2$X.JAHR.!=1996)

## clean the database to ensure that the value of some variables, such as the denominator when calculating the ratios, should not be zero
### Alle Zeilen excludieren, bei denen eine der folgenden Variablen 0 ist: Zielgröße ca. 10366, d.h. ca. 4390 müssen aussortiert werden

test_data_3=test_data_3[test_data_3$X.VAR16.!=0,]#sales - 743
test_data_3=test_data_3[is.na(test_data_3$X.VAR20.)==F,]#Ebit --> NAs entfernen
test_data_3=test_data_3[is.na(test_data_3$X.VAR19.)==F,]#Inte --> NAs entfernen
test_data_3=test_data_3[test_data_3$X.VAR20.!=0,]#Ebit
test_data_3=test_data_3[test_data_3$X.VAR19.!=0,]#Inte
test_data_3=test_data_3[test_data_3$X.VAR12.!=0,]#CL
test_data_3=test_data_3[test_data_3$X.VAR2.!=0,]#Inv
test_data_3=test_data_3[test_data_3$X.VAR1.!=0,]#Cash



#Anzahl der Firmen (solvent und insolvent) anzeigen:
nrow(test_data_3[test_data_3$X.T2.==0,])
nrow(test_data_3[test_data_3$X.T2.==1,])

## The Creditreform database provides many financial statement variables for each firm. 
## 28 ratios were selected for the bankruptcy analysis ## 
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
#names(test_data_ratio)[4:31]=c("NI.TA","NI.Sale","OI.TA","OI.SALE","EBIT.TA","EBITDA","EBIT.Sale","OF.TA",
#                                  "OF.ITGA...TA.ITGA.CASH.LB","CL.TA","CL.CASH...TA","TL.TA",
#                                  "DEBT.TA","EBIT.INTE","CASH.TA","CASH.CL","QA.CL","CA.CL","WC.TA",
#                                  "CL.TL","SALE.TA","INV.SALE","AR.SALE","AP.SALE","Log.TA.","IDINV.INV","IDL.TL","IDCASH.CASH")

### summary tables erstellen
##  the table summarize the descriptive statistics of the 28 ratios for both the insolvency and solvency sample
#any(is.na(...)) --> Falls ein Wert (=Ratio) NA ist --> wenn NA kommt vor, bricht die Ausführung ab,
## wenn Ratios gebildet werden, Lösung -> dann erstmal überspringen
summary_table_solvent=matrix(NA,28,6)
for (i in 1:28){
  if (any(is.na(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)])))==F){
summary_table_solvent[i,1]=min(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)]))
summary_table_solvent[i,2:4]=quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)]), probs = c(0.05,0.5,0.95))
summary_table_solvent[i,5]=quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)]), probs = c(0.75))-quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)]), probs = c(0.25))
summary_table_solvent[i,6]=max(as.numeric(test_data_ratio[test_data_ratio$X.T2.==0,(i+3)]))
  }}
colnames(summary_table_solvent)=c("Min","q0.05","q0.5","q0.95","IQR","Max")

summary_table_insolvent=matrix(NA,28,6)
for (i in 1:28){
  if (any(is.na(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)])))==F){
summary_table_insolvent[i,1]=min(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)]))
summary_table_insolvent[i,2:4]=quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)]), probs = c(0.05,0.5,0.95))
summary_table_insolvent[i,5]=quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)]), probs = c(0.75))-quantile(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)]), probs = c(0.25))
summary_table_insolvent[i,6]=max(as.numeric(test_data_ratio[test_data_ratio$X.T2.==1,(i+3)]))
}}
colnames(summary_table_insolvent)=c("Min","q0.05","q0.5","q0.95","IQR","Max")

## The statistics described in summary table reveal that several of the ratios are highly skewed and 
## there are many outliers; this may affect whether they can be of much help in identifying insolvent and solvent firms. 
## It is also possible that many of these outliers are errors of some kind ##

###Ausreißer entfernen ###
## In order to avoid sensitivity to outliers in applying the SVM and the logit model, Chen et al. truncated 
## each financial variable to be between its 5% quantile and 95% quantile ##

outlier=as.numeric(test_data_3$X.VAR1.>mean(as.numeric(test_data_3$X.VAR1.))+2*sd(as.numeric(test_data_3$X.VAR1.))) 
length(outlier[outlier==0])#keine Ausreißer
length(outlier[outlier==1])#Ausreißer


### Trainings- und Testsubset erstellen:
## use the data from 1997 to 1999 to train the model, and use the data from 2000 to 2002 to test the resulting model ##

training=subset(test_data_ratio,test_data_ratio$X.JAHR.<2000)
validierung=subset(test_data_ratio,test_data_ratio$X.JAHR.>=2000)

nrow(training[training$X.T2.==0,])#Training/solvent:3604
nrow(training[training$X.T2.==1,])#Training/insolvent:389
nrow(validierung[validierung$X.T2.==0,])#Test/solvent:6352
nrow(validierung[validierung$X.T2.==1,])#Test/insolvent:394

#Bootstrapping:

## Because the density of insolvent firms is rather low, we need to oversample the insolvent firms in order
## for the models to pick up the patterns predictive of insolvency. This is done through the bootstrap technique ##

training_insolvent=training[training$X.T2.==1,]

randum_numb=round(runif(nrow(training_insolvent), min = 1, max = nrow(training[training$X.T2.==0,])))
training_solvent_full=training[training$X.T2.==0,]
training_solvent=training_solvent_full[randum_numb,]
training_complete=rbind(training_insolvent,training_solvent)
training_complete=training_complete[,-1]
training_complete=training_complete[,-2]

### CART-Modell:

### logit Modell:

### random forest Modell:


