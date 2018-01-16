### Libraries installieren und starten

library("party")
library("randomForest")
library("ROCR")
library("raster")
library("rpart")
library("rpart.plot")
library("rattle")
library("caret")

# Einlesen des Datensatzes vom Arbeitspfad.
my_data=read.csv("Data/SPL_data.csv", sep = ";", header=TRUE) 

# Der Datensatz enthält nun auch spätere Jahre, in denen die Industriezweige 
# anders kodiert wurden. Daher ist VAR26 jetzt auch ein Faktor, was wir
# beheben sollten.
my_data = subset(my_data, JAHR < 2003)
my_data$VAR26 = as.numeric(as.character(my_data$VAR26))
my_data$VAR12 = abs(my_data$VAR12)
#-------------------------------------------------------------------------------
# Firmen nach ihrer uniquen ID filtern:
# Finde 1000 insolvente Firmen.
insolvent = unique(my_data[my_data$T2 == 1, "ID"])

# Finde 20000 solvente Firmen, die zu keinem Zeitpunkt insolvent waren.
solvent = unique(my_data[!(my_data$ID %in% insolvent), "ID"])

# Insgesamt 21000 verschiedene Firmen im Datensatz.
subs_my_data_ID = unique(my_data[, "ID"])

#-------------------------------------------------------------------------------
# Herausfiltern der Branchen, die nicht geringfügig vertreten sind (n>1000):
# gering vertretene Branchenblöcke aus dem Datensatz herausfiltern und... 

partition = function(data, var, lower, upper){
  result = subset(data, data[, var] >= lower & data[, var] <= upper)
  return(result)
}

# Das kannst Du auch ignorieren, es ist nur ein ganz kleines bisschen lesbarer.
manufacturing = partition(my_data, "VAR26", 15000, 37205)
construction = partition(my_data, "VAR26", 45000, 45500)
wholesale = partition(my_data, "VAR26", 50000, 52742)
real_estate = partition(my_data, "VAR26", 70000, 74848)

# ...und die restlichen 4 Blöcke wieder zu einer Tabelle zusammenfassen:
# Nur rbind reicht.
test_data = rbind(manufacturing,
                  construction,
                  wholesale,
                  real_estate)

#-------------------------------------------------------------------------------
# unique Firmen nach ihrer ID filtern:
# 17532 verschiedene Firmen
small = unique(test_data$ID)
# davon sind 949 insolvent
insolvent_small = unique(test_data[test_data$T2 == 1, "ID"])
# und 16583 solvent.
solvent_small = unique(test_data[!(test_data$ID %in% insolvent_small), "ID"])

#-------------------------------------------------------------------------------
# Umlaufvermögen filtern
# Wenn Du test_data nicht mehr brauchst, kannst Du es überschreiben

# Es bleiben 16011 übrig (da unsere selbstgeschriebene Funktion inclusive Inter-
# vallgrenzen verwendet).
test_data = partition(test_data, "VAR3", 10^5, 10^8)

#-------------------------------------------------------------------------------
# Firmen aus dem Jahr 1996 herausfiltern, da keine insolvente dabei ist:
# unique Jahreswerte anzeigen: 1996:2002
unique(test_data$JAHR)
#-->keine insolventen Firmn im Jahr 1996
subs_insolvent_1996=subset(test_data, test_data$JAHR==1996 & test_data$T2 == 1)

# also neue Tabelle erstellen, die alle Daten enthält - außer aus dem Jahr 1996:
test_data = partition(test_data, "JAHR", 1997, 2002)

# Ausreißer entfernen: Alles außerhalb des 5%- und 95%-Quantils wird entfernt
# Das Argument '...' leitet anonyme Argumente weiter an quantile().
remove_outliers = function(data, l_quantile = 0.05, u_quantile = 0.95,
                           debug = FALSE, ...){
  # Verwende nur numerische Spalten
  num_cols = sapply(X = data, FUN = is.numeric)
  numeric_data = data[, num_cols]
  
  is.between_quantile = function(x, int, ...){
    ind_low = x >= quantile(x, int[1], ...)
    ind_high = x <= quantile(x, int[2], ...) 
    return(ind_low & ind_high)
  }
  
  if(debug){
    qs = apply(X = numeric_data,
               MARGIN = 2,
               quantile,
               probs = c(l_quantile, u_quantile),
               ...)
    print(qs)
  }
  
  # Prüfe für alle Spalten, ob der Wert einer Zelle zwischen den Quantilen
  # der Spalte liegt. 
  index_matrix = apply(X = numeric_data,
                       MARGIN = 2,
                       is.between_quantile,
                       int = c(l_quantile, u_quantile),
                       ...)
  
  # Finde die Indizes, für die alle Werte einer Zeile TRUE sind (wenn mindestens
  # eine Zeile FALSE ist, ist das Produkt 0; 1 kann also nur dann herauskommen
  # wenn alle Spalten einer Zeile TRUE sind).
  indices = apply(X = index_matrix, MARGIN = 1, all)
  
  return(data[indices, ])
}

# Hier entfernt mein Code viel mehr Firmen als Deiner. Das kann daran liegen, dass ich
# bei allen Variablen die Ausreißer entferne (vielleicht wird es besser, wenn man nur
# gezielt Spalten vorgibt).

# Außerdem konnte ich Deinen loop bei mir nicht zum Laufen kriegen.
test_data_clean = remove_outliers(test_data, na.rm = TRUE)
#--------------------------------------------------------------------------------------
### Alle Zeilen excludieren, bei denen eine der folgenden Variablen 0 ist: Zielgröße 
# ca. 10366, d.h. ca. 4390 müssen aussortiert werden. Dieser Filter wurde a posteriori
# gesetzt, nachdem die Tabelle mit den Ratios (s.u.) aufgebaut wurde, da erst danach
# klar war, welche Quotienten hinsichtlich vorkommender Nullen bzw. NAs bearbeitet
# werden m?ssen.

library("dplyr")
# Ich konnte bei allem was wir bisher herausgeworfen haben, keine NA's bei VAR19 oder 
# VAR20 finden.

test_data_clean = test_data_clean %>% filter(VAR16 != 0,
                                             VAR20 != 0,
                                             VAR19 != 0,
                                             VAR12 != 0,
                                             VAR2  != 0,
                                             VAR1  != 0) 

# Weil is.na einen logischen Wert zurückgibt, kann man das einfach mit '!' invertieren
# Ebit --> NAs entfernen
test_data_clean=test_data_clean[!is.na(test_data_clean$VAR20), ]
# Inte --> NAs entfernen
test_data_clean=test_data_clean[!is.na(test_data_clean$VAR19), ]

# Anzahl der Firmen (solvent und insolvent) anzeigen:
table(test_data_clean$T2)

#--------------------------------------------------------------------------------------
# Tabelle mit den Ratios erstellen:
test_data_ratio = test_data_clean %>%
  mutate(NITA=VAR22/VAR6, # NI/TA,
         NISALE=VAR22/VAR16, # NI/Sale,
         OITA=VAR21/VAR6, # OI/TA,
         OISALE=VAR21/VAR16, # OI/Sale,
         EBITTA=VAR20/VAR6, # EBIT/TA,
         EBITADTA=(VAR20+VAR18)/VAR6, # (EBIT+AD)/TA,
         EBITSALE=VAR20/VAR16, # EBIT/Sale,
         OFTA=VAR9/VAR6, # OF/TA,
         OFITGA=(VAR9-VAR5)/(VAR6-VAR5-VAR1-VAR8), # (OF-ITGA)/(TA-ITGA-CASH-LB),
         CLTA=VAR12/VAR6, # CL/TA,
         CLCASHTA=(VAR12-VAR1)/VAR6, # (CL-CASH)/TA,
         TLTA=(VAR12+VAR13)/VAR6, # TL/TA,
         DEPTTA=VAR14/VAR6, # Dept/TA,
         EBITINTE=VAR20/VAR19, # EBIT/Inte,
         CASHTA=VAR1/VAR6, # Cash/TA,
         CASHCL=VAR1/VAR12, # Cash/CL,
         QACL=(VAR3-VAR2)/VAR12, # QA/CL,
         CACL=VAR3/VAR12, # CA/CL,
         WCTA=(VAR3-VAR12)/VAR6, # WC/TA,
         CLTA=VAR12/(VAR12+VAR13), # CL/TL,
         TASALE=VAR6/VAR16, # TA/Sale,
         INVSALE=VAR2/VAR16, # Inv/Sale,
         ARSALE=VAR7/VAR16, # AR/Sale,
         APSALE=VAR15/VAR16, # AP/Sale,
         logTA=log(VAR6), # Log(TA),
         IDINVINV=VAR23/VAR2, # IDINV/INV,
         IDLTL=VAR24/(VAR12+VAR13), # IDL/TL,
         IDCASH=VAR25/VAR1) # IDCASH/Cash)

# Wenn man nur die Variablen in remove_outliers() reinwirft, die für die Ratios
# gebraucht werden:

relevant = c("JAHR", "T2",
             "VAR1",  "VAR2",  "VAR3", 
             "VAR5",  "VAR6",  "VAR7",
             "VAR8",  "VAR9",  "VAR12",
             "VAR13", "VAR14", "VAR15",
             "VAR16", "VAR18", "VAR19",
             "VAR20", "VAR21", "VAR22",
             "VAR23", "VAR24", "VAR25")

# Hier bleiben 7868 Firmen übrig.
test_data_rel = remove_outliers(test_data[, relevant])

test_data_rel = test_data_rel %>%
  mutate(NITA=VAR22/VAR6, # NI/TA,
         NISALE=VAR22/VAR16, # NI/Sale,
         OITA=VAR21/VAR6, # OI/TA,
         OISALE=VAR21/VAR16, # OI/Sale,
         EBITTA=VAR20/VAR6, # EBIT/TA,
         EBITADTA=(VAR20+VAR18)/VAR6, # (EBIT+AD)/TA,
         EBITSALE=VAR20/VAR16, # EBIT/Sale,
         OFTA=VAR9/VAR6, # OF/TA,
         OFITGA=(VAR9-VAR5)/(VAR6-VAR5-VAR1-VAR8), # (OF-ITGA)/(TA-ITGA-CASH-LB),
         CLTA=VAR12/VAR6, # CL/TA,
         CLCASHTA=(VAR12-VAR1)/VAR6, # (CL-CASH)/TA,
         TLTA=(VAR12+VAR13)/VAR6, # TL/TA,
         DEPTTA=VAR14/VAR6, # Dept/TA,
         EBITINTE=VAR20/VAR19, # EBIT/Inte,
         CASHTA=VAR1/VAR6, # Cash/TA,
         CASHCL=VAR1/VAR12, # Cash/CL,
         QACL=(VAR3-VAR2)/VAR12, # QA/CL,
         CACL=VAR3/VAR12, # CA/CL,
         WCTA=(VAR3-VAR12)/VAR6, # WC/TA,
         CLTA=VAR12/(VAR12+VAR13), # CL/TL,
         TASALE=VAR6/VAR16, # TA/Sale,
         INVSALE=VAR2/VAR16, # Inv/Sale,
         ARSALE=VAR7/VAR16, # AR/Sale,
         APSALE=VAR15/VAR16, # AP/Sale,
         logTA=log(VAR6), # Log(TA),
         IDINVINV=VAR23/VAR2, # IDINV/INV,
         IDLTL=VAR24/(VAR12+VAR13), # IDL/TL,
         IDCASH=VAR25/VAR1) # IDCASH/Cash)

#########################################################################################################
#########################################################################################################
# Modelle bilden:
# Trainings- und Testsubset erstellen:
training=subset(test_data_rel, test_data_rel$JAHR < 2000) # Trainingsdatensatz f?r das CART-Modell erstellen
validierung=subset(test_data_rel,test_data_rel$JAHR >= 2000) # Validierungsdatensatz f?r das CART-Modell erstellen

#Anzahl der Firmen im jeweiligen Datensatz (solvent vs. insolvent)
#Die Zahlen weichen etwas von denen im Paper ab. Das kann einen Grund z.B. bei dem Aussortieren von Ausrei?ern haben.
nrow(training[training$T2 == 0,]) # Training/solvent: 2663
nrow(training[training$T2 == 1,]) # Training/insolvent: 304
nrow(validierung[validierung$T2 == 0,]) # Test/solvent: 4430
nrow(validierung[validierung$T2 == 1,]) # Test/insolvent: 300

# Ergebnis: Ungleichgewicht zwischen der Anzahl der solventen und insolventen Firmen.

#########################################################################################################
### CART-Modell I:

validierung_cart = validierung
names(validierung_cart)[2] = "status" # Umbenennen der zweiten Spalte, damit kein Punkt im Spaltennamen vorkommt
validierung_cart=validierung_cart[, -1] # Entfernen der Spalte Jahr

# Bootstrapping:
#Subset erstellen, das nur die insolventen Firmen aus dem Trainingsdatensatz enthält
training_insolvent = training[training$T2 == 1,] 
# Ein Subset des Trainingsdatensatzes wird erstellt, das nur die solventen Firmen enthält.
training_solvent_full = training[training$T2 == 0,] 

# Matrix, die sp?ter die Vergleichswerte zwischen Modell und Datensatz beinhaltet (true positives).
conf_mat_true_cart = 0
# Matrix, die später die Vergleichswerte zwischen Modell und Datensatz beinhaltet (all values).
conf_mat_sum_cart = 0 

# Beim Bootstrappen setzt man vorher immer eine Saat, um die Ergebnisse replizieren zu können.
set.seed(42)

for (i in 1:30){ 
  # 30x Modell mit Zufallszahlen bilden:
  randum_numb = sample(x = 1:nrow(training_insolvent),
                       size = nrow(training_insolvent), 
                       replace = TRUE)
  
  # Es werden gerundete Zufallszahlen generiert, und zwar so viele wie es insolvente Firmen in der Trainingsmatrix gibt.
  # Diese 389 Werte liegen zwischen 1 und der Anzahl der solventen Firmen im Trainingsvektor.
  # Diese Zufallszahlen werden gleich als Indices benutzt, um zuf?llige solventen Firmen herauszufiltern.
  
  # Aus diesem Subset wird ein weiteres Subset erstellt, dass nur die solventen Firmen enth?lt,
  # die als Indices die zuvor gezogenen Zufallszahlen haben.
  
  training_solvent = training_solvent_full[randum_numb, ]
  # Die gleich langen Datens?tze der solventen und insolventen Firmen werden verbunden.
  training_complete = rbind(training_insolvent,
                            training_solvent)
  # Die Spalte "JAHR" wird entfernt, weil sie f?r die weitere Modellbildung nicht notwendig ist.
  training_complete = training_complete[,-1]
  
  
  # Trainieren des Modells:
  # Die erste Spalte der Trainingsmatrix wird umbenannt, weil das Modell Punkte im Spaltennamen nicht handlen kann.
  names(training_complete)[1] = "status" 
  
  # Modellbildung: status in abh. aller anderen Variablen des Datensatzes, 
  # Methode "rpart" = CART, Datensatz=Trainingsdatensatz mit gleicher 
  # Anzahl solventer und insolventer Firmen.
  modfit=train(factor(status) ~ .,
               method="rpart",
               data=training_complete[complete.cases(training_complete), ])
  
  
  #Anwenden des Modells:
  # Da die Anwendung des Modell auf den Datensatz, aus dem es entstanden es, ein Zirkelschluss w?re, 
  # wird das Modell nun auf den Validierungsdatensatz angewendet:
  # Anwendung des Modell auf den Validierungsdatensatz
  pred.cart = predict(modfit, newdata=validierung_cart)
  # Anzeigen der Übereinstimmungen und Missinterpretationen zwischen Modell und Datensatz in einer Tabelle.
  conf_mat_cart=table(pred.cart, validierung_cart[complete.cases(validierung_cart), "status"])
  # Anzahl der Übereinstimmungen aus der Tabelle übernehmen --> wird mit jedem Schleifendurchlauf um das weitere Modellergebnis ergänzt
  conf_mat_true_cart = sum(conf_mat_true_cart,
                           conf_mat_cart[1,1],
                           conf_mat_cart[2,2],
                           na.rm=TRUE)
  
  # Gesamtsumme der Werte übernehmen --> wird mit jedem Schleifendurchlauf um das weitere Modellergebnis ergänzt
  conf_mat_sum_cart = sum(conf_mat_sum_cart + sum(conf_mat_cart, na.rm=T),na.rm=T) 
}
# Korrektheitsrate berechnen.
AR_cart = conf_mat_true_cart/conf_mat_sum_cart 
# AR_cart: ~ 65% Korrektheitsrate


# Visualisierung der Modellgüte - nur von dem Modell, das zuletzt in der Schleife trainiert wurde:
# Anwendung des Modell auf den Validierungsdatensatz
pred_cart=predict(modfit,newdata=validierung_cart,type="prob")

#Vorhersagefunktion: Modell auf den kompletten Datensatz anwenden
pred_cart_2=prediction(pred_cart[, 2],
                       validierung_cart[complete.cases(validierung_cart) , "status"])
# extrahieren der true positive rate und false positive rate
perf_cart=performance(pred_cart_2, "tpr", "fpr") 
# Plot des Modells
plot(perf_cart, col = "red", lwd = 2)
# Referenzlinie, wenn der Vergleich zufällig vorgenommen worden wäre.
abline(0,1,lty=2,col="gray")

#Fläche unter der Kurve berechnen:
korr=performance(pred_cart_2, measure = "auc")
korr=korr@y.values[[1]]
korr #-->Korrektheitsrate (Fl?che unter der Kurve: ~ 0.74

