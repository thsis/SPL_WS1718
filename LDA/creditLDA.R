source("Preparation/data.preparation.R")
source("LDA/lda.R")
set.seed(42)

data_clean$T2 = factor(data_clean$T2, levels = c(0, 1))
data_clean$ID = factor(data_clean$ID)
data_clean$JAHR = factor(data_clean$JAHR)

solvent = which(data_clean$T2 == 0)
insolvent = which(data_clean$T2 == 1)

train_insolvent = sample(x = insolvent,
                         size = 600)
train_solvent = sample(x = solvent,
                       size = 600)

train = data_clean[c(train_insolvent, train_solvent), ] 

creditLDA = lda(data = train,
                by = "T2")
plot(creditLDA)
