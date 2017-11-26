# Data Preparation

setwd("~/SPL_WS1718/Data/")

colnames = c(
# Assets
             "Scheck, Kassenbestand",
             "Vorräte -Gesamt",
             "Umlaufvermögen",
             "Sachanlagen -Gesamt",
             "Immaterielle Vermögensgegenstände",
             "Bilanzsumme",
             "Forderungen aus Lieferung und Leistung",
             "Grundstücke und Bauten",
# Liabilities
              "Eigenkapital",
              "Gesellschafterdahrlehen",
              "Pensionsrückstellungen und andere Aufwendungen fürAlterssicherung",
              "kurzfristige Verbindlichkeiten -Gesamt",
              "langfristige Verbindlichkeiten -Gesamt",
              "Bankschulden in den lang- und kurzfristigen Verbindlichkeiten",
              "Verbindlichkeiten aus Lieferung und Leistung",
# Income Statement:
               "Umsätze",
               "Vertiebs- / Verwaltungsaufwand",
               "Abschreibungen",
               "Zinsaufwendungen",
               "EBIT (Gewinn vor Zinsen und Steuern)",
               "Gewinn aus ordentlicher Geschäftstätigkeit (Betriebsgewinn)",
               "Jahresüberschuss",
# Cash-Flow:
               "(Lager)-Bestandsverrringerungen(-zunahme)",
               "Veränderungen der kurz- und langfristigen Verbindlichkeiten gegenüber dem Vorjahr",
               "Bargeld /Kassenbestand / flüssige Mittel - Erhöhung (Verringerung)",
# Others:
               "Branchenzugehörigkeit (Anzahl der Branchen, in denen das Unternehmen tätig ist)",
               "Rechtsform",
               "Anzahl der Mitarbeiter",
               "Forderungen gegenüber Unternehmen mit denen ein Beteiligungsverhältnis besteht",
               "Verbindlichkeiten gegenüber Unternehmen mit denen ein Beteiligungsverhältnis besteht")

# Importing and Preparing Data

data_solvent = read.csv("solvent.txt", sep = ';', dec = '.', header = TRUE, stringsAsFactors = FALSE)
data_insolvent = read.csv("insolvent.txt", sep = ';', dec = '.', header = TRUE, stringsAsFactors = FALSE)
data_complete = rbind(data_solvent, data_insolvent)

# Add indicator to full dataset.
data_complete$insolvent = c(rep(FALSE, dim(data_solvent)[1]), rep(TRUE, dim(data_insolvent)[1]))

# Remove partial data for clean memory. 
rm(data_solvent, data_insolvent)

clean = function(x, cast=c(as.integer, as.character, as.numeric, as.factor)){
  # Replace unwanted characters which were generated during import.
  x = gsub(pattern = "\"|'", replacement = '', x = x)
  # If only the empty string remains, return NA.
  x = ifelse(x == "", NA, x)
  x = cast(x)
  return(x)
}

clean_ints = apply(X = data_complete[, c(1:3, 33)], MARGIN = 2, FUN = clean, cast = as.integer)
clean_nums = apply(X = data_complete[, 4:31], MARGIN = 2, FUN = clean, cast = as.numeric)
clean_char = apply(X = data_complete[, c(32, 34, 35)], MARGIN = 2, FUN = clean, cast=as.factor)

data = cbind(
  clean_ints,
  as.data.frame(clean_nums),
  clean_char,
  "Insolvent" = data_complete$insolvent
)

rm(clean_char, clean_ints, clean_nums, data_complete)

# Reorder names
ordered_names = paste("X.VAR", 1:30, ".", sep = '')
full_names = c(
  "X.ID.",
  "X.T2.",
  "X.JAHR.",
  ordered_names,
  "X.RECHTSKREIS.",
  "X.ABSCHLUSSART.",
  "Insolvent")

# Remove one row containing only missing data
data = data[-5697, full_names]

# Scale Data
data_scaled = data.frame(
  data[, 1:3],
  scale(data[, 1:26 + 3]),
  data[, 30:36])
