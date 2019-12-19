# Analyse Skript 

#### Bibliotheken laden

library(tidyverse)
library(psych)

source("qualtricshelpers.R")

#### Datei laden ----

datensatz <- "data/umfragetestdaten.csv"
raw <- load_qualtrics_csv(datensatz)

#### Daten bereinigen ----
names(raw)
raw.short <- raw[,c(-1:-17, -20:-35, -80:-124)]
names(raw.short)

### Codebook erstellen

generate_codebook(raw.short, datensatz, "data/codebook.csv")
codebook <- read_codebook("data/codebook_final.csv")

## neue Namen auf die Daten anwenden:
names(raw.short) <- codebook$variable

### Variablen den richtigen Typen zuordnen

raw.short$gender <- as.factor(raw.short$gender)

#raw.short$age <- ???

raw.short$devices <- as.factor(raw.short$devices)

  
raw.short$tools_chat <- ordered(raw.short$tools_chat, levels = c("Ich kenne solche Programme nicht",
                                                     "Damit arbeite ich nie",
                                                     "Damit arbeite ich selten",
                                                     "Damit arbeite ich hin und wieder",
                                                     "Damit arbeite ich häufig"))

raw.short$tools_mail <- ordered(raw.short$tools_mail, levels = c("Ich kenne solche Programme nicht",
                                                                 "Damit arbeite ich nie",
                                                                 "Damit arbeite ich selten",
                                                                 "Damit arbeite ich hin und wieder",
                                                                 "Damit arbeite ich häufig"))

raw.short$tools_ticket <- ordered(raw.short$tools_ticket, levels = c("Ich kenne solche Programme nicht",
                                                                 "Damit arbeite ich nie",
                                                                 "Damit arbeite ich selten",
                                                                 "Damit arbeite ich hin und wieder",
                                                                 "Damit arbeite ich häufig"))

raw.short$tools_orga <- ordered(raw.short$tools_orga, levels = c("Ich kenne solche Programme nicht",
                                                                     "Damit arbeite ich nie",
                                                                     "Damit arbeite ich selten",
                                                                     "Damit arbeite ich hin und wieder",
                                                                     "Damit arbeite ich häufig"))

raw.short$ict_contact <- as.factor(raw.short$ict_contact)




raw.short$ict_usage_com <- ordered(raw.short$ict_usage_com, levels = c("Nie",
                                                     "Weniger als einmal im Monat",
                                                     "Weniger als einmal pro Woche",
                                                     "Ein- bis zweimal pro Woche",
                                                     "Täglich",
                                                     "mehrmals täglich"))

raw.short$ict_usage_colab <- ordered(raw.short$ict_usage_colab, levels = c("Nie",
                                                                       "Weniger als einmal im Monat",
                                                                       "Weniger als einmal pro Woche",
                                                                       "Ein- bis zweimal pro Woche",
                                                                       "Täglich",
                                                                       "mehrmals täglich"))

raw.short$ict_working_process_1 <- ordered(raw.short$ict_working_process_1, levels = c("Stimme gar nicht zu",
                                                                           "Stimme nicht zu",
                                                                           "Stimme eher nicht zu",
                                                                           "Stimme eher zu",
                                                                           "Stimme zu",
                                                                           "Stimme völlig zu"))

raw.short$ict_working_process_2 <- ordered(raw.short$ict_working_process_2, levels = c("Stimme gar nicht zu",
                                                                                       "Stimme nicht zu",
                                                                                       "Stimme eher nicht zu",
                                                                                       "Stimme eher zu",
                                                                                       "Stimme zu",
                                                                                       "Stimme völlig zu"))

raw.short$ict_working_process_3 <- ordered(raw.short$ict_working_process_3, levels = c("Stimme gar nicht zu",
                                                                                       "Stimme nicht zu",
                                                                                       "Stimme eher nicht zu",
                                                                                       "Stimme eher zu",
                                                                                       "Stimme zu",
                                                                                       "Stimme völlig zu"))

raw.short$ict_working_process_4 <- ordered(raw.short$ict_working_process_4, levels = c("Stimme gar nicht zu",
                                                                                       "Stimme nicht zu",
                                                                                       "Stimme eher nicht zu",
                                                                                       "Stimme eher zu",
                                                                                       "Stimme zu",
                                                                                       "Stimme völlig zu"))

raw.short$ict_working_process_5 <- ordered(raw.short$ict_working_process_5, levels = c("Stimme gar nicht zu",
                                                                                       "Stimme nicht zu",
                                                                                       "Stimme eher nicht zu",
                                                                                       "Stimme eher zu",
                                                                                       "Stimme zu",
                                                                                       "Stimme völlig zu"))

raw.short$tui_1 <- ordered(raw.short$tui_1, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_2 <- ordered(raw.short$tui_2, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_3 <- ordered(raw.short$tui_3, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_4 <- ordered(raw.short$tui_4, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_5 <- ordered(raw.short$tui_5, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_6 <- ordered(raw.short$tui_6, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_7 <- ordered(raw.short$tui_7, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_8 <- ordered(raw.short$tui_8, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_9 <- ordered(raw.short$tui_9, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_10 <- ordered(raw.short$tui_10, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_11 <- ordered(raw.short$tui_11, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_12 <- ordered(raw.short$tui_12, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_13 <- ordered(raw.short$tui_13, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_14 <- ordered(raw.short$tui_14, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))

raw.short$tui_15 <- ordered(raw.short$tui_15, levels = c("Stimme gar nicht zu",
                                                       "Stimme nicht zu",
                                                       "Stimme eher nicht zu",
                                                       "Stimme eher zu",
                                                       "Stimme zu",
                                                       "Stimme völlig zu"))


raw.short$ict_adoption <- as.factor(raw.short$ict_adoption)

raw.short$ict_rec_corp <- ordered(raw.short$ict_rec_corp, levels = c("Sehr selten",
                                                         "selten",
                                                         "eher selten",
                                                         "eher häufig",
                                                         "häufig",
                                                         "sehr häufig"))

raw.short$ict_rec_boss <- ordered(raw.short$ict_rec_boss, levels = c("Sehr selten",
                                                                     "selten",
                                                                     "eher selten",
                                                                     "eher häufig",
                                                                     "häufig",
                                                                     "sehr häufig"))

raw.short$ict_rec_cow <- ordered(raw.short$ict_rec_cow, levels = c("Sehr selten",
                                                                     "selten",
                                                                     "eher selten",
                                                                     "eher häufig",
                                                                     "häufig",
                                                                     "sehr häufig"))

raw.short$ict_rec_friends <- ordered(raw.short$ict_rec_friends, levels = c("Sehr selten",
                                                                     "selten",
                                                                     "eher selten",
                                                                     "eher häufig",
                                                                     "häufig",
                                                                     "sehr häufig"))

raw.short$ict_rec_fam <- ordered(raw.short$ict_rec_fam, levels = c("Sehr selten",
                                                                     "selten",
                                                                     "eher selten",
                                                                     "eher häufig",
                                                                     "häufig",
                                                                     "sehr häufig"))

raw.short$ict_rec_acq <- ordered(raw.short$ict_rec_acq, levels = c("Sehr selten",
                                                                     "selten",
                                                                     "eher selten",
                                                                     "eher häufig",
                                                                     "häufig",
                                                                     "sehr häufig"))

raw.short$ict_rec_alt <- ordered(raw.short$ict_rec_alt, levels = c("Sehr selten",
                                                                     "selten",
                                                                     "eher selten",
                                                                     "eher häufig",
                                                                     "häufig",
                                                                     "sehr häufig"))


####

raw.short$ict_trust_corp <- ordered(raw.short$ict_trust_corp, levels = c("Vollstes Misstrauen",
                                                                     "Misstraue ich sehr",
                                                                     "Misstraue ich etwas",
                                                                     "Vertraue ich etwas",
                                                                     "Vertraue ich sehr",
                                                                     "Vollstes Vertrauen"))

raw.short$ict_trust_boss <- ordered(raw.short$ict_trust_boss, levels = c("Vollstes Misstrauen",
                                                                         "Misstraue ich sehr",
                                                                         "Misstraue ich etwas",
                                                                         "Vertraue ich etwas",
                                                                         "Vertraue ich sehr",
                                                                         "Vollstes Vertrauen"))

raw.short$ict_trust_cow <- ordered(raw.short$ict_trust_cow, levels = c("Vollstes Misstrauen",
                                                                       "Misstraue ich sehr",
                                                                       "Misstraue ich etwas",
                                                                       "Vertraue ich etwas",
                                                                       "Vertraue ich sehr",
                                                                       "Vollstes Vertrauen"))

raw.short$ict_trust_friends <- ordered(raw.short$ict_trust_friends, levels = c("Vollstes Misstrauen",
                                                                               "Misstraue ich sehr",
                                                                               "Misstraue ich etwas",
                                                                               "Vertraue ich etwas",
                                                                               "Vertraue ich sehr",
                                                                               "Vollstes Vertrauen"))

raw.short$ict_trust_fam <- ordered(raw.short$ict_trust_fam, levels = c("Vollstes Misstrauen",
                                                                       "Misstraue ich sehr",
                                                                       "Misstraue ich etwas",
                                                                       "Vertraue ich etwas",
                                                                       "Vertraue ich sehr",
                                                                       "Vollstes Vertrauen"))

raw.short$ict_trust_acq <- ordered(raw.short$ict_trust_acq, levels = c("Vollstes Misstrauen",
                                                                       "Misstraue ich sehr",
                                                                       "Misstraue ich etwas",
                                                                       "Vertraue ich etwas",
                                                                       "Vertraue ich sehr",
                                                                       "Vollstes Vertrauen"))

raw.short$ict_trust_alt <- ordered(raw.short$ict_trust_alt, levels = c("Vollstes Misstrauen",
                                                                       "Misstraue ich sehr",
                                                                       "Misstraue ich etwas",
                                                                       "Vertraue ich etwas",
                                                                       "Vertraue ich sehr",
                                                                       "Vollstes Vertrauen"))




####################################################### beyond this line is nowhere land


### Schritt 4: Skalen berechnen

## Jetzt benötigen wir die psych-bibliothek.
library(psych)

## Der scoreItems-Befehl benötigt eine Liste der folgenden Gestalt. Negative Items sind mit Minus gekennzeichnet.
schluesselliste <- list(ATI = c("ati_1", "ati_2", "-ati_3", "ati_4", "ati_5", "-ati_6", "ati_7", "-ati_8", "ati_9"),
                        VBA = c("-vb_allg_1", "vb_allg_2", "-vb_allg_3", "vb_allg_4"),
                        AAZ = c("-aaz_1", "aaz_2", "-aaz_3", "aaz_4", "aaz_5", "aaz_6", "aaz_7", "aaz_8"),
                        PRO = c("pro_1", "pro_2", "pro_3", "pro_4"),
                        PRE = c("pre_1", "pre_2", "pre_3", "pre_4")
)

## Hier werden die Skalen berechnet: 
scores <- scoreItems(schluesselliste, raw.short, missing = TRUE, min = 1, max = 6)


## Die errechneten Scores ATI, VBA usw. werden hinten als Spalten an raw.short angefügt:
data <- bind_cols(raw.short, as_tibble(scores$scores))

## Über den pipe-operator %>% sprechen wir nochmal in Ruhe. 
## Da wir die Konstrukte ja schon berechnet haben, haben wir für die einzelnen Items keine Verwendung mehr. 
## Hierdurch entfernen wir alle Einzelitems. Wörtlich: "Entferne alle Spalten, die mit kleingeschriebenem "ati" beginnen usw. 
data <- data %>% 
  select(-starts_with("ati", ignore.case = F)) %>% 
  select(-starts_with("vb", ignore.case = F)) %>%
  select(-starts_with("aaz", ignore.case = F)) %>%
  select(-starts_with("pre", ignore.case = F)) %>%
  select(-starts_with("pro", ignore.case = F))

## data sieht jetzt genau so aus wie in der Musterlösung und kann abgespeichert werden:
saveRDS(data, "data/data.rds")

## Fertig.

## JRH: Sieht sehr gut aus! Die Skalen hätten sie auch einmal anlegen können, aber das ist eher kosmetisch. 


