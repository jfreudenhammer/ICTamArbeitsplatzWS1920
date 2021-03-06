# Analyse Skript 

#### Bibliotheken laden

library(tidyverse)
library(psych)
library(questionr)

source("qualtricshelpers.R")

#### Datei laden ----

datensatz <- "data/umfragedaten.xlsx"
raw <- load_qualtrics_csv(datensatz)

#### Daten bereinigen ----
names(raw)
raw.short <- raw[,c(-1:-17, -24:-35, -80:-124)]
names(raw.short)

### Codebook erstellen

generate_codebook(raw.short, datensatz, "data/codebook.csv")
codebook <- read_codebook("data/codebook_final.csv")

## neue Namen auf die Daten anwenden:
names(raw.short) <- codebook$variable

### Variablen den richtigen Typen zuordnen

raw.short$gender <- as.factor(raw.short$gender)

raw.short$edu <- ordered(raw.short$edu, levels = c("kein Schulabschluss",
                                                    "Hauptschulabschluss",
                                                    "Realschule (Mittlere Reife)",
                                                    "Fachabitur",
                                                    "Allg. Hochschulreife (Abitur)",
                                                    "Abgeschlossene Ausbildung",
                                                    "Bachelor",
                                                    "Master",
                                                    "Diplom",
                                                    "Promotion"))

raw.short$job_type <- as.factor(raw.short$job_type)

raw.short$job_field <- as.factor(raw.short$job_field)

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

### Schritt 4: Skalen berechnen

schluesselliste <- list(IKT = c("ict_working_process_1","ict_working_process_2","ict_working_process_3","ict_working_process_4","ict_working_process_5"),
                        TUI = c("-tui_1","-tui_2","tui_3","tui_4","tui_5","tui_6","tui_7","tui_8","-tui_9","-tui_10","-tui_11","-tui_12","tui_13","tui_14","tui_15"))
      

## Skalen berechnen: 
scores <- scoreItems(schluesselliste, raw.short, missing = TRUE, min = 0, max = 5)


## Die errechneten Scores werden hinten als Spalten an raw.short angefügt:
data <- bind_cols(raw.short, as_tibble(scores$scores))

data <- data %>% 
select(-starts_with("ict_working_process", ignore.case = F)) %>% 
select(-starts_with("tui", ignore.case = F))

saveRDS(data, "data/data.rds")

df <- readRDS("data/data.rds")




