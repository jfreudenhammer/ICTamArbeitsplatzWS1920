# Analyse Skript 

#### Bibliotheken laden

library(tidyverse)
source("qualtricshelpers.R")

#### Datei laden ----

raw <- read_csv("data/umfrage_testdaten.csv")

## Daten mit unserer Funktion load_qualtrics_csv einlesen: 
datensatz <- "data/umfrage_testdaten.csv"
raw <- load_qualtrics_csv(datensatz)


#### Daten bereinigen ----
### Schritt 1: Unnötige Spalten löschen. 
## Die ausgeschlossenen Fragen sind Systemdaten, Items von anderen Gruppenmitgliedern etc.:
raw.short <- raw[,c(-1:-17, -22:-28, -58:-118, -121:-125)]
## Quizfrage: Warum raw.short und nicht raw <- raw[,c(-1:-17, -22:-28, -58:-118, -121:-125)] ?


### Schritt 2: Variablen umbenennen 
## Variante 1:
# names(raw.short)[5] <- "ati_1"
## usw... Diese Variante ist sehr umständlich.

## Variante 2: Eine eigene Datei mit den Variablennamen erzeugen:
generate_codebook(raw.short, filename, "data/codebook.csv")
## Dann codebook.csv in Excel öffnen, die Vairablennamen in der ersten Spalte per Hand umbenennen, 
## die Datei als codebook_final.csv abspeichern und hier wieder einlesen:
codebook <- read_codebook("data/codebook_final_musterloesung.csv")

## neue Namen auf die Daten anwenden:
names(raw.short) <- codebook$variable

### Schritt 3: Variablen den richtigen Typen zuordnen

## Gender zu kategorialer Variable machen:
raw.short$gender <- as.factor(raw.short$gender)

## Schulabschluss zu ordinaler Variable machen:
raw.short$edu1 <- ordered(raw.short$edu1, levels = c("(noch) kein Schulabschluss",
                                                     "Haupt-/ Volksschulsabschluss",
                                                     "Realschulabschluss",
                                                     "Abitur/Fachabitur"))

raw.short$edu2 <- ordered(raw.short$edu2, levels = c("noch keine Ausbildung",
                                                     "Berufsausbildung",
                                                     "Meister",
                                                     "Hochschulabschluss",
                                                     "Promotion"))


## Nun zu den einzelnen Likert-Items:
## Diese Variante wäre zu umständlich für jedes Item:
raw.short$ati_1 <- ordered(raw.short$ati_1, levels = c("Stimme gar nicht zu", 
                                                       "Stimme nicht zu", 
                                                       "Stimme eher nicht zu", 
                                                       "Stimme eher zu", 
                                                       "Stimme zu", 
                                                       "Stimme völlig zu"))

## Alternativ: Skala für Likertskala einmal anlegen...
scale.zustimmung <-c("Stimme gar nicht zu", 
                     "Stimme nicht zu", 
                     "Stimme eher nicht zu", 
                     "Stimme eher zu", 
                     "Stimme zu", 
                     "Stimme völlig zu")

## ... und für jedes Item verwenden:
raw.short$ati_1 <- ordered(raw.short$ati_1, levels = scale.zustimmung)
raw.short$ati_2 <- ordered(raw.short$ati_2, levels = scale.zustimmung)
raw.short$ati_3 <- ordered(raw.short$ati_3, levels = scale.zustimmung)
raw.short$ati_4 <- ordered(raw.short$ati_4, levels = scale.zustimmung)
raw.short$ati_5 <- ordered(raw.short$ati_5, levels = scale.zustimmung)
raw.short$ati_6 <- ordered(raw.short$ati_6, levels = scale.zustimmung)
raw.short$ati_7 <- ordered(raw.short$ati_7, levels = scale.zustimmung)
raw.short$ati_8 <- ordered(raw.short$ati_8, levels = scale.zustimmung)
raw.short$ati_9 <- ordered(raw.short$ati_9, levels = scale.zustimmung)

raw.short$vb_allg_1 <- ordered(raw.short$vb_allg_1, levels = scale.zustimmung)
raw.short$vb_allg_2 <- ordered(raw.short$vb_allg_2, levels = scale.zustimmung)
raw.short$vb_allg_3 <- ordered(raw.short$vb_allg_3, levels = scale.zustimmung)
raw.short$vb_allg_4 <- ordered(raw.short$vb_allg_4, levels = scale.zustimmung)


## Die AAZ-Items messen ausnahmsweise mit "Trifft sehr zu". Wir brauchen also eine zweite Skala:
scale.zustimmung2 <-c("Trifft gar nicht zu", 
                     "Trifft nicht zu", 
                     "Trifft eher nicht zu", 
                     "Trifft eher zu", 
                     "Trifft zu", 
                     "Trifft völlig zu")

raw.short$aaz_1 <- ordered(raw.short$aaz_1, levels = scale.zustimmung2)
raw.short$aaz_2 <- ordered(raw.short$aaz_2, levels = scale.zustimmung2)
raw.short$aaz_3 <- ordered(raw.short$aaz_3, levels = scale.zustimmung2)
raw.short$aaz_4 <- ordered(raw.short$aaz_4, levels = scale.zustimmung2)
raw.short$aaz_5 <- ordered(raw.short$aaz_5, levels = scale.zustimmung2)
raw.short$aaz_6 <- ordered(raw.short$aaz_6, levels = scale.zustimmung2)
raw.short$aaz_7 <- ordered(raw.short$aaz_7, levels = scale.zustimmung2)
raw.short$aaz_8 <- ordered(raw.short$aaz_8, levels = scale.zustimmung2)

## Promotion / Prevention wieder normal mit "Stimme sehr zu":
raw.short$pro_1 <- ordered(raw.short$pro_1, levels = scale.zustimmung)
raw.short$pro_2 <- ordered(raw.short$pro_2, levels = scale.zustimmung)
raw.short$pro_3 <- ordered(raw.short$pro_3, levels = scale.zustimmung)
raw.short$pro_4 <- ordered(raw.short$pro_4, levels = scale.zustimmung)

raw.short$pre_1 <- ordered(raw.short$pre_1, levels = scale.zustimmung)
raw.short$pre_2 <- ordered(raw.short$pre_2, levels = scale.zustimmung)
raw.short$pre_3 <- ordered(raw.short$pre_3, levels = scale.zustimmung)
raw.short$pre_4 <- ordered(raw.short$pre_4, levels = scale.zustimmung)


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
