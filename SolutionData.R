## Bibliotheken laden: 
library(tidyverse)
library(psych)

## Laden für die read_codebook()-Funktion
source("qualtricshelpers.R")

## Bei den folgenden Datensätzen sind die einzelnen Items alle korrekt zugeordnet.
dataA.classified <- readRDS("data/dataA_prepared.rds")
dataB.classified <- readRDS("data/dataB_prepared.rds")

codebookA <- read_codebook("data/codebookA.csv")
codebookB <- read_codebook("data/codebookB.csv")


## Optional: Daten der anderen Gruppen entfernen:


## Schlüsselliste anlegen:
## Der scoreItems-Befehl benötigt eine Liste der folgenden Gestalt. Negative Items sind mit Minus gekennzeichnet.

schluessellisteA <- list(KUT = c("kut_1", "kut_2", "kut_3", "-kut_4"))

## ACHTUNG! Bei Fragebogen B bedeuten kleine Werte Zustimmung und große Werte Ablehnung! Sie können das umdrehen, indem Sie den positiven (!) Items ein Minus geben.
## Falls Sie es nicht umdrehen bitte aufpassen bei der Interpretation. 
schluessellisteB <- list(KUT = c("-kut_1", "-kut_2", "-kut_3", "kut_4"))

## Skalenberechnung 
scoresA <- psych::scoreItems(schluessellisteA, dataA.classified, missing = TRUE, min = 1, max = 6)
scoresB <- psych::scoreItems(schluessellisteB, dataB.classified, missing = TRUE, min = 1, max = 6)


## Daten Anhängen:
dataA <- bind_cols(dataA.classified, as_tibble(scoresA$scores))
dataB <- bind_cols(dataB.classified, as_tibble(scoresB$scores))

## Daten abspeichern:
saveRDS(dataA, "data/dataA_final.rds")
saveRDS(dataB, "data/dataB_final.rds")
