## Afin de pouvoir analyser nos données et réaliser des tableaux, il faut commencer par réaliser
## des recodages des variables pour mieux les exploiter.

# Premièrement, on charge les library. library Tidyverse et questionr permettent de charger la base de donnée
library(dplyr)
library(questionr)
library(tidyverse)

library(ggplot2)
library(FactoMineR)

# Deuxiement on charge la base de donée
epic <- read.csv("repondant.txt", sep="\t")

## Dans le cadre de ce travail, nous nous souhaitons faire un état des lieux de la natalité en France. 
## Pour ce faire, nous allons nous intéressons à différentes caractéristiques des personnes. 

### RECODAGE 1
#Premièrement, nous allons recoder la variable sur les catégories sociaux profesionnelles.
mode (epic$M_CS8)
epic$pcs <- as.character (epic$M_CS8)
epic$pcs <- fct_recode (epic$pcs,
                        "Agriculteurs exploitants" = "1",
                        "Artisans, commercants et chefs d'entreprise" = "2",
                        "Cadres et professions intellectuelles superieures" ="3",
                        "Professions Intermediaires" ="4",
                        "Employes" = "5",
                        "Ouvriers" = "6",
                        "Sans activite professionnelle" = "8",
                        "NSP" = "99")
table(epic$pcs)

## Nous allons enlever la réponse "NSP", car elle ne nous ait pas utile dans ce travail. 
epic$pcs <- fct_recode(epic$pcs, "NULL" = "NSP")
table(epic$pcs)

### RECODAGE 2
# A présent, nous allons recoder la variable niveau de diplôme. 
mode(epic$M_DIPLOME)
epic$diplome <- as.character(epic$M_DIPLOME)
epic$diplome <- fct_recode(epic$diplome,
                           "Aucun diplome" = "1",
                           "CEP" = "2",
                           "Brevet" = "3",
                           "CAP BEP" = "4",
                           "Baccalaureat" = "5",
                           "BTS" = "6",
                           "BAC +3 BAC +4" = "7",
                           "Master 2" = "8",
                           "Doctorat de recherche" = "9",
                           "NSP" = "99")
table(epic$diplome)

## On enlève à nouveau la réponse "NSP", car elle ne nous intéresse pas
epic$diplome <- fct_recode(epic$diplome, "NULL" = "NSP")
table(epic$diplome)

## Cependant, pour rendre la lecture plus agréable lors de la mise en tableau, nous allons établir des niveaux de diplôme plus large
epic$diplome2 <- fct_collapse(epic$diplome,
                             "Inferieur au Bac" = c("Aucun diplome" , "CEP" , "Brevet" , "CAP BEP"),
                             "Bac" = "Baccalaureat",
                             "Bac +2" = "BTS",
                             "Bac +3 ou +4" = "BAC +3 BAC +4",
                             "Bac +5 et plus" = c("Master 2" , "Doctorat de recherche"))
table(epic$diplome2)

### RECODAGE 3
# Recodons la variable couple afin de voir si la personne est en couple cohabitant ou non cohabitant, ou si elle n'est pas en couple.
mode(epic$M_COUPLE)
epic$couple <- as.character (epic$M_COUPLE)
epic$couple <- fct_recode(epic$couple,
                          "En couple cohabitant" ="1",
                          "En couple non cohabitant" = "2",
                          "Pas en couple" = "3")
table(epic$couple)

### RECODAGE 4 
# Nous nous intéressons au type de logement occupé par la personne.
mode(epic$TYPLOG)
epic$logement <- as.character(epic$TYPLOG)
epic$logement <- fct_recode(epic$logement,
                            "Maison indépendante" = "1",
                            "Maison de ville" = "2",
                            "Appartement immeuble deux logements" = "3",
                            "Appartement immeuble trois logements et +" = "4",
                            "Appartement immeuble dix logements" = "5",
                            "Habitation précaire" = "6",
                            "Autre type de logement" = "7")
table(epic$logement)


# Nous allons regrouper les catégorie appartement en une seule catégorie.
epic$logement2 <- fct_collapse(epic$logement,
                               "Maison indépendante" = "Maison indépendante",
                               "Maison de ville" = "Maison de ville",
                               "Appartement" = c("Appartement immeuble deux logements" , "Appartement immeuble trois logements et +" , "Appartement immeuble dix logements"),
                               "Habitation précaire" = "Habitation précaire",
                               " Autre type de logement" = "Autre type de logement")
table(epic$logement2)

### RECODAGE 5
# Recodage de la variable sexe
mode(epic$SEXER)
epic$sexe <- as.character(epic$SEXER)
epic$sexe <- fct_recode(epic$sexe,
                        "homme" = "1",
                        "femme" = "2")
table(epic$sexe)



