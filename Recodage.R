## Afin de pouvoir analyser nos données et réaliser des tableaux, il faut commencer par 
## recoder des variables pour mieux les exploiter.

# Premièrement, on charge les library. library Tidyverse et questionr permettent de charger la base de donnée
library(dplyr)
library(questionr)
library(tidyverse)

library(ggplot2)
library(FactoMineR)

# Deuxiement on charge la base de donnée
epic <- read.csv("repondant.txt", sep="\t")

## Dans le cadre de ce travail, nous nous souhaitons faire un état des lieux de la natalité en France. 
## Pour ce faire, nous nous intéressons à certaines caractéristiques des individus qui sont intéressantes pour notre sujet. 

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


# Nous allons regrouper les catégorie "appartement" en une seule catégorie.
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

#Essai commit pour vérifier si erreur corrigée



############## Analyse univariée (1 variable) ################

#### Age

epic$AGEM2 <- cut(epic$AGEM, c(20, 30, 40, 50, 60, 70))
table(epic$AGEM2)

freq(epic$AGEM2)

par(mar=c(1,1,1,1))
pie(table(epic$AGEM2), col=rainbow(5),
    main="Nombre de personne suivant les tranches d'âges", cex=0.7)


#### Sexe

par(mar=c(3,3,3,3))
barplot(table(epic$sexe),col = c("blue","red"), ylim = c(0,5000), main = "Effectif d'hommes et de femmes")

prop.table(table(epic$sexe))

#### Diplome 

table(epic$diplome)
p1=prop.table(table(epic$diplome))*100
round(p1,1)

par(mar=c(3,8,2,2))
g=barplot(table(epic$diplome),horiz=T,col="orange",main="Diplôme obtenu", xlab = "Effectif",
        las = 1,cex.names = 0.8,cex.axis = 0.8)
text(0,g,round(p1,1),pos = 4)


table(epic$diplome2)
p2=prop.table(table(epic$diplome2))*100
round(p2,1)

par(mar=c(3,8,2,2))
g=barplot(table(epic$diplome2),horiz=T,col="orange",main="Diplôme obtenu", xlab = "Effectif",
          las = 1,cex.names = 0.8,cex.axis = 0.8, xlim = c(0,4000))
text(0,g,round(p2,1),pos=4)


#### Pcs

table(epic$pcs)
p3=prop.table(table(epic$pcs))*100
round(p3,1)


par(mar=c(5,17,2,3))
g=barplot(table(epic$pcs), main = "Nombre de personnes suivant le Pcs", horiz=T, xlab = "Effectif",
        las = 1 , cex.names=0.8, cex.axis = 0.8, xlim = c(0,2500), col = "yellow3")
text(0,g,round(p3,1),pos=4)

############## Analyse bivariée (2 variables) ################

#### Nombre d'nfant avec la situation relationnelle

table(epic$couple)
table(epic$NBENF)


t=table(epic$couple,epic$NBENF)
addmargins(t)

par(mar=c(3,3,3,3))
barplot(t,
        col=rainbow(3), main = "Nombre d'enfants suivant la situation relationnelle",
        ylim=c(0,2000),beside=T,ylab = "Nombre d'individu", xlab = "nombre d'enfant" )
legend("topright", legend = c("En couple cohabitant","En couple non cohabitant","Pas en couple")
       ,pch=15,cex=0.8,col=rainbow(3),text.col="black")

par(mar=c(3,3,3,3))
barplot(prop.table(t,2)*100,
        col=rainbow(3), main = "Nombre d'enfants suivant la situation relationnelle",
        ylim=c(0,100),beside=T, ylab = "%", xlab = "nombre d'enfant" )
legend("topright", legend = c("En couple cohabitant","En couple non cohabitant","Pas en couple")
       ,pch=15,cex=0.8,col=rainbow(3),text.col="black")


#### Sexe avec le code pcs


t2 = table(epic$pcs, epic$sexe)

par(mar=c(1,1,1,1))
mosaicplot(t2,col=c("blue","red"), las = 2,cex.axis = 1)



####  Sexe avec le diplome obtenu

t3 = table(epic$diplome, epic$sexe)

par(mar=c(1,1,1,1))
mosaicplot(t3,col=c("blue","red"), las = 2,cex.axis = 1.3)


####  situtation relationnelle avec l'âge

t4 = table(epic$couple, epic$AGEM2)

par(mar=c(5,5,5,5))
barplot(t4,beside=F,col=c("#F5BCA9","#F7D358","#D8F781"),las=1,horiz=F
        ,space=0.2, xlim = c(0,7))
legend("topleft", legend = c("Pas en couple","En couple non cohabitant","En couple cohabitant")
       ,pch=15,cex=1.1,col=c("#D8F781","#F7D358","#F5BCA9"),text.col="black", bty = "n")


### Nombre d'années entre le début de relation et la naissance du premier enfant en proportion

epic2 <- filter(epic, H_ENFANT_C==1)
epic3 <- filter(epic2, C_ENFPREC==2)
epic3 <- filter(epic3, H_ADEB_C!=9999)
epic3 <- filter(epic3, IND_AGE_1ER_ENF!=9999)

epic3$arrivee <- epic3$H_AENF1_C - epic3$H_ADEB_C
epic4 <- filter(epic3, arrivee >= 0)
epic4 <- filter(epic4, arrivee <= 9)
tabarrivee = table(epic4$arrivee)
freq(tabarrivee)

par(mar=c(1,1,1,1))
pie(prop.table(tabarrivee)*100,
    xlim = c(0,9),
    col=rainbow('9'))


### Probabilité de faire des enfants à nouveau dans le couple actuelle selon le nombre d'enfants déjà eu avec d'autres

epic5 <- filter(epic, H_ENFANT_C==1)
epic5$nbrenfpass <- epic5$NBENF - epic5$H_NBENF_C
freq(epic5$nbrenfpass)


### Proportion de parents dont l'enfant est né dans une union de rang 2 ou plus, par sexe et génération

# Je garde seulement les individus qui ont eu des enfants durant cette relation et qui n'avaient pas d'enfants avant

rep3 <- filter(epic, H_ENFANT_C==1)
rep31 <- filter(rep3, C_ENFPREC==2)

# Je mets ceux qui ont eu 1 relation importante (celle actuelle incluse) dans une variable, et les autres dans une autre.

rep31$H_NBREL3 <- as.character(rep31$H_NBREL)
rep31$H_NBREL3[rep31$H_NBREL3 %in% c("2","3","4","5","6","7","8","9","10","15")] <- "relation de rang 2 ou plus"
rep31$H_NBREL3[rep31$H_NBREL3 == "1"] <- "relation de rang 1"
table(rep31$H_NBREL3)

#Je réunis ensemble certaines générations

rep31$ANAISR2 <- cut(rep31$ANAISR, c(1948,1950,1959,1969,1988))
table(rep31$ANAISR2,rep31$H_NBREL3)

# Je sépare les filles et les garçons

rep31f <- filter(rep31, SEXER==2)
rep31h <- filter(rep31, SEXER==1)
table(rep31$SEXER)
table(rep31$H_NBREL3,rep31$ANAISR2)
table(rep31f$H_NBREL3,rep31f$ANAISR2)

#Je fais le graphique

t2=table(rep31f$H_NBREL3,rep31f$ANAISR2)
addmargins(t2)

par(mar=c(3,3,3,3))
barplot(1-prop.table(t2,2)*100+100, 
        col=rainbow('4'),
        main="Proportion des femmes dont l'enfant est né 
        dans une union de rang 2 ou plus, par génération")

t3=table(rep31h$H_NBREL3,rep31h$ANAISR2)
addmargins(t3)

par(mar=c(3,3,3,3))
barplot(1-prop.table(t3,2)*100+100, 
        col=rainbow('4'),
        main="Proportion des hommes dont l'enfant est né 
        une union de rang 2 ou plus, par génération")

### Nombre d'années entre le début de relation et la naissance du premier en fant, par âge au début de la relation et par nombre de relations importantes

# On garde seulement les individus qui ont eu des enfants durant cette relation et qui n'avaient pas d'enfants avant

rep3 <- filter(rep2, H_ENFANT_C==1)
rep31 <- filter(rep3, C_ENFPREC==2)
rep31 <- filter(rep31, H_AENF1_C!=9999)
rep31 <- filter(rep31, H_ADEB_C!=9999)
rep31 <- filter(rep31, IND_AGE_1ER_ENF!=9999)

# On stock la moyenne de la différence entre l'année de début de la relation et l'année de naissance de l'enfant par age dans une variable indépendante
moyage <- numeric(1)
for(i in 1:45){
  temp1 <- filter(rep31, IND_AGE_1ER_ENF==i)
  moyage[i] <- mean(temp1$H_AENF1_C - temp1$H_ADEB_C)
}
plot(moyage,  xlim = c(18, 45))
lines(moyage)

# On stock la moyenne de la différence entre l'année de début de la relation et l'année de naissance de l'enfant par relation importante dans une variable indépendante

moyrel <- numeric(1)
for(i in 1:5){
  temp2 <- filter(rep31, H_NBREL==i)
  moyrel[i] <- mean(temp2$H_AENF1_C - temp2$H_ADEB_C)
}

# On trace le graphique
par(mar=c(5,5,5,5))
plot(moyrel,  xlim = c(1,5))
lines(moyrel)


### Proportion de parents dont l'enfant est né dans une union de rang 2 ou plus, par sexe et génération

# Je garde seulement les individus qui ont eu des enfants durant cette relation et qui n'avaient pas d'enfants avant
rep3 <- filter(rep2, H_ENFANT_C==1)
rep31 <- filter(rep3, C_ENFPREC==2)

# Je mets ceux qui ont eu 1 relation importante (celle actuelle incluse) dans une variable, et les autres dans une autre.
rep31$H_NBREL3 <- as.character(rep31$H_NBREL)
rep31$H_NBREL3[rep31$H_NBREL3 %in% c("2","3","4","5","6","7","8","9","10","15")] <- "relation de rang 2 ou plus"
rep31$H_NBREL3[rep31$H_NBREL3 == "1"] <- "relation de rang 1"
table(rep31$H_NBREL3)

#Je réunis ensemble certaines générations
rep31$ANAISR2 <- cut(rep31$ANAISR, c(1948,1950,1959,1969,1988))
table(rep31$ANAISR2,rep31$H_NBREL3)

# Je sépare les filles et les garçons
rep31f <- filter(rep31, SEXER==2)
rep31h <- filter(rep31, SEXER==1)
table(rep31$SEXER)
table(rep31$H_NBREL3,rep31$ANAISR2)
table(rep31f$H_NBREL3,rep31f$ANAISR2)

#Je fais le graphique
t2=table(rep31f$H_NBREL3,rep31f$ANAISR2)
addmargins(t2)

par(mar=c(3,3,3,3))
barplot(1-prop.table(t2,2)*100+100, 
        col=rainbow('4'),
        main="Proportion des femmes dont l'enfant est né dans une union de rang 2 ou plus, par génération")

t3=table(rep31h$H_NBREL3,rep31h$ANAISR2)
addmargins(t3)

par(mar=c(3,3,3,3))
barplot(1-prop.table(t3,2)*100+100, 
        col=rainbow('4'),
        main="Proportion des hommes dont l'enfant est né dans une union de rang 2 ou plus, par génération")
epic <- read.csv("~/Sociologie/M2/Stats info/repondant.txt", sep="\t")

### Nombre d'enfants avec le nombre de relations importantes

rep2 <- epic

rep2$NBENF2 <- as.character(rep2$NBENF)
rep2$NBENF2[rep2$NBENF %in% c("4","5","6","7","8","9","10")] <- "5 enfants et +"
table(rep2$NBENF2)

rep2$H_NBREL2 <- as.character(rep2$H_NBREL)
rep2$H_NBREL2[rep2$H_NBREL %in% c("6","7","8","9","10","15")] <- "6 relations et +"
table(rep2$H_NBREL2)

t=table(rep2$NBENF2,rep2$H_NBREL2)
addmargins(t)

par(mar=c(3,3,3,3))
barplot(prop.table(t,2)*100, 
        col=rainbow(6),
        main="Proportion de relations importantes selon le nombre d'enfants",)

legend("topright",
       legend = c("0", "1", "2", "3", "4", ">4"),
       pch=15,
       col=rainbow(6))
