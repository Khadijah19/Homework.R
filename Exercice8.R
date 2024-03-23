#====================================================================================#
#     ENSAE Pierre NDIAYE de Dakar ISEP2 2023-2024                                   #
#     COURS DE Traitement statistiques avec le logiciel R        avec M. DIALLO      #
#                                                                                    #
#====================================================================================#

#============== Exercice Numero 8 : Sur la base de donnees cereales =================#


#Calculons la quantité achetée en Kg
##-Preparation de la base
library(haven)
cereales <- read_dta(paste0("C:\\Users\\HP\\OneDrive\\1231116193333-Desktop\\Desktop\\ISEPcoursR2024\\Exos_Céréales\\cereales.dta"))

library("tidyverse")
glimpse(cereales)

#--Renommer variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

##Gestion des NA

##Suppression des ménages ne consommant pas de céréales
attach(cereales)
anyNA(Qtty_cons)

##Création d'une variable temporaire
cereales$t <- ifelse(is.na(Qtty_cons)==1,1,0)
table(cereales$t)
cereales_na <- cereales[cereales$t==1,] ##Sous la base cereales
View(cereales_na)

##Suppression des ménages n'ayant pas déclaré les qtités cons

cereales <- cereales[cereales$t==0,]
dim(cereales)
View(cereales)

#Calculons la quantité achetée en kg
basefusionnee$poids <- as.numeric(basefusionnee$poids)
basefusionnee$Qtty_achat <- as.numeric(basefusionnee$Qtty_achat)
basefusionnee$Quantite_achetee_en_kg <- (basefusionnee$Qtty_achat*basefusionnee$poids)/1000

#Calculons le prix unitaire
basefusionnee$prix_unitaire= basefusionnee$Value_achat/basefusionnee$Qtty_achat

#Calculons les dépenses de consommation
depenses_de_consommation <- basefusionnee$prix_unitaire*basefusionne$Qtty_cons
View(basefusionnee)

#Detection des valeurs aberrantes
##Traçage d'un boxplot
boxplot(basefusionnee$Qtty_cons, 
        main = "Boxplot des quantités consommées", # Titre du graphique
        ylab = "Quantité consommée", # Nom de l'axe des ordonnées
        col = "pink", 
        border = "blue", 
        horizontal=TRUE)
#Calcul de l'intervalle interquartile
summary(basefusionnee$Qtty_cons)
IQR<- 14-3
if (basefusionnee$Qtty_cons) < I