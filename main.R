

#--------------Imports des packages--------------------
rm(list=ls())

require(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
require(tseries) #diverses fonctions sur les series temporelles
#install.packages("fUnitRoots")#tests de racine unitaire plus modulables
library(fUnitRoots)



#---------------Mettre en place l'espace de travail--------------
path <- "C:/Users/thoma/Documents/GitHub/serie_temporelle"
setwd(path) #definit l'espace de travail (working directory ou "wd")





#---------------Import des données--------------
datafile <- "donnees.csv" #definit le fichier de donnees
data <- read.csv(datafile,sep=";") #importe un fichier .csv dans un objet de classe data.frame #fichier csv



#---------------Traitement des données--------------
data = data[4:220,1:2] # On enlève la dernière colonne avec les codes et les 4 premières lignes inutlies
colnames(data) <- c("dates","indice")
dates_char <- as.character(data$dates)
dates_char[1] #
tail(dates_char,1) #
dates <- as.yearmon(seq(from=2005, to=2022+8/12, by=1/12)) #
# indice.source <- zoo(as.numeric(data$indice), order.by=dates) #convertit le premier element de data en serie temporelle de type "zoo"

indice <- zoo(as.numeric(data$indice), order.by=dates) #convertit le premier element de data en serie temporelle de type "zoo"

T <- length(indice.source)
#indice <- indice.source[1:(T-4)] #supprime les 4 dernieres valeurs



#---------------Question 1--------------
# On trace la série temporelle pour faire les premières observations qualitative
# On trace la série brute 
plot.ts(indice, xlab="Années", ylab="Indice brut")
# commentaire : la série n'est pas stationnaire
# La série en niveau semble être très persistante et semble avoir une tendance linéaire croissante.

dindice <- diff(indice,1)
plot.ts(dindice, xlab="Années", ylab="lag Indice brut")
# La série en différence première semble ne pas être stationnaire.
# La série est probablement supérieure à I(1).

#---------------Question 2--------------

# Avant de procéder aux tests de racine unitaire, il convient de vérifier s’il y a une constante et/ou une tendance linéaire non nulle. 
# La représentation graphique de spread a montré que la tendance est probablement positive linéaire.
summary(lm(indice ~ dates))
# Le coefficient associé à la tendance linéaire (dates) est bien positif, et peut-être significatif 
# (on ne peut pas vraiment le confirmer car le test n’est pas valide en présence de résidus possiblement autocorrélés). 
# Il faudra donc se mettre dans le cas des tests de racine unitaire avec constante et éventuellement tendance non nulles.

adf <- adfTest(indice, lag=0, type="ct")  #test ADF dans le cas avec constante et tendance.
adf


# Avant d’interpréter le test, vérifions que les résidus du modèle de régression sont bien non autocorrélés, sans quoi le test ne serait pas valide.
# Comme la série est mensuelle, testons l’autocorrélation des résidus jusqu’à l’ordre 24 (deux ans), sans oublier de corriger les degrés de libertés du nombre de régresseurs.
source(file= "./stationnarite.R",local=TRUE)
Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))
#L’absence d’autocorrélation des résidus est rejetée au moins une fois (Q(4) à Q(24)) ??? beaucoupt trop d'ailleurs ???
# Le test ADF avec aucun retard n’est donc pas valide. 
# Ajoutons des retards de ∆Xt jusqu’à ce que les résidus ne soient plus autocorrélés.

