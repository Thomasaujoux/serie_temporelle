# Projet : séries temporelles

#--------------Imports des packages--------------------
rm(list=ls())

#install.packages("fUnitRoots") #tests de racine unitaire plus modulables

library(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
library(tseries) #diverses fonctions sur les series temporelles
library(fUnitRoots)
library("polynom")
library("ggplot2")
library(forecast)
library(car)


#---------------Mise en place l'espace de travail--------------
path <- "C:/Users/thoma/Documents/GitHub/serie_temporelle"
setwd(path) #definit l'espace de travail (working directory ou "wd")



#---------------Import des données--------------
datafile <- "donnees.csv" #definit le fichier de donnees
data <- read.csv(datafile,sep=";") #importe un fichier .csv dans un objet de classe data.frame #fichier csv



#---------------Traitement des données--------------
data_debut = data[4:218,1:2] # On enlève la dernière colonne avec les codes et les 4 premières lignes inutiles
last_points = data[219:220,1:2] # On enlève les deux derniers points pour la partie III
colnames(data_debut) <- c("dates","indice")
colnames(last_points) <- c("dates","indice")

dates_char <- as.character(datadebut$dates) # Transformation des dates pour les rendre utilisables
dates_char[1] 
tail(dates_char,1) 
dates_char <- as.character(last_points$dates) 

dates_debut <- as.yearmon(seq(from=2005, to=2022+11/12, by=1/12))
dates_last <- as.yearmon(seq(from=2023, to=2023+1/12, by=1/12))
indice <- zoo(as.numeric(data_debut$indice), order.by=dates_debut) #convertit le premier element de data en serie temporelle de type "zoo"
last_points <- zoo(as.numeric(last_points$indice), order.by=dates_last) #convertit le premier element de data en serie temporelle de type "zoo"

T <- length(indice)



#---------------PARTIE 1--------------
#---------------Question 1--------------
# On trace la série temporelle pour faire les premières observations qualitatives.
plot.ts(indice, xlab="Années", ylab="Indice brut")
# commentaire : la série n'est pas stationnaire
# La série en niveau semble être très persistante et semble avoir une tendance linéaire croissante. A confirmer par la suite.

dindice <- diff(indice,1)
plot.ts(dindice, xlab="Années", ylab="lag Indice brut")
# La série en différence première semble être stationnaire.
# La série est probablement I(1) (à vérifier, c'est seulement une première analyse en regardant uniquement la courbe).




#---------------Question 2--------------
# Etape 1 : Analyse qualitative de la non stationnarité de la série différenciée.
acf(dindice)  #trace les fonctions d’autocorrélation totale. 
dev.print(device = png, file = "./Images_pour_rapport/acf_dindice.png", width = 600)
pacf(dindice)  #trace les fonctions d’autocorrélation partielle.
dev.print(device = png, file = "./Images_pour_rapport/pacf_dindice.png", width = 600)
# L’autocorrélation d’ordre 1 (totale ou partielle, c’est la même chose) est d’environ -0.45, soit petite et loin d’être égales à 1. 
# La série semble donc stationnaire. Vérification du travail précédent.




# Etape 2 : Choix pour le test ADF
# Avant de procéder aux tests de racine unitaire, il convient de vérifier s’il y a une constante et/ou une tendance linéaire non nulle. 
# La représentation graphique de spread a montré que la tendance est probablement positive linéaire.
summary(lm(indice ~ dates))
# Le coefficient associé à la tendance linéaire (dates) est bien positif, et peut-être significatif 
# (on ne peut pas vraiment le confirmer car le test n’est pas valide en présence de résidus possiblement autocorrélés). 
# Il faudra donc se mettre dans le cas des tests de racine unitaire avec constante et éventuellement tendance non nulles.




# Etape 3 : Choix pour le nombre de lags
# Vérifions que les résidus du modèle de régression sont bien non autocorrélés, sans quoi le test ADF ne serait pas valide.
# Comme la série est mensuelle, testons l’autocorrélation des résidus jusqu’à l’ordre 24 (deux ans), sans oublier de corriger les degrés de libertés du nombre de régresseurs.
source(file= "./stationnarite.R",local=TRUE)
adf <- adfTest(indice, lag=0, type="ct")  #test ADF dans le cas avec constante et tendance.
Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))
#L’absence d’autocorrélation des résidus est rejetée au moins une fois.
# Le test ADF avec aucun retard n’est donc pas valide. 
# Ajoutons des retards de ∆Xt jusqu’à ce que les résidus ne soient plus autocorrélés.
series <- indice
kmax <- 24
adftype="ct"
adf <- adfTest_valid(series,kmax,adftype=adftype)
# Il a fallu considérer 11 retards au test ADF pour supprimer l’autocorrélation des résidus.




# Etape 4 : Résultats du test ADF
adf #affichage des r´esultats du test valide maintenu
print(paste0("La pvaleur du test ADF est : ", round(adf@test$p.value,digits = 2)))
# La racine unitaire n’est pas rejetée à un seuil de 95% pour la série en niveau, la série est donc au moins I(1).




# Etape 5 : On répéte le même raisonnement pour la série différentiée.
# Testons maintenant la racine unitaire pour la série différenciée dindice. 
# La représentation graphique précédente semble montrer l’absence de constante et de tendance non nulle. Vérifions avec une régression :
summary(lm(dindice ∼ dates[-1])) #sans la premi`ere date car on a diff´erenci´e la s´erie
# Il y a bien ni constante ni tendance significative
# Effectuons donc le test ADF dans le cas sans constante ni tendance, en vérifiant l’absence autocorrélation des résidus.
adf <- adfTest_valid(dindice,24,"nc")
# Il est nécessaire d’inclure des retards dans le test ADF.
# Le test ADF avec aucun retard n’est donc pas valide. 
# Ajoutons des retards de ∆Xt jusqu’à ce que les résidus ne soient plus autocorrélés.
adf
print(paste0("La pvaleur du test ADF est : ", round(adf@test$p.value,digits = 2)))
# Le test rejette la racine unitaire (p-value<0.05), on dira donc que la série différenciée est ”stationnaire”. Indice est donc I(1).




#---------------Question 3--------------
p = ggplot(data=indice) + geom_line(aes(x=dates,y=indice))
p
ggsave("Serie_brute.png",path="./Images_pour_rapport",width = 10, height = 5)


p_diff = ggplot(data=dindice) + geom_line(aes(x=dates[-1],y=dindice))
p_diff
ggsave("Serie_differenciee.png",path="./Images_pour_rapport",width = 10, height = 5)




#---------------PARTIE 2--------------
#---------------Question 4--------------
# Etape 1 : Détermination des p et q miximaux
acf(dindice)  #trace les fonctions d’autocorrélation totale. q* = 3
pacf(dindice)  #trace les fonctions d’autocorrélation partielle. p* = 6
axis(side=1,at=seq(0,25))
# Comme la série est stationnaire, elle est intégrée d’ordre d = 0.


# Etape 2 : validation des différents paramètres
source(file= "./validation_parametres.R",local=TRUE)
# On estime tous les ARMA possibles et on les stocke dans la variable armamodels
armamodels <- armamodelchoice(6,3) 
# On garde les modèles bien ajustés et valides.
selec <- armamodels[armamodels[,"ok"]==1&!is.na(armamodels[,"ok"]),] 
print("Les modèles valides et ajustés sont")
print(selec)



#on a 3 modèles valides et ajustés : ARMA (4,1), ARMA(5,2) et ARMA(1, 3)
arima401 <- arima(dindice,c(4,0,1))
arima502 <- arima(dindice, c(5,0,2))
arima103 <- arima(dindice, c(1,0,3))



# Etape 3 : Calcul des AIC et BIC pour les modèles sélectionnés
models <- c("arima401","arima502","arima103"); names(models) <- models
apply(as.matrix(models),1, function(m) c("AIC"=AIC(get(m)), "BIC"=BIC(get(m))))
#ARMA(4,1) minimise l'AIC
#ARMA(1,3) minimise le BIC 


# Etape 4 : On choisit le modèle parmis les 2 qui minimise le R2 ajusté
source(file= "./selection_modele.R",local=TRUE)
adj_r2(arima401)
adj_r2(arima103)
# On sélectionne le ARIMA(4,0,1) qui a le plus grand R2 ajusté




#---------------Question 5--------------
# Il s'agit de montrer que le modèle ARMA(4,0,1) qu'on a pour la série différenciée est bien causal.
# Or un ARMA est causal ssi pas de racine dans le disque unité du polynôme phi
source(file= "./test_causalité.R",local=TRUE)
arma_causal(arima401) # renvoie TRUE

png("./Images_pour_rapport/root401.png")
Arima(dindice, order = c(4, 0, 1), xreg = seq_along(dindice)) %>%
  autoplot()

dev.off()

# Le modèle ARMA pour la série différenciée est causal.
# Donc, par définition d'un ARIMA, la série non transformée suit un modèle ARIMA(4,1,1).




#---------------PARTIE 3--------------
#---------------Question 6--------------
T # La longueur de la série
# On suppose pour la suite que les résidus de la série sont gaussiens.

# Voir rapport pour l'équation

#---------------Question 7--------------

# Voir rapport pour les hypothèses 

#---------------Question 8--------------
# Représentation graphique de la région pour alpha = 95%
png("./Images_pour_rapport/ellispe2.png")
model = arima401
a <- predict(model, 2)
df <- data.frame(X_T1 = (-1000:700) / 100, X_T2 = (-1000:700) / 100)

phi <- as.numeric(model$coef[4])
psi <- as.numeric(model$coef[1])
sigma_bruit = mean(model$residuals**2)-mean(model$residuals)**2 #variance du bruit blanc
sigma <- cbind(c(1 + phi + psi, psi + phi), c(psi + phi, 1)) * sigma_bruit

plot(X_T2 ~ X_T1, data = df, type = "n")
ellipse(center = c(a$pred[2], a$pred[1]), shape = sigma, radius = qchisq(0.05, 2) * model_2$sigma2, draw = TRUE, add = TRUE, lty = 2, fill = TRUE, fill.alpha = 0.1) # nolint

points(a$pred[2], a$pred[1], pch = "+", col = "green")
points(last_points[2], last_points[1], pch = "+", col = "red")

dev.off()