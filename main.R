

#--------------Imports des packages--------------------
rm(list=ls())

require(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
require(tseries) #diverses fonctions sur les series temporelles




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
indice.source <- zoo(as.numeric(data$indice), order.by=dates) #convertit le premier element de data en serie temporelle de type "zoo"


T <- length(indice.source)
indice <- indice.source[1:(T-4)] #supprime les 4 dernieres valeurs



#---------------Question 1--------------
# On trace la série temporelle pour faire les premières observations qualitative
# On trace la série brute 
plot.ts(indice, xlab="Années", ylab="Indice brut")
# commentaire : la série n'est pas stationnaire


#---------------Question 2--------------

