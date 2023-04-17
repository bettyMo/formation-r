# On vide l'environnement
rm(list=ls())

# On reimporte
apparitions <- read.csv("Donnees/in/apparitions.csv")
episodes <- read.csv("Donnees/in/episodes.csv")
perso <- read.csv("Donnees/in/personnages.csv")
scenes <- read.csv("Donnees/in/scenes.csv")

# On creer le tableau final
perso_fr<-perso

# On enlève image
perso_fr<-subset(perso_fr,select=-image)

# On renome les colonnes
colnames(perso_fr)<-c("nom","sexe","maison","Tue_par")