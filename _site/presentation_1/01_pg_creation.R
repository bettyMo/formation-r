
# Importation des données

chemin<-"C:/Users/elisa/Documents/Dossier_tout_compris/Ined/Formation/R intermediaire/Poly/donnees/in"
apparitions <- read.csv(paste(chemin,"apparitions.csv",sep="/"))
episodes <- read.csv(paste(chemin,"episodes.csv",sep="/"))
perso <- read.csv(paste(chemin,"personnages.csv",sep="/"))

scenes <- read.csv(paste(chemin,"scenes.csv",sep="/"))

# Traduction

library(dplyr)
perso_fr<-rename(perso,nom=name,
                 sexe=sex,
                 maison=house,
                 Tue_par=killedBy)




# On enlève la colonne image
perso_fr<-select(perso_fr,-image)

# Remarque : j'ai quelque doublon , vue ici par le package janitor
# library(janitor)
# perso_fr %>%
#   get_dupes(nom) 

# On creer une variable 
# Famille : oui/non
# Et mort : oui/non

perso_fr<-mutate(perso_fr,famille=ifelse(is.na(maison),"non","oui"))
perso_fr<-mutate(perso_fr,mort=ifelse(!is.na(Tue_par),"oui","non"))


# On traduit le sexe
## Recodage de perso_fr$sexe 
library(forcats)
perso_fr <- mutate(perso_fr,
                   sexe=fct_recode(perso_fr$sexe,
                                   "femme" = "female",
                                   "homme" = "male"
                   )
)

# On ajoute si le personnage est un tueur
perso_fr<-perso_fr %>% 
  count(Tue_par, sort = TRUE) %>%
  filter(!is.na(Tue_par))%>%
  right_join(perso_fr,by=c('Tue_par'='nom'))%>%
  rename(nom=Tue_par,
         personnes_tue_n=n)

# Tueur

perso_fr<-mutate(perso_fr,tueur=ifelse(!is.na(personnes_tue_n),"oui","non"))

# On recode le nombre de tue en 
# non_assassin
# tueur(une fois)
# assassin(deux et 3 fois)
# serial_killer (plus de 3 fois)

perso_fr<-perso_fr%>%
  mutate(type_tueur= cut(personnes_tue_n,
                         include.lowest = TRUE,
                         right = FALSE,
                         dig.lab = 4,
                         breaks = c(0, 1, 2, 4, 14))) %>%
  mutate(type_tueur=fct_recode(type_tueur,
                               "non" = "[0,1)",
                               "tueur" = "[1,2)",
                               "assassin" = "[2,4)",
                               "serial_killer" = "[4,14]")
  ) %>%
  mutate(type_tueur=fct_explicit_na(type_tueur, "non"))


#### On ajoute le nombre de scenes par acteur
apparitions2<-apparitions%>%
  group_by(name)%>%
  summarise(nb_scene = n())

perso_fr<-inner_join(perso_fr,apparitions2,by=c("nom"="name"))


##Rajout de saisons et numeros saisons



duree<-left_join(scenes,episodes,by="episodeId")%>%
  inner_join(apparitions,by="sceneId")


dur1 <- duree %>%
  arrange(name,sceneId) %>%
  group_by(name) %>%
  mutate(temps_presence=sum(duration))


# on garde la premiere appartion
app_prem<-dur1 %>%
  arrange(name,sceneId) %>%
  distinct(name, .keep_all = TRUE)%>%
  select(name,sceneId,seasonNum,duration,episodeNum)%>%
  rename (prem_scene=sceneId,prem_saison=seasonNum,prem_duree=duration,prem_episode=episodeNum)

# on garde la derniere appartion
app_dern<-dur1 %>%
  arrange(name,desc(sceneId)) %>%
  distinct(name, .keep_all = TRUE)%>%
  select(name,sceneId,seasonNum,duration,episodeNum,temps_presence)%>%
  rename (dern_scene=sceneId,dern_saison=seasonNum,dern_duree=duration,dern_episode=episodeNum)


# Les apparitions des personnages
appf<-inner_join(app_prem,app_dern,by="name")

# table  final!
perso_fr<-inner_join(perso_fr,appf,by=c("nom"="name"))
perso_fr<-as.data.frame(perso_fr)
#587 obs et 17 va

## Le package janitor pour clean les noms et verifier les duplicates

#install.packages("devtools")
#devtools::install_github("sfirke/janitor")
library(janitor)

perso_fr <- perso_fr %>%
  clean_names()



library(tidyr)
# Creation des données de séquence
loc_got <- scenes %>% 
  group_by(episodeId) %>%
  mutate(rang=row_number()) %>%
  select(episodeId,location,rang) %>% 
  pivot_wider(names_from = rang, values_from = location,
              names_prefix="scene")

loc_got<-(as.data.frame(loc_got))

#write.csv(loc_got,'Donnees/out/loc_got.csv',row.names=FALSE)

# Là c'est pour normaliser les noms de colonnes en plusieurs mots sans maj , séparé par des underscores
loc_got <- loc_got %>%
  clean_names()

# On creee une deuxieme sequence 
# le nombre de personnages?
# le nombre de mort?
# la durée? la durée en 5 classes c'est facile donc on fait çasceneStart
# On merge scenes et episode pour avoir la duree total de l'episode
tot<-episodes%>%
  select(episodeId,total_duration)%>%
  merge(scenes,by="episodeId")%>%
  mutate(prop_tps=duration/total_duration)%>%
  mutate(prop_tps_rec=cut(prop_tps,
                          include.lowest = TRUE,
                          right = TRUE,
                          dig.lab = 4,
                          breaks = c(0, 0.01, 0.02, 0.04, 0.17)
  ))


tot$prop_tps_rec<-  fct_recode(as.factor(tot$prop_tps_rec),
                               "très_court" = "[0,0.01]",
                               "court" = "(0.01,0.02]",
                               "long" = "(0.02,0.04]",
                               "très_long" = "(0.04,0.17]"
)

sum(summary(tot$prop_tps_rec))

tps_scene_got <- tot %>% 
  group_by(episodeId) %>%
  mutate(rang=row_number()) %>%
  select(episodeId,prop_tps_rec,rang) %>% 
  pivot_wider(names_from = rang, values_from = prop_tps_rec,
              names_prefix="scene")

# write.csv(tps_scene_got,'Donnees/out/tps_got.csv',row.names=FALSE)



# au vue de l'analyse préliminaire , on opte pour seulement 6 etats de localisation soit :
## Recodage de tot$location en tot$location_rec
tot$location_rec <- fct_recode(tot$location,
                               "Autre" = "Astapor",
                               "Autre" = "Braavos",
                               "Autre" = "Dorne",
                               "Autre" = "Pentos",
                               "Autre" = "Qarth",
                               "Autre" = "The Dothraki Sea",
                               "Autre" = "The Iron Islands",
                               "Autre" = "The Narrow Sea",
                               "Autre" = "The Reach",
                               "Autre" = "The Red Waste",
                               "Autre" = "The Shivering Sea",
                               "Autre" = "The Stormlands",
                               "Autre" = "The Summer Sea",
                               "Autre" = "The Sunset Sea",
                               "Autre" = "The Vale",
                               "Autre" = "The Westerlands",
                               "Autre" = "Vaes Dothrak",
                               "Autre" = "Valyria",
                               "Autre" = "Volantis",
                               "Autre" = "Yunkai"
)


loc_got2 <- tot %>% 
  group_by(episodeId) %>%
  mutate(rang=row_number()) %>%
  select(episodeId,location_rec,rang) %>% 
  pivot_wider(names_from = rang, values_from = location_rec,
              names_prefix="scene")

loc_got2<-(as.data.frame(loc_got2))

# write.csv(loc_got2,'Donnees/out/loc_got2.csv',row.names=FALSE)