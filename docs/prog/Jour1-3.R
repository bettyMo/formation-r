
## ----importation --------------------------


## Creer perso_fr


# Mon code illisible et non reproductible à ne pas faire!
perso_fr<-perso[,-5]
colnames(perso_fr)<-c("nom","sexe","maison","Tue_par")


# Recoder des variables qualitatives
#- Creer une variable à partir d'une autre :
  1. Les variables indicatrices 
  2. Une variable par modalité
  3. Changer des intitulés de modalités
  4. Regrouper des modalités
  
#- Crée une variable à partir de plusieurs
## -------------------------------------------------------------


## ----------------------------------------------------------------
## simplifier en 
library(dplyr)
library(tidyr)
## simplifier en :
library(tidyverse)
  
 

#- Creer une variable à partir d'une autre :
#1. Les variables indicatrices 
## -------------------------------------------------------------

perso_fr<-mutate(perso_fr,Tue=ifelse(is.na(Tue_par),"vivant","Mort"))



## Changer des intitulés de modalités
## La cas particulier des NA qui sont non concernées
## ----------------------------------------------------------------
perso_fr<-mutate(perso_fr,maison_recod = replace_na(maison, "NC"))
# verification
table(perso_fr$maison_recod,perso_fr$maison_recod2)

## Une quali à partir d'une quanti
## ----------------------------------------------------------------
## code pas terrible conduisant à un variable quanti
perso_fr$Mort<-as.numeric(!is.na(perso_fr$Tue_par))


## ----------------------------------------------------------------
library(forcats)
perso_fr<-mutate(perso_fr,Mort=as.factor(Mort))

perso_fr<-mutate(perso_fr,Mort=fct_recode(Mort,
           "Vivant" = "0",
           "Mort" = "1"))

# vérification
perso_fr%>%
  select(Tue,Mort)%>%
  table(useNA="ifany")
## ----------------------------------------------------------------
## Regroupement de modalité

## ----------------------------------------------------------------
perso_fr<-perso_fr%>%
  mutate(maison_recod2=fct_recode(maison_recod,
     "Arryn et Tarly" = "Arryn",
           "Arryn et Tarly" = "Tarly"
     )
  )

## On verifie
library(questionr)
 freq(perso_fr$maison_recod2)

## ---------------------------------------------------------------- Recodage à partir de plusieurs variables


## ----------------------------------------------------------------
perso_fr<-perso_fr %>%
  mutate(maison_recod_f=as.character(maison_recod)) %>%
  mutate(maison_recod_f= ifelse(maison_recod=='NC' & nom == "Baratheon General", "Baratheon", maison_recod_f))
  

 
