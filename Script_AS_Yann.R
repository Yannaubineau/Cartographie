library(tidyverse)
library(questionr)
library(readxl)

lilas

clémence 

disp <- read.csv("data/FILO2018_DISP_COM.csv", sep = ";")

# Table des taux de pauvreté
pvr <- read.csv("data/FILO2018_DISP_Pauvres_COM.csv", sep = ";")


# Table des déciles revenu
dec <- read.csv("data/FILO2018_TRDECILES_DISP_COM.csv", sep = ";")


# Table de la composition communale des unités urbaines
uu <- read_excel("data/UU2020_au_01-01-2021.xlsx", sheet = 2, skip = 5)


# Extraction des communes faisant partie de l'UU Paris
uu <- uu %>% 
  filter(UU2020 == "00756")

# Filtrage de la table disp
disp <- disp %>% 
  filter(CODGEO %in% uu$CODGEO)

# Filtrage de la table pvr
pvr <- pvr %>% 
  filter(CODGEO %in% uu$CODGEO)


# Filtrage de la table dec
dec <- dec %>% 
  filter(CODGEO %in% uu$CODGEO)


# Sélection des variables décrivant l'ensemble des ménages
disp <- disp %>% 
  select(1:30)


# Contrôler la présence de valeurs manquantes dans une variable
table(is.na(disp$D118))
table(disp$D118, useNA = "ifany")

# Connaitre le résumé statistique

table(disp$RD, useNA = "ifany")

# Observer la distribution d'une variable
# Courbe de densité
ggplot(disp, aes(x=Q218)) + 
  geom_density()+
  scale_y_continuous(labels = scales::comma)+ # se débarasser de la notation scientifique
  # Habiller le graphique
  ylab("Densité de probabilité")+
  xlab("Revenu disponible médian") +
  ggtitle("Distribution du revenu médian par commune à Paris")



# Boîte à moustache
ggplot(disp_caen_ens, aes(x = Q218))+
  geom_boxplot()


# Graphique rang-taille

ggplot(disp, aes(x= reorder(CODGEO, -NBPERS18), y = NBPERS18))+
  geom_point()+
  scale_y_log10("Population communale", labels = scales:: comma)+
  scale_x_discrete("")



test <- disp %>% select(contains("AGE")) %>% select(contains("Q2"))
colnames(test)

