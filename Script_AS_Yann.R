library(tidyverse)
library(questionr)
library(readxl)
library(ggplot2)
library(cartography) # fonction carto.col
library(ggpubr) #ggarrange

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


disp <- disp %>% 
  filter(CODGEO %in% uu$CODGEO)
test <- disp %>% dplyr::select(contains("AGE")) %>% dplyr::select(contains("Q2"))
colnames(test)


disp_douai_long<-gather(disp,age,median_age,colnames(test), na.rm = T)

#Distribution du revenu médian par catégorie d'âge 
douai_plot <- ggplot(disp_douai_long, aes(x = median_age, fill = age,alpha=0.3))+
  geom_density(alpha=0.8,size = 0.8)+
  scale_y_continuous(labels = scales::comma)+ # se débarasser de la notation scientifique
  # Habiller le graphique
  ylab("Densité de probabilité")+
  xlab("Revenu disponible médian")+
  ggtitle("Distribution du revenu médian par catégorie d'âge par commune à Douai-Lens")+
  scale_fill_manual("Catégories d'âge", labels=c("Moins de 30 ans","30-39 ans","40-49 ans",
                                                 "50-59 ans","60-74 ans","75 ans et plus"),
                    values = rev(carto.pal(pal1 = "blue.pal",n1=6))) +
  xlim(10000,35000) +
  theme_minimal() 



disp_tout <- read.csv("data/FILO2018_DISP_COM.csv", sep = ";")

test <- disp_tout %>% dplyr::select(contains("AGE")) %>% dplyr::select(contains("Q2"))
colnames(test)

disp_tout_long<-gather(disp_tout,age,median_age,colnames(test), na.rm = T)

#Distribution du revenu médian par catégorie d'âge 
# tout_plot <- 
ggplot(disp_tout_long, aes(x = median_age, fill = age))+
  geom_density(alpha=0.8, size = 0.8)+
  scale_y_continuous(labels = scales::comma)+ # se débarasser de la notation scientifique
  # Habiller le graphique
  ylab("Densité de probabilité")+
  xlab("Revenu disponible médian")+
  ggtitle("Distribution du revenu médian par catégorie d'âge par commune en France")+
  scale_fill_manual("Catégories d'âge", labels=c("Moins de 30 ans","30-39 ans","40-49 ans",
                                                 "50-59 ans","60-74 ans","75 ans et plus"),
                    values = rev(carto.pal(pal1 = "blue.pal",n1=6))) +
  theme_minimal() +
  geom_text(aes(x = 0.0001, y = 0.0001, label = "Adelie"), color="black") + 
  xlim(10000,35000)
library(ggpubr) #ggarrange

ggarrange(douai_plot, tout_plot, common.legend = TRUE, legend = "right",
                    ncol = 1, nrow = 2) 
# indicateurs relatifs, écarts interquartiles, coefficients de variation

disp <- disp %>% mutate(Q218_tout = AGE1Q218 +AGE2Q218 + AGE3Q218 + AGE4Q218 + AGE5Q218 + AGE6Q218)

(sd(disp$Q218_tout, na.rm = T)/mean(disp$Q218_tout, na.rm = T))*100

disp_tout <- disp_tout %>% mutate(Q218_tout = AGE1Q218 +AGE2Q218 + AGE3Q218 + AGE4Q218 + AGE5Q218 + AGE6Q218)

(sd(disp_tout$Q218_tout, na.rm = T)/mean(disp_tout$Q218_tout, na.rm = T))*100
