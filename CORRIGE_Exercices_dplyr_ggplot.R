#################################################################
##          Manipulation de données avec R: Exercices          ##
#################################################################

# Ci-après vous trouverez les exercices en lien avec le séminaire
# EEG 

# Commencez par importer les données en exécutant le code 
# ci-dessous. Attention le fichier Excel doit être dans le 
# même répertoire de travail. Sinon utilisez l'utilitaire 
# d'importation sous l'onglet environnement en haut à droite

library(readxl)
data_exercices <- read_excel("data_exercices.xlsx")
View(data_exercices)


##----------------------------------------------------------------
##                          Exercice 1                           -
##----------------------------------------------------------------

##--------------
##  Question 1  
##--------------

# Commencez par explorer le jeu de données en affichant les 6 premières
# lignes et en affichant la structure du document. Pour cela utilisez 
# les fonctions head() et str()

head(data_exercices)

str(data_exercices)

##--------------
##  Question 2  
##--------------

# Le processus de nettoyage de données demande souvent de supprimer les 
# valeurs manquantes du jeu de données (NA). Attention, dans R, les NA ont
# un statut un peu spécial. En effet, pour tester si A vaut 4, on utilisera
# l'expression A == 4. Par contre pour traiter des NA, il faudra utiliser
# l'argument is.na(A) afin de savoir si A vaut NA. Pour savoir si A est 
# différent de NA, il faudra alors utiliser l'expression !is.na(A).
# Sachant cela, en utilisant la fonction filter de dplyr, créez un nouveau
# data frame nommé data_no_NA contenant uniquement les valeurs pour la variable RT. Attention,
# pensez bien à charger le package dplyr avant de commencer. 

library(dplyr)

data_no_na <- filter(data_exercices, !is.na(RT))

##--------------
##  Question 3  
##--------------

# Dans le cadre de l'analyse sur les temps de réaction la colonne accuracy
# n'apporte rien. gardez toutes les autres colonnes du data frame data_no_na sauf
# la colonne accuracy. Utilisez pour cela la fonction select(). Il est soit possible
# de recopier tous les noms de variables, soit d'écire uniquement la variable accuracy
# précédée d'un -.Sauvegardez le résultat dans un data frame appelé data_no_na_RT.

data_no_na_RT <- select(data_no_na, -accuracy)

##--------------
##  Question 4  
##--------------

# En utilisant les arguments summarize() et group_by(), sortez les statistiques descriptives
# moyennes et écarts-types par condition et par groupe d'âge du data frame data_na_no_RT. 
#Appelez ces deux statistiques moyennes et ET.

data_no_na_RT%>%
  group_by(group, condition)%>%
  summarise(moyenne = mean(RT), ET = sd(RT))

##--------------
##  Question 5  
##--------------

# Afin de préparer les analyses EEG, pour être certain de pas être dans la période de 
# l'articulation, généralement, nous retranchons 100ms aux TR. Utilisez la fonction 
# mutate() pour créer la colonne RT_EEG comptant 100ms de moins que la colonne RT. 
# sauvegardez le dataframe sous le nom data_no_na_RT_EEG


data_no_na_RT_EEG <- data_no_na_RT%>%
  mutate(RT_EEG = RT-100)


##----------------------------------------------------------------
##                          Exercice 2                           -
##----------------------------------------------------------------

library(ggplot2)

##--------------
##  Question 1  
##--------------

# établissez un boxplot des TR par condition. et sauvegardez le plot sous le 
# nom p1. Utilisez également le data frame data_no_na_RT_EEG

p1 <- ggplot(data_no_na_RT_EEG, aes(x = condition, y = RT)) + geom_boxplot()

##--------------
##  Question 2  
##--------------

# Afin de comprendre l'évolution des RT en fonction des groupes d'âges, assignez
# la variable groupe à l'aesthetic fill. Nommez ce plot p2.

p2 <- ggplot(data_no_na_RT_EEG, aes(x = condition, y = RT, fill = group))+ geom_boxplot()
p2 

##--------------
##  Question 2  
##--------------
# Les couleurs des boxplots peuvent être modifiées. lorsque l'argument fill est utilisé dans ce
# contexte, il est possible de changer les couleurs de base en ajoutant + scale_fill_brewer(palette = 
# "..."). Pour consulter les palettes de couleur disponibles: 
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
# Nommez votre plot p3


p3 <- p2 + scale_fill_brewer(palette = "Blues")
             
##--------------
##  Question 3  
##--------------

# Reprennez le plot précédent, et: 
#   - Ajoutez un titre
#   - Renommez l'axe des x : "Current Trial"
#   - Changez le titre de la légende : "Age Groups"*
#   - Appliquez le theme_minimal()
# 
# *Pour changer le titre de la légende, dans la fonction lab(), inscrivez fill = "nom de la
# légende". 
# nommez votre plot p4

p3 + labs(title = "Mon joli plot", x = "Current Trial", fill = "Age Groups") +
  theme_minimal()

