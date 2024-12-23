# Charger les bibliothèques nécessaires
library(readr)      # Pour lire des fichiers CSV efficacement
library(dplyr)      # Pour manipuler les données

# Définir le chemin vers le fichier CSV
chemin_fichier <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/raw/globalterrorismdb_0718dist.csv"

# Importer les données
donnees_gtd <- read_csv(chemin_fichier)

# Afficher un aperçu des données
print("Aperçu des données :")
print(head(donnees_gtd, n = 10))  # Afficher les 10 premières lignes

# Obtenir des informations sur la structure des données
print("Structure des données :")
print(str(donnees_gtd))

# Résumer les données
print("Résumé statistique des données :")
print(summary(donnees_gtd))
