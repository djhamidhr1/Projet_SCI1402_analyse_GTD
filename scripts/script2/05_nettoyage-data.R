# Charger les bibliothèques nécessaires
library(tidyverse)

# Charger les données depuis le script d'importation
file_path <- "data/raw/globalterrorismdb_0718dist.csv" # Chemin correct vers le fichier brut
donnees_gtd <- read_csv(file_path, show_col_types = FALSE)

# Étape 1 : Aperçu des données
cat("\n**Aperçu des données**\n")
cat("Nombre de lignes :", nrow(donnees_gtd), "\n")
cat("Nombre de colonnes :", ncol(donnees_gtd), "\n")
cat("Types des colonnes :\n")
print(sapply(donnees_gtd, class))

# Identifier les colonnes avec des valeurs manquantes
cat("\n**Colonnes avec des valeurs manquantes**\n")
missing_data <- colSums(is.na(donnees_gtd))
missing_data <- missing_data[missing_data > 0]
print(missing_data)

# Problèmes de parsing (si existant)
cat("\n**Problèmes de parsing**\n")
parsing_problems <- problems(donnees_gtd)
if (nrow(parsing_problems) > 0) {
  print(parsing_problems)
} else {
  cat("Aucun problème de parsing trouvé.\n")
}

# Étape 2 : Résumé statistique des colonnes
cat("\n**Résumé statistique**\n")
print(summary(donnees_gtd))

# Étape 3 : Exploration des colonnes clés
cat("\n**Valeurs uniques pour les colonnes clés**\n")
cat("Régions :\n")
print(unique(donnees_gtd$region_txt))

cat("\nTypes d'attaques :\n")
print(unique(donnees_gtd$attacktype1_txt))

# Étape 4 : Nettoyage des données
## Supprimer les colonnes entièrement vides
cat("\n**Suppression des colonnes vides**\n")
donnees_gtd <- donnees_gtd[, colSums(!is.na(donnees_gtd)) > 0]

## Supprimer les doublons
cat("\n**Suppression des doublons**\n")
nb_doublons <- nrow(donnees_gtd) - nrow(distinct(donnees_gtd))
if (nb_doublons > 0) {
  cat(nb_doublons, "doublons supprimés.\n")
  donnees_gtd <- distinct(donnees_gtd)
} else {
  cat("Aucun doublon trouvé.\n")
}

## Analyser les valeurs manquantes pour des actions spécifiques
cat("\n**Colonnes restantes avec des valeurs manquantes**\n")
missing_data <- colSums(is.na(donnees_gtd))
missing_data <- missing_data[missing_data > 0]
print(missing_data)

# Étape 5 : Visualisation rapide des données nettoyées
cat("\n**Aperçu des 10 premières lignes des données nettoyées**\n")
print(head(donnees_gtd, 10))

# Exporter les données nettoyées dans le répertoire 'processed/'
output_dir <- "data/processed/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
output_file <- paste0(output_dir, "donnees_gtd_nettoyees.csv")
write_csv(donnees_gtd, output_file)

cat("\nTraitement terminé. Les données nettoyées sont enregistrées sous '", output_file, "'.\n")
