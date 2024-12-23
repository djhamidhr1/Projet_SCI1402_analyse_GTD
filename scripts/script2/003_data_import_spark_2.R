# ==============================================================================
#                 IMPORTATION, TRAITEMENT ET VISUALISATION AVEC SPARK
# ==============================================================================

# -----------------------------------------------------------------------------
#                1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------
# Chargement des packages nécessaires
library(tidyverse)   # Manipulation de données
library(sparklyr)    # Interface avec Apache Spark
library(arrow)       # Sauvegarde en format Parquet
library(ggplot2)     # Visualisation

# Définition des chemins d'accès
PATHS <- list(
  RAW_DATA = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/raw/globalterrorismdb_0718dist.csv",
  CLEAN_CSV = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv",
  CLEAN_PARQUET = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.parquet"
)

# Configuration Spark
sc <- spark_connect(master = "local")

# -----------------------------------------------------------------------------
#                2. IMPORTATION DES DONNÉES BRUTES ET VISUALISATION
# -----------------------------------------------------------------------------
message("\n=== IMPORTATION DES DONNÉES BRUTES ===\n")

# 2.1 Importation des données brutes avec Spark
import_data_spark <- function(file_path) {
  data <- spark_read_csv(sc, name = "gtd_raw", path = file_path, infer_schema = TRUE, memory = FALSE)
  return(data)
}

# 2.2 Visualisation brute des données
visualiser_brut <- function(data) {
  message("\n--- Aperçu des données brutes ---\n")
  print(data %>% head(10) %>% collect())
  
  message("\n--- Résumé des données brutes ---\n")
  print(summary(data %>% collect()))
  
  # Visualisation des données brutes
  data_r <- data %>% collect()
  
  ggplot(data_r, aes(x = as.factor(iyear))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Distribution des incidents par année (données brutes)",
         x = "Année", y = "Nombre d'incidents") +
    theme_minimal()
}

# Exécution de l'importation et de la visualisation brute
data_raw <- import_data_spark(PATHS$RAW_DATA)
visualiser_brut(data_raw)

# -----------------------------------------------------------------------------
#                3. TRAITEMENT ET NETTOYAGE DES DONNÉES
# -----------------------------------------------------------------------------
message("\n=== TRAITEMENT ET NETTOYAGE DES DONNÉES ===\n")

# 3.1 Sélection et nettoyage des données
clean_and_process <- function(data) {
  data_clean <- data %>%
    select(
      year = iyear, month = imonth, day = iday,
      country = country_txt, region = region_txt, city,
      latitude, longitude, attack_type = attacktype1_txt,
      target_type = targtype1_txt, weapon_type = weaptype1_txt,
      killed = nkill, wounded = nwound, success
    ) %>%
    mutate(
      killed = ifelse(is.na(as.numeric(killed)), 0, as.numeric(killed)),
      wounded = ifelse(is.na(as.numeric(wounded)), 0, as.numeric(wounded)),
      total_casualties = killed + wounded,
      success = as.logical(success)
    ) %>%
    filter(!is.na(year)) %>%  # Suppression des lignes sans année
    distinct()               # Suppression des doublons
  return(data_clean)
}

data_clean <- clean_and_process(data_raw)

# 3.2 Visualisation des données nettoyées
visualiser_clean <- function(data) {
  data_r <- data %>% collect()
  
  # Visualisation des incidents par année
  ggplot(data_r, aes(x = year)) +
    geom_bar(fill = "green") +
    labs(title = "Nombre d'incidents par année (Nettoyé)",
         x = "Année", y = "Nombre d'incidents") +
    theme_minimal()
  
  # Visualisation des attaques par région
  ggplot(data_r, aes(x = fct_reorder(region, year, .fun = length))) +
    geom_bar(fill = "darkorange") +
    coord_flip() +
    labs(title = "Nombre d'incidents par région",
         x = "Région", y = "Nombre d'incidents") +
    theme_minimal()
}

visualiser_clean(data_clean)

# -----------------------------------------------------------------------------
#                4. EXPORTATION DES DONNÉES NETTOYÉES
# -----------------------------------------------------------------------------
message("\n=== EXPORTATION DES DONNÉES NETTOYÉES ===\n")

export_data <- function(data, csv_path, parquet_path) {
  # Sauvegarde au format CSV
  spark_write_csv(data, path = csv_path, mode = "overwrite")
  message("Données exportées en CSV : ", csv_path)
  
  # Sauvegarde au format Parquet
  spark_write_parquet(data, path = parquet_path, mode = "overwrite")
  message("Données exportées en Parquet : ", parquet_path)
}

export_data(data_clean, PATHS$CLEAN_CSV, PATHS$CLEAN_PARQUET)

# -----------------------------------------------------------------------------
#                5. FERMETURE DE LA CONNEXION SPARK
# -----------------------------------------------------------------------------
spark_disconnect(sc)
message("\n=== TRAITEMENT TERMINÉ AVEC SUCCÈS ===\n")
