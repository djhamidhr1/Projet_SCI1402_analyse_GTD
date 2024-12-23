# ==============================================================================
#                 IMPORTATION ET ANALYSE APPROFONDIE DES DONNÉES
# ==============================================================================

# -----------------------------------------------------------------------------
#                1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------

# 1.1 Chargement des packages nécessaires
library(tidyverse)  # Manipulation des données
library(sparklyr)   # Interface avec Apache Spark
library(arrow)      # Sauvegarde en Parquet
library(readr)      # Lecture rapide des fichiers CSV
library(ggplot2)    # Visualisation des données

# 1.2 Configuration des chemins d'accès
PATHS <- list(
  RAW_DATA = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/raw/globalterrorismdb_0718dist.csv",
  PROCESSED_CSV = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv",
  PROCESSED_PARQUET = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.parquet"
)

# 1.3 Initialisation de Spark
sc <- spark_connect(master = "local")

# -----------------------------------------------------------------------------
#                2. IMPORTATION ET NETTOYAGE DES DONNÉES
# -----------------------------------------------------------------------------
# Importer et nettoyer les données
import_and_clean <- function(file_path) {
  message("\n--- Importation et nettoyage des données ---")
  data <- spark_read_csv(sc, name = "gtd_data", path = file_path, infer_schema = TRUE, memory = FALSE) %>%
    select(
      year = iyear, month = imonth, day = iday,
      country = country_txt, region = region_txt, city,
      latitude, longitude, attack_type = attacktype1_txt,
      target_type = targtype1_txt, weapon_type = weaptype1_txt,
      killed = nkill, wounded = nwound, success
    ) %>%
    mutate(
      killed = as.numeric(killed),
      wounded = as.numeric(wounded),
      success = as.numeric(success)
    ) %>%
    mutate_at(vars(killed, wounded), ~ ifelse(is.na(.), 0, .))  # Remplacer NA par 0
  return(data)
}

# -----------------------------------------------------------------------------
#                3. VISUALISATIONS
# -----------------------------------------------------------------------------
# 3.1 Visualisation des attaques par région
plot_attacks_by_region <- function(data) {
  message("\n--- Visualisation des attaques par région ---")
  data_r <- data %>%
    group_by(region) %>%
    summarise(total_attacks = n()) %>%
    collect()
  
  ggplot(data_r, aes(x = reorder(region, -total_attacks), y = total_attacks)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Nombre d'attaques par région",
      x = "Région",
      y = "Nombre d'attaques"
    ) +
    theme_minimal()
}

# 3.2 Visualisation des types d'attaques
plot_attack_types <- function(data) {
  message("\n--- Visualisation des types d'attaques ---")
  data_r <- data %>%
    group_by(attack_type) %>%
    summarise(total_attacks = n()) %>%
    collect()
  
  ggplot(data_r, aes(x = reorder(attack_type, -total_attacks), y = total_attacks)) +
    geom_bar(stat = "identity", fill = "tomato") +
    coord_flip() +
    labs(
      title = "Nombre d'attaques par type",
      x = "Type d'attaque",
      y = "Nombre d'attaques"
    ) +
    theme_minimal()
}

# 3.3 Visualisation de l'impact humain par année
plot_human_impact <- function(data) {
  message("\n--- Visualisation de l'impact humain par année ---")
  data_r <- data %>%
    group_by(year) %>%
    summarise(
      total_killed = sum(killed, na.rm = TRUE),
      total_wounded = sum(wounded, na.rm = TRUE)
    ) %>%
    collect()
  
  ggplot(data_r, aes(x = year)) +
    geom_line(aes(y = total_killed, color = "Tués"), size = 1.2) +
    geom_line(aes(y = total_wounded, color = "Blessés"), size = 1.2) +
    labs(
      title = "Impact humain des attaques terroristes par année",
      x = "Année",
      y = "Nombre de personnes",
      color = "Impact"
    ) +
    theme_minimal()
}

# 3.4 Visualisation des attaques par année pour repérer les tendances
plot_attacks_by_year <- function(data) {
  message("\n--- Visualisation des attaques par année ---")
  data_r <- data %>%
    group_by(year) %>%
    summarise(total_attacks = n()) %>%
    collect()
  
  ggplot(data_r, aes(x = year, y = total_attacks)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    labs(
      title = "Nombre d'attaques terroristes par année",
      x = "Année",
      y = "Nombre d'attaques"
    ) +
    theme_minimal()
}

# -----------------------------------------------------------------------------
#                4. EXÉCUTION PRINCIPALE
# -----------------------------------------------------------------------------
main <- function() {
  message("\n=== Début du traitement et de l'analyse ===")
  
  # Importation et nettoyage des données
  data <- import_and_clean(PATHS$RAW_DATA)
  
  # Visualisations
  plot_attacks_by_region(data)
  plot_attack_types(data)
  plot_human_impact(data)
  plot_attacks_by_year(data)
  
  # Sauvegarde des données nettoyées
  spark_write_csv(data, PATHS$PROCESSED_CSV, mode = "overwrite")
  spark_write_parquet(data, PATHS$PROCESSED_PARQUET, mode = "overwrite")
  
  message("\n=== Fin du traitement et de l'analyse ===")
}

# -----------------------------------------------------------------------------
#                5. LANCEMENT DU SCRIPT
# -----------------------------------------------------------------------------
if (!interactive()) {
  main()
} else {
  message("\nExécution en mode interactif...")
  data_test <- import_and_clean(PATHS$RAW_DATA)
  plot_attacks_by_region(data_test)
  plot_attack_types(data_test)
  plot_human_impact(data_test)
  plot_attacks_by_year(data_test)
}

# -----------------------------------------------------------------------------
#                6. FERMETURE DE LA CONNEXION SPARK
# -----------------------------------------------------------------------------
spark_disconnect(sc)
