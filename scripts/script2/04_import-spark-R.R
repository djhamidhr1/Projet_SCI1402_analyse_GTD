# ==============================================================================
#                    IMPORTATION ET TRAITEMENT DES DONNÉES
# ==============================================================================

# -----------------------------------------------------------------------------
#                    1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------

# 1.1 Chargement des packages nécessaires
library(tidyverse)  # Manipulation de données (dplyr, ggplot2, etc.)
library(sparklyr)   # Interface Spark pour R
library(arrow)      # Lecture et écriture des fichiers Parquet

# 1.2 Configuration des chemins
PATHS <- list(
  RAW_DATA = "data/raw/globalterrorismdb_0718dist.csv",
  PROCESSED_CSV = "data/processed/gtd_clean.csv",
  PROCESSED_PARQUET = "data/processed/terrorism_processed.parquet",
  SPARK_PARQUET = "data/processed/gtd_raw.parquet"
)

# 1.3 Création des répertoires si nécessaire
for (path in PATHS) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------------------------------------------------------
#                    2. FONCTIONS D'IMPORTATION
# -----------------------------------------------------------------------------

# 2.1 Importation standard avec R
import_with_r <- function(file_path) {
  tryCatch({
    message("Importation des données avec R...")
    data <- read_csv(file_path) %>%
      select(
        year = iyear,
        month = imonth,
        day = iday,
        country = country_txt,
        region = region_txt,
        city,
        latitude,
        longitude,
        attack_type = attacktype1_txt,
        target_type = targtype1_txt,
        weapon_type = weaptype1_txt,
        killed = nkill,
        wounded = nwound,
        property,
        success
      )
    message("Importation R réussie!")
    return(data)
  }, error = function(e) {
    message("Erreur lors de l'importation R : ", e$message)
    return(NULL)
  })
}

# 2.2 Importation avec Spark
import_with_spark <- function(file_path, sc) {
  tryCatch({
    message("Importation des données avec Spark...")
    
    # Importation avec Spark
    gtd_spark <- spark_read_csv(
      sc,
      name = "gtd_data",
      path = file_path,
      header = TRUE,
      infer_schema = TRUE
    )
    
    # Sélection et renommage des colonnes
    gtd_data <- gtd_spark %>%
      select(
        iyear, imonth, iday,
        country_txt, region_txt, city,
        latitude, longitude,
        attacktype1_txt, targtype1_txt, weaptype1_txt,
        nkill, nwound, property, success
      )
    
    message("Importation Spark réussie!")
    return(gtd_data)
    
  }, error = function(e) {
    message("Erreur lors de l'importation Spark : ", e$message)
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
#                    3. FONCTIONS D'ANALYSE
# -----------------------------------------------------------------------------

examine_data <- function(data) {
  message("\n=== ANALYSE DES DONNÉES ===")
  
  message("\nAperçu des données:")
  print(glimpse(data))
  
  message("\nRésumé statistique:")
  print(summary(data))
  
  message("\nValeurs manquantes par colonne:")
  print(colSums(is.na(data)))
}

# -----------------------------------------------------------------------------
#                    4. FONCTIONS DE STOCKAGE
# -----------------------------------------------------------------------------

store_data <- function(data, output_path, format = "parquet") {
  tryCatch({
    message(sprintf("\nSauvegarde des données en format %s...", format))
    
    if (format == "parquet") {
      write_parquet(data, output_path)
    } else if (format == "csv") {
      write_csv(data, output_path)
    }
    
    message("Données sauvegardées avec succès dans : ", output_path)
    
  }, error = function(e) {
    message("Erreur lors de la sauvegarde : ", e$message)
  })
}

# -----------------------------------------------------------------------------
#                    5. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
  message("=== DÉBUT DU TRAITEMENT DES DONNÉES ===\n")
  
  # 5.1 Connexion à Spark
  config <- spark_config()
  config$spark.executor.memory <- "4G"
  sc <- spark_connect(master = "local", config = config)
  
  # 5.2 Import des données (deux méthodes)
  data_r <- import_with_r(PATHS$RAW_DATA)
  data_spark <- import_with_spark(PATHS$RAW_DATA, sc)
  
  # 5.3 Analyse des données
  if (!is.null(data_r)) {
    examine_data(data_r)
    store_data(data_r, PATHS$PROCESSED_CSV, "csv")
    store_data(data_r, PATHS$PROCESSED_PARQUET, "parquet")
  }
  
  # 5.4 Stockage des données Spark
  if (!is.null(data_spark)) {
    spark_write_parquet(
      data_spark,
      path = PATHS$SPARK_PARQUET,
      mode = "overwrite"
    )
  }
  
  # 5.5 Déconnexion de Spark
  spark_disconnect(sc)
  
  message("\n=== TRAITEMENT DES DONNÉES TERMINÉ ===")
}

# -----------------------------------------------------------------------------
#                    6. EXÉCUTION
# -----------------------------------------------------------------------------

if (!interactive()) {
  main()
}
