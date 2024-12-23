# ==============================================================================
#                    CONFIGURATION ET TRAITEMENT DES DONNÉES GTD
# ==============================================================================

# -----------------------------------------------------------------------------
#                    1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------

# 1.1 Chargement des packages
library(tidyverse)
library(sparklyr)
library(arrow)

# 1.2 Configuration des chemins
PATHS <- list(
  RAW_DATA = "data/raw/globalterrorismdb_0718dist.csv",
  PROCESSED_CSV = "data/processed/gtd_clean.csv",
  PROCESSED_PARQUET = "data/processed/terrorism_processed.parquet",
  SPARK_PARQUET = "data/processed/gtd_raw.parquet"
)

# 1.3 Création des répertoires
for(path in PATHS) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------------------------------------------------------
#                    2. CONFIGURATION DE SPARK
# -----------------------------------------------------------------------------

configure_spark <- function() {
  config <- spark_config()
  config$`sparklyr.shell.driver-memory` <- "4G"
  config$`sparklyr.shell.executor-memory` <- "4G"
  config$spark.executor.cores <- 2
  return(config)
}

connect_to_spark <- function() {
  tryCatch({
    message("Connexion à Spark...")
    config <- configure_spark()
    sc <- spark_connect(
      master = "local",
      version = "3.5.1",
      config = config
    )
    message("Connexion Spark établie!")
    return(sc)
  }, error = function(e) {
    message("Erreur Spark : ", e$message)
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
#                    3. IMPORTATION DES DONNÉES
# -----------------------------------------------------------------------------

import_data <- function(file_path, sc) {
  tryCatch({
    message("Import des données avec Spark...")
    
    # Lecture avec Spark
    gtd_spark <- spark_read_csv(
      sc,
      name = "gtd_data",
      path = file_path,
      header = TRUE,
      infer_schema = TRUE,
      options = list(
        encoding = "UTF-8",
        multiLine = TRUE
      )
    )
    
    # Sélection des colonnes
    gtd_data <- gtd_spark %>%
      select(
        iyear, imonth, iday,
        country_txt, region_txt, city,
        latitude, longitude,
        attacktype1_txt, targtype1_txt, weaptype1_txt,
        nkill, nwound, property, success
      )
    
    message("Import réussi!")
    return(gtd_data)
    
  }, error = function(e) {
    message("Erreur d'import : ", e$message)
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
#                    4. ANALYSE ET STOCKAGE
# -----------------------------------------------------------------------------

analyze_and_store <- function(data, sc) {
  # Analyse basique
  message("\nAnalyse des données...")
  tbl_cache(sc, "gtd_data")
  
  # Statistiques de base
  stats <- data %>%
    summarise(
      total_incidents = n(),
      total_killed = sum(nkill, na.rm = TRUE),
      total_wounded = sum(nwound, na.rm = TRUE)
    ) %>%
    collect()
  
  print(stats)
  
  # Sauvegarde
  message("\nSauvegarde des données...")
  spark_write_parquet(
    data,
    path = PATHS$SPARK_PARQUET,
    mode = "overwrite"
  )
  
  message("Données sauvegardées!")
}

# -----------------------------------------------------------------------------
#                    5. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
  message("=== DÉBUT DU TRAITEMENT ===")
  
  # Connexion Spark
  sc <- connect_to_spark()
  if (is.null(sc)) {
    stop("Impossible de continuer sans Spark")
  }
  
  tryCatch({
    # Import
    data <- import_data(PATHS$RAW_DATA, sc)
    if (!is.null(data)) {
      # Analyse et stockage
      analyze_and_store(data, sc)
    }
    
    # Déconnexion
    spark_disconnect(sc)
    message("=== TRAITEMENT TERMINÉ ===")
    
  }, error = function(e) {
    message("Erreur : ", e$message)
    if (!is.null(sc)) spark_disconnect(sc)
  })
}

# Exécution
if (!interactive()) main()

# Vérifier les fichiers créés
list.files("data/processed")