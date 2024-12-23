# ==============================================================================
#                    INSTALLATION DES PACKAGES R
# ==============================================================================

# -----------------------------------------------------------------------------
#                    1. LISTE DES PACKAGES
# -----------------------------------------------------------------------------

# Liste des packages organisés par catégorie
categories_packages <- list(
  # 1. Data manipulation et visualisation
  data_visu = c("tidyverse", "scales", "gridExtra", "knitr", "rmarkdown"),
  
  # 2. Visualisation géographique
  geo_visu = c("leaflet", "sf", "maps", "mapdata"),
  
  # 3. Palettes et couleurs
  palettes = c("RColorBrewer", "viridis"),
  
  # 4. Visualisation avancée
  adv_visu = c("plotly", "treemapify", "corrplot", "gganimate", "animation"),
  
  # 5. Analyse statistique et Machine Learning
  ml_stats = c("cluster", "factoextra", "caret", "xgboost", "randomForest", 
               "e1071", "nnet", "ROCR", "pROC", "MLmetrics"),
  
  # 6. Analyse temporelle
  time_analysis = c("lubridate", "tsibble", "feasts", "fable", "forecast", 
                    "prophet", "zoo"),
  
  # 7. Big Data
  big_data = c("sparklyr"),
  
  # 8. Traitement des données
  data_processing = c("janitor", "data.table", "DT", "htmlwidgets"),
  
  # 9. Export et rapports
  export_reports = c("openxlsx", "kableExtra", "bookdown", "flexdashboard"),
  
  # 10. Séries temporelles
  time_series = c("quantmod", "TTR", "astsa")
)

# -----------------------------------------------------------------------------
#                    2. FONCTIONS D'INSTALLATION ET CHARGEMENT
# -----------------------------------------------------------------------------

# Fonction d'installation des packages manquants
installer_packages <- function(packages) {
  packages_manquants <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(packages_manquants) > 0) {
    cat("\nInstallation des packages manquants:\n")
    print(packages_manquants)
    for (pkg in packages_manquants) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com")
        cat(sprintf("✓ Package %s installé avec succès\n", pkg))
      }, error = function(e) {
        cat(sprintf("⚠ Erreur lors de l'installation de %s: %s\n", pkg, e$message))
      })
    }
  } else {
    cat("\nTous les packages sont déjà installés!\n")
  }
}

# Fonction de chargement des packages
charger_packages <- function(packages) {
  invisible(lapply(packages, function(pkg) {
    tryCatch({
      if (!require(pkg, character.only = TRUE)) {
        stop(sprintf("Le package %s n'a pas pu être chargé", pkg))
      }
      cat(sprintf("✓ Package %s chargé avec succès\n", pkg))
    }, error = function(e) {
      cat(sprintf("⚠ Erreur lors du chargement de %s: %s\n", pkg, e$message))
    }, warning = function(w) {
      cat(sprintf("⚠ Avertissement lors du chargement de %s: %s\n", pkg, w$message))
    })
  }))
}

# -----------------------------------------------------------------------------
#                    3. INSTALLATION ET CHARGEMENT
# -----------------------------------------------------------------------------

cat("=== DÉBUT DE L'INSTALLATION DES PACKAGES ===\n")
for (cat_name in names(categories_packages)) {
  cat(sprintf("\nTraitement des packages pour la catégorie: %s\n", cat_name))
  installer_packages(categories_packages[[cat_name]])
}

cat("\n=== CHARGEMENT DES PACKAGES ===\n")
for (cat_name in names(categories_packages)) {
  cat(sprintf("\nChargement des packages pour la catégorie: %s\n", cat_name))
  charger_packages(categories_packages[[cat_name]])
}

cat("\n=== INSTALLATION ET CHARGEMENT TERMINÉS ===\n")
