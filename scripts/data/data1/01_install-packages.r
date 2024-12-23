# ==============================================================================
#                    INSTALLATION DES PACKAGES R
# ==============================================================================

# Liste des packages à installer
packages_necessaires <- c(
  # 1. Data manipulation et visualisation de base
  "tidyverse",     # ggplot2, dplyr, tidyr, readr, etc.
  "scales",        # Formatage des échelles
  "gridExtra",     # Organisation des graphiques
  "knitr",         # Génération de rapports
  "rmarkdown",     # Création de documents
  
  # 2. Visualisation géographique
  "leaflet",       # Cartes interactives
  "sf",            # Données géographiques
  "maps",          # Données cartographiques
  "mapdata",       # Données supplémentaires
  
  # 3. Palettes et couleurs
  "RColorBrewer",  # Palettes de couleurs
  "viridis",       # Palettes accessibles
  
  # 4. Visualisation avancée
  "plotly",        # Graphiques interactifs
  "treemapify",    # Création de treemaps
  "corrplot",      # Matrices de corrélation
  "gganimate",     # Animations
  "animation",     # Support d'animation
  
  # 5. Analyse statistique et Machine Learning
  "cluster",       # Analyse de clustering
  "factoextra",    # Visualisation clusters
  "caret",         # Machine Learning
  "xgboost",       # Boosting
  "randomForest",  # Random Forest
  "e1071",         # SVM
  "nnet",          # Réseaux de neurones
  "ROCR",          # Courbes ROC
  "pROC",          # Analyse ROC
  "MLmetrics",     # Métriques ML
  
  # 6. Analyse temporelle
  "lubridate",     # Gestion des dates
  "tsibble",       # Séries temporelles
  "feasts",        # Analyse de séries temporelles
  "fable",         # Prévision de séries temporelles
  "forecast",      # Prévisions avancées
  "prophet",       # Prévisions Facebook
  "zoo",           # Séries temporelles irrégulières
  
  # 7. Big Data
  "sparklyr",      # Interface R pour Apache Spark
  
  # 8. Traitement des données
  "janitor",       # Nettoyage des données
  "data.table",    # Manipulation efficace
  "DT",            # Tables interactives
  "htmlwidgets",   # Widgets HTML
  
  # 9. Export et rapports
  "openxlsx",      # Export Excel
  "kableExtra",    # Tables améliorées
  "bookdown",      # Documents longs
  "flexdashboard", # Tableaux de bord
  
  # 10. Packages supplémentaires pour l'analyse de séries temporelles
  "quantmod",      # Analyse financière
  "TTR",           # Technical Trading Rules
  "astsa"          # Applied Statistical Time Series Analysis
)

# Fonction d'installation des packages manquants
installer_packages <- function(packages) {
  packages_manquants <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(packages_manquants) > 0) {
    cat("\nInstallation des packages manquants:\n")
    print(packages_manquants)
    for(pkg in packages_manquants) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE)
        cat(sprintf("✓ Package %s installé avec succès\n", pkg))
      }, error = function(e) {
        cat(sprintf("⚠ Erreur lors de l'installation de %s: %s\n", pkg, e$message))
      })
    }
  } else {
    cat("\nTous les packages sont déjà installés!\n")
  }
}

# Exécution de l'installation
cat("=== DÉBUT DE L'INSTALLATION DES PACKAGES ===\n")
try({
  installer_packages(packages_necessaires)
})

# Vérification finale
packages_installes <- packages_necessaires %in% installed.packages()[,"Package"]
if(all(packages_installes)) {
  cat("\n✓ Tous les packages ont été installés avec succès!\n")
} else {
  cat("\n⚠ Attention: Certains packages n'ont pas été installés:\n")
  print(packages_necessaires[!packages_installes])
}

# Chargement des packages avec gestion d'erreurs améliorée
cat("\n=== CHARGEMENT DES PACKAGES ===\n")
invisible(lapply(packages_necessaires, function(pkg) {
  tryCatch({
    if(!require(pkg, character.only = TRUE)) {
      stop(sprintf("Le package %s n'a pas pu être chargé", pkg))
    }
    cat(sprintf("✓ Package %s chargé avec succès\n", pkg))
  }, error = function(e) {
    cat(sprintf("⚠ Erreur lors du chargement de %s: %s\n", pkg, e$message))
  }, warning = function(w) {
    cat(sprintf("⚠ Avertissement lors du chargement de %s: %s\n", pkg, w$message))
  })
}))

cat("\n=== INSTALLATION ET CHARGEMENT TERMINÉS ===\n")