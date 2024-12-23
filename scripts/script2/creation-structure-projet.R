# ==============================================================================
#            SCRIPT DE CRÉATION DE LA STRUCTURE DU PROJET GTD
# ==============================================================================

# Configuration
PROJET_DIR <- getwd()

# Structure complète des dossiers
project_structure <- list(
  "data" = c(
    "raw",
    "processed",
    "temp"
  ),
  "scripts" = c(
    "01_import_data.R",
    "02_config_environnement.R",
    "03_nettoyage_data.R",
    "04_analyse_exploratoire.R",
    "05_analyse_temporelle.R",
    "06_analyse_spatiale.R",
    "07_analyse_statistique.R",
    "08_analyse_clustering.R",
    "09_analyse_predictive.R",
    "10_predictive_temporelle.R",
    "11_prediction_actak.R",
    "12_analyse_visualisation.R",
    "13_visualisation_interractive.R",
    "14_visualisation_animées.R"
  ),
  "outputs" = c(
    "figures",
    "tables",
    "models"
  ),
  "docs" = c(
    "methodology",
    "references",
    "rapport_mi_parcours",
    "rapport_final"
  ),
  "visualizations" = c(
    "maps",
    "charts",
    "interactive"
  ),
  "results" = c(
    "analyses",
    "predictions",
    "evaluation"
  )
)

# Fonction pour créer la structure
create_project_structure <- function() {
  message("Création de la structure du projet GTD...")
  
  # Création des dossiers principaux et sous-dossiers
  for (main_dir in names(project_structure)) {
    # Création du dossier principal
    main_path <- file.path(PROJET_DIR, main_dir)
    dir.create(main_path, recursive = TRUE, showWarnings = FALSE)
    
    # Création des sous-dossiers ou fichiers
    for (sub_item in project_structure[[main_dir]]) {
      sub_path <- file.path(main_path, sub_item)
      
      # Si c'est un fichier .R, le créer avec un template
      if (grepl("\\.R$", sub_item)) {
        if (!file.exists(sub_path)) {
          writeLines(
            paste0(
              "# ", gsub("\\.R$", "", sub_item), "\n",
              "# Auteur: [Votre nom]\n",
              "# Date: ", format(Sys.Date(), "%Y-%m-%d"), "\n\n",
              "# Chargement des packages\n",
              "library(tidyverse)\n\n",
              "# Configuration\n\n",
              "# Code principal\n\n",
              "# Exécution\n"
            ),
            sub_path
          )
          message("Fichier script créé : ", sub_path)
        }
      } else {
        # Sinon créer un dossier
        dir.create(sub_path, recursive = TRUE, showWarnings = FALSE)
        message("Dossier créé : ", sub_path)
      }
    }
  }
  
  # Création du README.md
  readme_content <- paste0(
    "# Projet d'analyse GTD (Global Terrorism Database)\n\n",
    "## Structure du projet\n",
    "```\n",
    "Projet_SCI1402_analyse_GTD/\n",
    "├── data/\n",
    "│   ├── raw/            # Données brutes\n",
    "│   ├── processed/      # Données nettoyées\n",
    "│   └── temp/          # Fichiers temporaires\n",
    "├── scripts/\n",
    "│   ├── 01_import_data.R\n",
    "│   ├── 02_config_environnement.R\n",
    "│   ├── 03_nettoyage_data.R\n",
    "│   ├── 04_analyse_exploratoire.R\n",
    "│   ├── 05_analyse_temporelle.R\n",
    "│   ├── 06_analyse_spatiale.R\n",
    "│   ├── 03_nettoyage_data.R\n",
    "│   ├── 04_analyse_exploratoire.R\n",
    "│   ├── 05_analyse_temporelle.R\n",
    "│   ├── 06_analyse_spatiale.R\n",
    "│   ├── 03_nettoyage_data.R\n",
    "│   ├── 04_analyse_exploratoire.R\n",
    "│   ├── 05_analyse_temporelle.R\n",
    "│   ├── 06_analyse_spatiale.R\n",
    "│   └── 07_analyse_predictive.R\n",
    "├── outputs/\n",
    "│   ├── figures/       # Graphiques\n",
    "│   ├── tables/       # Tableaux de résultats\n",
    "│   └── models/       # Modèles sauvegardés\n",
    "├── docs/\n",
    "│   ├── methodology/   # Documentation méthodologique\n",
    "│   ├── references/    # Sources et références\n",
    "│   └── rapports/     # Rapports mi-parcours et final\n",
    "└── README.md\n",
    "```\n\n",
    "## Installation\n",
    "1. Cloner le repository\n",
    "2. Placer le fichier GTD dans data/raw/\n",
    "3. Exécuter les scripts dans l'ordre\n\n",
    "## Utilisation\n",
    "Suivre les scripts dans l'ordre numérique.\n"
  )
  writeLines(readme_content, file.path(PROJET_DIR, "README.md"))
  
  message("\nStructure du projet créée avec succès!")
}

# Exécution
tryCatch({
  create_project_structure()
  message("\nVérifiez que le fichier GTD est bien dans data/raw/")
}, error = function(e) {
  message("Erreur lors de la création de la structure : ", e$message)
})

# Affichage de la structure créée
message("\nStructure du projet :")
list.dirs(PROJET_DIR, recursive = TRUE)