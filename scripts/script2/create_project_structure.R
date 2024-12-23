# ==============================================================================
#            SCRIPT DE CRÉATION DE LA STRUCTURE DU PROJET GTD
# ==============================================================================

# Configuration initiale
PROJET_DIR <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD"
setwd(PROJET_DIR)

# Structure complète des dossiers et fichiers
project_structure <- list(
  "data" = c("raw", "processed", "temp"),
  "scripts" = c(
    "01_import_data.R", "02_config_environnement.R",
    "03_nettoyage_data.R", "04_analyse_exploratoire.R",
    "05_analyse_temporelle.R", "06_analyse_spatiale.R",
    "07_analyse_statistique.R", "08_analyse_clustering.R",
    "09_analyse_predictive.R", "10_predictive_temporelle.R",
    "11_prediction_actak.R", "12_analyse_visualisation.R",
    "13_visualisation_interractive.R", "14_visualisation_animées.R"
  ),
  "outputs" = c("figures", "tables", "models"),
  "docs" = c(
    "methodology", "references",
    "rapport_mi_parcours", "rapport_final",
    "templates" = c("plan_projet_template.Rmd", 
                    "rapport_mi_parcours_template.Rmd", 
                    "rapport_final_template.Rmd")
  ),
  "visualizations" = c("maps", "charts", "interactive"),
  "results" = c("analyses", "predictions", "evaluation")
)

# Fonction pour créer un répertoire ou fichier avec gestion d'existence
create_item <- function(path, content = NULL) {
  if (is.null(content)) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      message(sprintf("Dossier créé : %s", path))
    } else {
      message(sprintf("Dossier existant : %s", path))
    }
  } else {
    if (!file.exists(path)) {
      writeLines(content, path, useBytes = TRUE)
      message(sprintf("Fichier créé : %s", path))
    } else {
      message(sprintf("Fichier existant : %s", path))
    }
  }
}

# Fonction pour créer la structure complète
create_project_structure <- function(structure, base_dir = PROJET_DIR) {
  for (name in names(structure)) {
    item <- structure[[name]]
    item_path <- file.path(base_dir, name)
    if (is.character(item)) {
      for (sub_item in item) {
        if (grepl("\\.R|\\.md|\\.Rmd$", sub_item)) {
          create_item(file.path(item_path, sub_item), 
                      ifelse(grepl("\\.R$", sub_item), create_r_script_template(sub_item), ""))
        } else {
          create_item(file.path(item_path, sub_item))
        }
      }
    } else if (is.list(item)) {
      create_project_structure(item, item_path)
    }
  }
}

# Fonction pour générer les templates de scripts R
create_r_script_template <- function(script_name) {
  paste0(
    "# ==============================================================================\n",
    "#            ", gsub("\\.R$", "", script_name), "\n",
    "# ==============================================================================\n\n",
    "# Auteur: HAMID DIGORGORD\n",
    "# Date: ", format(Sys.Date(), "%Y-%m-%d"), "\n\n",
    "# -----------------------------------------------------------------------------\n",
    "#                    1. CONFIGURATION INITIALE\n",
    "# -----------------------------------------------------------------------------\n\n",
    "# Chargement des packages\n",
    "library(tidyverse)\n",
    "library(here)\n\n",
    "# -----------------------------------------------------------------------------\n",
    "#                    2. FONCTIONS PRINCIPALES\n",
    "# -----------------------------------------------------------------------------\n\n",
    "# -----------------------------------------------------------------------------\n",
    "#                    3. EXÉCUTION PRINCIPALE\n",
    "# -----------------------------------------------------------------------------\n\n",
    "main <- function() {\n",
    "    message('Début de l'exécution...')\n",
    "    # Ajouter le code ici\n",
    "}\n\n",
    "# Exécution directe\n",
    "if (!interactive()) main()\n"
  )
}

# Création de la structure
create_project_structure(project_structure)

message("\n=== STRUCTURE DU PROJET CRÉÉE AVEC SUCCÈS ===")
