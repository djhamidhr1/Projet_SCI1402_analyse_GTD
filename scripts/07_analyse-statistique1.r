# ==============================================================================
#                   ANALYSE STATISTIQUE DES DONNÉES GTD
# ==============================================================================

# ------------------------------ #
# 1. CHARGEMENT DES LIBRAIRIES
# ------------------------------ #
suppressPackageStartupMessages({
  library(tidyverse)    # Manipulation et visualisation des données
  library(ggstatsplot)  # Visualisations statistiques
  library(corrplot)     # Matrice de corrélation
  library(lubridate)    # Manipulation des dates
  library(tsibble)      # Séries temporelles
  library(feasts)       # Décomposition des séries temporelles
  library(broom)        # Résumé des modèles statistiques
  library(scales)       # Mise en forme des axes
  library(gridExtra)    # Affichage multiple
  library(here)         # Gestion des chemins de fichiers
})

# ------------------------------ #
# 2. CONFIGURATION GÉNÉRALE
# ------------------------------ #

# Configuration du thème général pour ggplot2
theme_set(theme_minimal() +
            theme(
              text = element_text(family = "Arial", size = 11),
              plot.title = element_text(size = 14, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)
            ))

# Création des dossiers de sortie
output_dir <- here("outputs")
fig_dir <- file.path(output_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------ #
# 3. FONCTIONS UTILITAIRES
# ------------------------------ #

#' Fonction pour sauvegarder les graphiques de manière standardisée
#' @param plot Objet ggplot à sauvegarder
#' @param filename Nom du fichier
#' @param width Largeur en pouces
#' @param height Hauteur en pouces
save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  message(sprintf("✅ Graphique sauvegardé : %s", filename))
}

#' Fonction pour générer un rapport statistique descriptif
#' @param data Dataframe à analyser
#' @param numeric_vars Vecteur des variables numériques
generate_summary_stats <- function(data, numeric_vars) {
  data %>%
    summarise(across(
      all_of(numeric_vars),
      list(
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        n_missing = ~sum(is.na(.))
      )
    )) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", "statistic"),
      names_sep = "_"
    ) %>%
    pivot_wider(
      names_from = statistic,
      values_from = value
    )
}

# ------------------------------ #
# 4. CHARGEMENT DES DONNÉES
# ------------------------------ #
load_gtd_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("❌ ERREUR : Le fichier spécifié n'existe pas.")
  }
  
  data <- read_csv(file_path, show_col_types = FALSE) %>%
    rename(
      attack_type = attacktype1_txt,
      killed = nkill,
      wounded = nwound,
      region_text = region_txt,
      year = iyear
    )
  
  # Validation des données
  assertthat::assert_that(
    all(c("attack_type", "killed", "wounded", "region_text", "year") %in% names(data)),
    msg = "❌ ERREUR : Colonnes manquantes dans le jeu de données"
  )
  
  message("✅ Données chargées avec succès")
  return(data)
}

# ------------------------------ #
# 5. ANALYSES STATISTIQUES
# ------------------------------ #

#' Analyse temporelle des incidents
#' @param data Données GTD
analyze_temporal_trends <- function(data) {
  yearly_trends <- data %>%
    count(year) %>%
    mutate(date = as.Date(paste0(year, "-01-01")))
  
  p <- ggplot(yearly_trends, aes(x = date, y = n)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(
      title = "Évolution temporelle des incidents terroristes",
      x = "Année",
      y = "Nombre d'incidents"
    )
  
  save_plot(p, "temporal_trends.png")
  return(p)
}

#' Analyse géographique des incidents
#' @param data Données GTD
analyze_geographic_distribution <- function(data) {
  p <- data %>%
    count(region_text) %>%
    mutate(region_text = fct_reorder(region_text, n)) %>%
    ggplot(aes(x = region_text, y = n)) +
    geom_bar(stat = "identity", fill = "orange") +
    coord_flip() +
    labs(
      title = "Distribution géographique des incidents",
      x = "Région",
      y = "Nombre d'incidents"
    )
  
  save_plot(p, "geographic_distribution.png")
  return(p)
}

#' Analyse des types d'attaques
#' @param data Données GTD
analyze_attack_types <- function(data) {
  p <- ggbetweenstats(
    data = data,
    x = attack_type,
    y = killed,
    title = "Distribution des victimes par type d'attaque",
    xlab = "Type d'attaque",
    ylab = "Nombre de victimes"
  )
  
  save_plot(p, "attack_types_distribution.png")
  return(p)
}

#' Analyse des corrélations
#' @param data Données GTD
analyze_correlations <- function(data) {
  numeric_vars <- c("killed", "wounded", "latitude", "longitude")
  corr_data <- data %>%
    select(all_of(numeric_vars)) %>%
    drop_na()
  
  corr_matrix <- cor(corr_data)
  corr_test <- cor.mtest(corr_data)
  
  png(file.path(fig_dir, "correlation_matrix.png"),
      width = 8, height = 8, units = "in", res = 300)
  corrplot(corr_matrix,
           method = "color",
           type = "upper",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           p.mat = corr_test$p,
           sig.level = 0.05)
  dev.off()
  
  message("✅ Matrice de corrélation sauvegardée")
}

# ------------------------------ #
# 6. EXÉCUTION PRINCIPALE
# ------------------------------ #
main <- function() {
  # Définition du chemin des données
  data_path <- here("data", "processed", "gtd_clean.csv")
  
  # Chargement des données
  gtd_data <- load_gtd_data(data_path)
  
  # Analyses
  analyze_temporal_trends(gtd_data)
  analyze_geographic_distribution(gtd_data)
  analyze_attack_types(gtd_data)
  analyze_correlations(gtd_data)
  
  # Statistiques descriptives
  summary_stats <- generate_summary_stats(
    gtd_data,
    c("killed", "wounded", "latitude", "longitude")
  )
  
  # Sauvegarde des statistiques descriptives
  write_csv(summary_stats, file.path(output_dir, "summary_statistics.csv"))
  
  message("\n✅ ANALYSE TERMINÉE")
  message("Les résultats ont été sauvegardés dans : ", output_dir)
}

# Exécution du script
if (!interactive()) {
  main()
}