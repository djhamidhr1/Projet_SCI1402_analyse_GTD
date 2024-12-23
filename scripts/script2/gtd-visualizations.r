# ==============================================================================
#                    VISUALISATIONS DÉTAILLÉES DES DONNÉES GTD
# ==============================================================================

# Chargement des packages nécessaires
library(tidyverse)
library(corrplot)
library(gridExtra)
library(scales)
library(viridis)
library(maps)

# -----------------------------------------------------------------------------
#                    1. DISTRIBUTIONS ET TENDANCES GÉNÉRALES
# -----------------------------------------------------------------------------

plot_attack_distributions <- function(data) {
  # Distribution par type d'attaque
  p1 <- ggplot(data, aes(x = reorder(attack_type, attack_type, function(x) length(x)))) +
    geom_bar(aes(fill = ..count..)) +
    scale_fill_viridis() +
    coord_flip() +
    theme_minimal() +
    labs(title = "Distribution des Types d'Attaques",
         x = "Type d'attaque",
         y = "Nombre d'incidents")
  
  # Évolution temporelle par type d'attaque
  p2 <- data %>%
    count(year, attack_type) %>%
    ggplot(aes(x = year, y = n, color = attack_type)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Évolution des Types d'Attaques dans le Temps",
         x = "Année",
         y = "Nombre d'incidents",
         color = "Type d'attaque")
  
  # Combinaison des graphiques
  grid.arrange(p1, p2, ncol = 1)
}

# -----------------------------------------------------------------------------
#                    2. ANALYSE GÉOGRAPHIQUE
# -----------------------------------------------------------------------------

plot_geographical_analysis <- function(data) {
  # Distribution par région
  p1 <- ggplot(data, aes(x = reorder(region, region, function(x) length(x)))) +
    geom_bar(aes(fill = ..count..)) +
    scale_fill_viridis() +
    coord_flip() +
    theme_minimal() +
    labs(title = "Nombre d'Incidents par Région",
         x = "Région",
         y = "Nombre d'incidents")
  
  # Carte des incidents
  world <- map_data("world")
  p2 <- ggplot() +
    geom_map(data = world, map = world,
             aes(long, lat, map_id = region),
             color = "white", fill = "lightgray") +
    geom_point(data = data,
               aes(x = longitude, y = latitude, color = killed),
               alpha = 0.5) +
    scale_color_viridis() +
    theme_minimal() +
    labs(title = "Distribution Géographique des Incidents",
         color = "Nombre de victimes")
  
  # Combinaison des graphiques
  grid.arrange(p1, p2, ncol = 1)
}

# -----------------------------------------------------------------------------
#                    3. ANALYSE DES VICTIMES
# -----------------------------------------------------------------------------

plot_casualty_analysis <- function(data) {
  # Distribution des victimes
  p1 <- ggplot(data, aes(x = killed)) +
    geom_histogram(bins = 50, fill = "darkred", alpha = 0.7) +
    scale_x_log10() +
    theme_minimal() +
    labs(title = "Distribution du Nombre de Victimes (échelle log)",
         x = "Nombre de victimes",
         y = "Fréquence")
  
  # Victimes par région
  p2 <- data %>%
    group_by(region) %>%
    summarize(total_killed = sum(killed, na.rm = TRUE)) %>%
    ggplot(aes(x = reorder(region, total_killed), y = total_killed)) +
    geom_col(fill = "darkred", alpha = 0.7) +
    coord_flip() +
    theme_minimal() +
    labs(title = "Nombre Total de Victimes par Région",
         x = "Région",
         y = "Nombre total de victimes")
  
  # Combinaison des graphiques
  grid.arrange(p1, p2, ncol = 1)
}

# -----------------------------------------------------------------------------
#                    4. ANALYSE DE CORRÉLATION
# -----------------------------------------------------------------------------

plot_correlation_analysis <- function(data) {
  # Préparation des données numériques
  numeric_data <- data %>%
    select(killed, wounded, year) %>%
    cor(use = "complete.obs")
  
  # Matrice de corrélation
  corrplot(numeric_data,
           method = "color",
           type = "upper",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           title = "Matrice de Corrélation")
  
  # Relation entre tués et blessés
  ggplot(data, aes(x = killed, y = wounded)) +
    geom_point(alpha = 0.5, color = "darkred") +
    geom_smooth(method = "lm", color = "blue") +
    scale_x_log10() +
    scale_y_log10() +
    theme_minimal() +
    labs(title = "Relation entre Nombre de Tués et de Blessés",
         x = "Nombre de tués (log)",
         y = "Nombre de blessés (log)")
}

# -----------------------------------------------------------------------------
#                    5. ANALYSE TEMPORELLE
# -----------------------------------------------------------------------------

plot_temporal_analysis <- function(data) {
  # Tendance annuelle générale
  data %>%
    count(year) %>%
    ggplot(aes(x = year, y = n)) +
    geom_line(color = "darkred") +
    geom_smooth(method = "loess", color = "blue") +
    theme_minimal() +
    labs(title = "Évolution du Nombre d'Incidents par Année",
         x = "Année",
         y = "Nombre d'incidents")
}

# -----------------------------------------------------------------------------
#                    6. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
  # Chargement des données
  data_path <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv"
  
  # Lecture des données
  if (file.exists(data_path)) {
    data <- read_csv(data_path)
    message("Données chargées avec succès!")
  } else {
    stop("Le fichier gtd_clean.csv est introuvable. Vérifiez le chemin.")
  }
  
  # Création des visualisations
  message("\n=== CRÉATION DES VISUALISATIONS ===")
  
  message("\n1. Graphiques de distribution...")
  plot_attack_distributions(data)
  
  message("2. Graphiques géographiques...")
  plot_geographical_analysis(data)
  
  message("3. Graphiques des victimes...")
  plot_casualty_analysis(data)
  
  message("4. Graphiques de corrélation...")
  plot_correlation_analysis(data)
  
  message("5. Graphiques temporels...")
  plot_temporal_analysis(data)
  
  message("\n=== VISUALISATIONS TERMINÉES ===")
}

# Exécution du script en mode non-interactif
if (!interactive()) {
  main()
}