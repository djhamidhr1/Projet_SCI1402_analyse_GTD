# ==============================================================================
#                    VISUALISATIONS INTERACTIVES DES DONNÉES GTD
# ==============================================================================

install.packages(c("tidyverse", "plotly", "leaflet", "viridis", "DT", "htmlwidgets"))


# Chargement des packages nécessaires
library(tidyverse)
library(plotly)
library(leaflet)
library(viridis)
library(DT)
library(htmlwidgets)

# -----------------------------------------------------------------------------
#                    1. CHARGEMENT DES DONNÉES
# -----------------------------------------------------------------------------

load_gtd_data <- function() {
  message("Chargement des données...")
  input_path <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv"
  
  if (!file.exists(input_path)) {
    stop(paste("Erreur : Le fichier 'gtd_clean.csv' est introuvable au chemin :", input_path))
  }
  
  data <- read_csv(input_path, show_col_types = FALSE)
  message("Données chargées avec succès !")
  return(data)
}

# -----------------------------------------------------------------------------
#                    2. CARTE INTERACTIVE DES INCIDENTS
# -----------------------------------------------------------------------------

create_interactive_map <- function(data) {
  message("Création de la carte interactive...")
  
  map_data <- data %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    mutate(
      popup_text = sprintf(
        "<strong>Pays:</strong> %s<br>
        <strong>Ville:</strong> %s<br>
        <strong>Date:</strong> %s<br>
        <strong>Type d'attaque:</strong> %s<br>
        <strong>Victimes:</strong> %d",
        country, city, date, attack_type, killed
      )
    )
  
  map <- leaflet(map_data) %>%
    addTiles() %>%
    addCircleMarkers(
      ~longitude, ~latitude,
      popup = ~popup_text,
      radius = ~sqrt(killed + 1) * 3,
      color = "red",
      fillOpacity = 0.6,
      clusterOptions = markerClusterOptions()
    )
  
  saveWidget(map, "visualizations/interactive_map.html")
  return(map)
}

# -----------------------------------------------------------------------------
#                    3. TENDANCES TEMPORELLES INTERACTIVES
# -----------------------------------------------------------------------------

create_temporal_trends <- function(data) {
  message("Création des tendances temporelles interactives...")
  
  yearly_trend <- data %>%
    count(year) %>%
    plot_ly(x = ~year, y = ~n, type = 'scatter', mode = 'lines+markers') %>%
    layout(title = 'Évolution Annuelle des Incidents',
           xaxis = list(title = 'Année'), yaxis = list(title = 'Nombre d\'incidents'))
  
  saveWidget(yearly_trend, "visualizations/yearly_trend.html")
  return(yearly_trend)
}

# -----------------------------------------------------------------------------
#                    4. DISTRIBUTIONS INTERACTIVES
# -----------------------------------------------------------------------------

create_interactive_distributions <- function(data) {
  message("Création des distributions interactives...")
  
  attack_dist <- data %>%
    count(attack_type) %>%
    plot_ly(x = ~reorder(attack_type, n), y = ~n, type = 'bar') %>%
    layout(title = 'Distribution des Types d\'Attaques',
           xaxis = list(title = 'Type d\'attaque'), yaxis = list(title = 'Nombre d\'incidents'))
  
  saveWidget(attack_dist, "visualizations/attack_distribution.html")
  return(attack_dist)
}

# -----------------------------------------------------------------------------
#                    5. HEATMAP INTERACTIVE
# -----------------------------------------------------------------------------

create_interactive_heatmap <- function(data) {
  message("Création des heatmaps interactives...")
  
  heatmap <- data %>%
    count(region, attack_type) %>%
    plot_ly(x = ~region, y = ~attack_type, z = ~n, type = "heatmap", colors = viridis(100)) %>%
    layout(title = 'Heatmap des Attaques par Région et Type',
           xaxis = list(title = 'Région'), yaxis = list(title = 'Type d\'attaque'))
  
  saveWidget(heatmap, "visualizations/region_type_heatmap.html")
  return(heatmap)
}

# -----------------------------------------------------------------------------
#                    6. TABLEAU INTERACTIF
# -----------------------------------------------------------------------------

create_interactive_table <- function(data) {
  message("Création des tableaux interactifs...")
  
  summary_table <- data %>%
    group_by(region) %>%
    summarise(
      incidents = n(),
      total_killed = sum(killed, na.rm = TRUE),
      avg_killed = mean(killed, na.rm = TRUE),
      total_wounded = sum(wounded, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(incidents))
  
  table <- datatable(summary_table, options = list(pageLength = 10, scrollX = TRUE))
  saveWidget(table, "visualizations/interactive_table.html")
  return(table)
}

# -----------------------------------------------------------------------------
#                    7. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
  # Création du dossier pour les visualisations
  dir.create("visualizations", showWarnings = FALSE)
  
  # Chargement des données
  data <- load_gtd_data()
  
  # Génération des visualisations
  create_interactive_map(data)
  create_temporal_trends(data)
  create_interactive_distributions(data)
  create_interactive_heatmap(data)
  create_interactive_table(data)
  
  message("\nToutes les visualisations ont été générées dans le dossier 'visualizations'!")
}

# -----------------------------------------------------------------------------
#                    8. EXÉCUTION DU SCRIPT
# -----------------------------------------------------------------------------

if (!interactive()) {
  main()
}
