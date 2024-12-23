# ==============================================================================
#                    CARTE INTERACTIVE AVEC LEAFLET
# ==============================================================================

library(tidyverse)
library(leaflet)
library(htmlwidgets)

# -----------------------------------------------------------------------------
#                    1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------

# Chargement des données GTD nettoyées
data <- read_csv("data/processed/gtd_cleaned.csv") 

# Filtrer les données pour s'assurer des points valides
data <- data %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(
    # Catégorisation du nombre de victimes pour les couleurs
    severity = case_when(
      killed == 0 ~ "Vert",
      killed > 0 & killed <= 5 ~ "Jaune",
      killed > 5 & killed <= 10 ~ "Orange",
      killed > 10 ~ "Rouge"
    )
  )

# -----------------------------------------------------------------------------
#                    2. CRÉATION DE LA CARTE LEAFLET
# -----------------------------------------------------------------------------

# Définition des couleurs
color_palette <- colorFactor(
  palette = c("green", "yellow", "orange", "red"),
  levels = c("Vert", "Jaune", "Orange", "Rouge")
)

# Création de la carte
carte_interactive <- leaflet(data) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% # Fond de carte sombre
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~log(killed + 1) * 2,  # Taille proportionnelle au nombre de victimes
    color = ~color_palette(severity),
    fillOpacity = 0.7,
    popup = ~sprintf(
      "<b>Pays :</b> %s<br>
             <b>Ville :</b> %s<br>
             <b>Année :</b> %d<br>
             <b>Nombre de Victimes :</b> %d<br>
             <b>Type d'Attaque :</b> %s<br>
             <b>Cible :</b> %s<br>",
      country, city, year, killed, attack_type, target_type
    )
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Gravité des Attaques",
    colors = c("green", "yellow", "orange", "red"),
    labels = c("0 Victime", "1-5 Victimes", "6-10 Victimes", ">10 Victimes"),
    opacity = 0.7
  )

# -----------------------------------------------------------------------------
#                    3. SAUVEGARDE DE LA CARTE
# -----------------------------------------------------------------------------

# Sauvegarde en fichier HTML
saveWidget(carte_interactive, "visualizations/interactive_map_victimes.html")

# Affichage dans R
carte_interactive
