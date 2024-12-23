# ================================
# 1. CHARGEMENT DES BIBLIOTHÈQUES
# ================================
library(tidyverse)      # Manipulation des données
library(lubridate)      # Manipulation des dates
library(plotly)         # Visualisations interactives
library(viridis)        # Palette de couleurs

# =====================================
# 2. CHARGEMENT ET PRÉPARATION DES DONNÉES
# =====================================
# Spécifier le chemin du fichier
data_path <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv"

# Charger les données
gtd_data <- read.csv(data_path, stringsAsFactors = FALSE)

# Nettoyer les données
gtd_data$nkill <- suppressWarnings(as.numeric(gtd_data$nkill))  # Conversion en numérique
gtd_data$nkill[is.na(gtd_data$nkill)] <- 0                     # Remplacer les NA par 0

# Agréger les données mondiales
gtd_world <- gtd_data %>%
  filter(!is.na(iyear) & !is.na(country_txt)) %>%
  group_by(iyear, country_txt) %>%
  summarise(
    total_attacks = n(),
    total_killed = sum(nkill, na.rm = TRUE)
  ) %>%
  rename(Year = iyear, Country = country_txt) %>%
  ungroup()

# =====================================
# 3. PRÉPARATION DES DONNÉES PAR PAYS
# =====================================
# Données pour le Canada
gtd_canada <- gtd_data %>%
  filter(country_txt == "Canada" & !is.na(provstate)) %>%
  group_by(iyear, provstate) %>%
  summarise(
    total_attacks = n(),
    total_killed = sum(nkill, na.rm = TRUE)
  ) %>%
  rename(Year = iyear, Province = provstate) %>%
  ungroup()

# Données pour la France
gtd_france <- gtd_data %>%
  filter(country_txt == "France" & !is.na(provstate)) %>%
  group_by(iyear, provstate) %>%
  summarise(
    total_attacks = n(),
    total_killed = sum(nkill, na.rm = TRUE)
  ) %>%
  rename(Year = iyear, Region = provstate) %>%
  ungroup()

# Compléter les données pour le Canada et la France
gtd_canada$total_attacks[is.na(gtd_canada$total_attacks)] <- 0
gtd_france$total_attacks[is.na(gtd_france$total_attacks)] <- 0

# =========================================
# 4. CRÉATION DES CARTES CHOROPLÈTHES
# =========================================

# Palette de couleurs personnalisée
color_scale <- list(
  colors = c("blue", "green", "yellow", "orange", "red")
)

# Carte mondiale
fig_world <- plot_ly(
  gtd_world,
  type = "choropleth",
  locations = ~Country,
  locationmode = "country names",
  z = ~total_attacks,
  frame = ~Year,
  text = ~paste("Pays:", Country, "<br>Total Attaques:", total_attacks, "<br>Total Tués:", total_killed),
  hoverinfo = "text",
  colors = color_scale$colors
) %>%
  layout(
    title = "Évolution des attaques terroristes dans le monde",
    geo = list(
      projection = list(type = "equirectangular"),
      showland = TRUE,
      landcolor = "white",
      showocean = TRUE,
      oceancolor = "lightblue"
    )
  )

# Carte pour le Canada
fig_canada <- plot_ly(
  gtd_canada,
  type = "choropleth",
  locations = ~Province,
  locationmode = "country names",
  z = ~total_attacks,
  frame = ~Year,
  text = ~paste("Province:", Province, "<br>Total Attaques:", total_attacks, "<br>Total Tués:", total_killed),
  hoverinfo = "text",
  colors = color_scale$colors
) %>%
  layout(
    title = "Évolution des attaques terroristes au Canada",
    geo = list(
      scope = "north america",
      showlakes = TRUE,
      lakecolor = "lightblue"
    )
  )

# Carte pour la France
fig_france <- plot_ly(
  gtd_france,
  type = "choropleth",
  locations = ~Region,
  locationmode = "country names",
  z = ~total_attacks,
  frame = ~Year,
  text = ~paste("Région:", Region, "<br>Total Attaques:", total_attacks, "<br>Total Tués:", total_killed),
  hoverinfo = "text",
  colors = color_scale$colors
) %>%
  layout(
    title = "Évolution des attaques terroristes en France",
    geo = list(
      scope = "europe",
      showlakes = TRUE,
      lakecolor = "lightblue"
    )
  )
