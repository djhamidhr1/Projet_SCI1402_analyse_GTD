# ====== 1. chargement des bibliothèques ======

# Chargement des bibliothèques
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(scales)
library(knitr)

# ====== 2. Configuration initiale ======
# Définition du thème global pour ggplot2
theme_set(theme_minimal(base_size = 12))

# Options générales
options(scipen = 999)  # Désactive la notation scientifique
options(dplyr.summarise.inform = FALSE)  # Réduit les messages de dplyr

# ====== 3. Importation et nettoyage des données ======
# Chemin du fichier (à modifier selon votre emplacement)
chemin_fichier <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/raw/globalterrorismdb_0718dist.csv"

# Fonction de chargement des données
load_and_clean_data <- function(file_path) {
  # Lecture du fichier
  data <- read_csv(file_path, 
                   guess_max = 20000, 
                   show_col_types = FALSE)
  
  # Sélection et renommage des colonnes
  data_clean <- data %>%
    select(iyear, imonth, iday, country_txt, region_txt, city, 
           latitude, longitude, attacktype1_txt, nkill, nwound, 
           gname, targtype1_txt, weaptype1_txt, summary) %>%
    rename_with(~ c("Year", "Month", "Day", "Country", "Region", 
                    "City", "Latitude", "Longitude", "Attacktype", 
                    "Kill", "Wound", "Group", "Targettype", 
                    "Weapon", "Summary"))
  
  # Nettoyage des données
  data_clean <- data_clean %>%
    mutate(
      across(c(Kill, Wound), ~replace_na(., 0)),
      across(where(is.character), ~replace_na(., "Unknown")),
      Date = as.Date(sprintf("%d-%02d-%02d", Year, Month, Day)),
      TotalCasualties = Kill + Wound
    )
  
  return(data_clean)
}

# Chargement des données
data <- load_and_clean_data(chemin_fichier)

# ====== 4. Fonctions d'analyse ======
# Fonction pour générer un rapport annuel
generate_yearly_report <- function(data, year) {
  year_data <- data %>% filter(Year == year)
  
  report <- list(
    total_attacks = nrow(year_data),
    total_casualties = sum(year_data$TotalCasualties),
    most_affected_country = year_data %>%
      count(Country) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(Country),
    most_active_group = year_data %>%
      filter(Group != "Unknown") %>%
      count(Group) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(Group),
    primary_attack_type = year_data %>%
      count(Attacktype) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(Attacktype)
  )
  
  return(report)
}

# Fonction pour l'analyse des tendances temporelles
analyze_temporal_trends <- function(data) {
  data %>%
    group_by(Year) %>%
    summarise(
      Attacks = n(),
      Casualties = sum(TotalCasualties),
      UniqueGroups = n_distinct(Group[Group != "Unknown"]),
      .groups = "drop"
    )
}

# ====== 5. Visualisations ======
# Fonction pour créer la visualisation temporelle
plot_temporal_trends <- function(data) {
  trends <- analyze_temporal_trends(data)
  
  p <- ggplot(gather(trends, key = "Metric", value = "Count", -Year),
              aes(x = Year, y = Count, color = Metric)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "Évolution temporelle des attaques terroristes",
         x = "Année",
         y = "Nombre") +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "bottom")
  
  return(ggplotly(p))
}

# Fonction pour créer la carte des attaques
create_attack_map <- function(data, year_range = c(2000, 2017)) {
  data_map <- data %>%
    filter(Year >= year_range[1], 
           Year <= year_range[2],
           !is.na(Latitude), 
           !is.na(Longitude)) %>%
    mutate(
      popup_text = sprintf(
        "<strong>%s</strong><br/>Date: %s<br/>Victimes: %d<br/>Type: %s",
        City, Date, TotalCasualties, Attacktype
      )
    )
  
  leaflet(data_map) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addHeatmap(
      lng = ~Longitude, 
      lat = ~Latitude,
      intensity = ~TotalCasualties,
      radius = 8
    ) %>%
    addCircleMarkers(
      lng = ~Longitude,
      lat = ~Latitude,
      radius = 3,
      popup = ~popup_text,
      color = "red",
      fillOpacity = 0.7,
      group = "Points"
    ) %>%
    addLayersControl(
      baseGroups = c("Points", "Heatmap"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# Fonction pour l'analyse régionale
plot_regional_analysis <- function(data) {
  regional_analysis <- data %>%
    group_by(Region, Attacktype) %>%
    summarise(
      total_attacks = n(),
      total_casualties = sum(TotalCasualties),
      avg_casualties_per_attack = mean(TotalCasualties),
      .groups = "drop"
    ) %>%
    arrange(desc(total_attacks))
  
  plot_ly(regional_analysis, 
          x = ~Region, 
          y = ~total_attacks,
          color = ~Attacktype,
          type = "bar") %>%
    layout(
      title = "Analyse régionale par type d'attaque",
      barmode = "stack",
      xaxis = list(title = "Région"),
      yaxis = list(title = "Nombre d'attaques")
    )
}

# ====== 6. Exécution des analyses ======
# Générer et afficher les visualisations
temporal_plot <- plot_temporal_trends(data)
attack_map <- create_attack_map(data)
regional_plot <- plot_regional_analysis(data)

# Générer un rapport pour 2017
report_2017 <- generate_yearly_report(data, 2017)

# Afficher les résultats
print("=== Rapport 2017 ===")
print(report_2017)

# Sauvegarder les visualisations (optionnel)
htmlwidgets::saveWidget(temporal_plot, "temporal_trends.html")
htmlwidgets::saveWidget(attack_map, "attack_map.html")
htmlwidgets::saveWidget(regional_plot, "regional_analysis.html")

# ====== 7. Analyses supplémentaires ======
# Top 10 des groupes les plus actifs
top_groups <- data %>%
  filter(Group != "Unknown") %>%
  count(Group, sort = TRUE) %>%
  head(10)

# Top 10 des pays les plus touchés
top_countries <- data %>%
  count(Country, sort = TRUE) %>%
  head(10)

# Statistiques sur les types d'attaques
attack_types <- data %>%
  count(Attacktype, sort = TRUE)

# Afficher les résultats supplémentaires
print("=== Top 10 des groupes terroristes ===")
print(top_groups)

print("=== Top 10 des pays les plus touchés ===")
print(top_countries)

print("=== Types d'attaques ===")
print(attack_types)




