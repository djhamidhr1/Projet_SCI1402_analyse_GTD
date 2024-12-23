# ==============================================================================
#                    CARTES INTERACTIVES RÉGIONALES GTD
# ==============================================================================

library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(scales)
library(sf)
library(viridis)
library(htmlwidgets)

# -----------------------------------------------------------------------------
#                    1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------

# Configuration des régions
REGIONS_CONFIG <- list(
    south_america = list(
        name = "South America",
        color = "#2ecc71",
        bounds = c(-100, -60, -20, 25)
    ),
    north_america = list(
        name = "North America",
        color = "#3498db",
        bounds = c(-170, 0, -55, 75)
    ),
    mena = list(
        name = "Middle East & North Africa",
        color = "#e74c3c",
        bounds = c(-35, 0, 65, 60)
    ),
    subsaharan = list(
        name = "Sub-Saharan Africa",
        color = "#f1c40f",
        bounds = c(-35, -45, 65, 60)
    ),
    europe = list(
        name = c("Eastern Europe", "Western Europe"),
        color = "#9b59b6",
        bounds = c(-15, 10, 70, 75)
    ),
    asia = list(
        name = c("South Asia", "Southeast Asia", "Central Asia", "East Asia"),
        color = "#1abc9c",
        bounds = c(30, -15, 165, 70)
    )
)

# Palettes de couleurs améliorées
COLORS <- list(
    attack = colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))(5),
    region = viridis(8, alpha = 0.8),
    severity = c(
        Faible = "#2ecc71",
        Moyen = "#f1c40f",
        Élevé = "#e74c3c"
    )
)

# -----------------------------------------------------------------------------
#                    2. FONCTIONS DE PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------

prepare_data <- function(data, region_name) {
    data %>%
        filter(region_txt %in% region_name,
               !is.na(longitude),
               !is.na(latitude)) %>%
        mutate(
            killed_cat = cut(nkill,
                           breaks = c(-Inf, 0, 5, 10, 25, Inf),
                           labels = c("0", "1-5", "6-10", "11-25", ">25")),
            year = iyear,
            popup_text = sprintf(
                "<div class='popup-content'>
                    <h3>Détails de l'incident</h3>
                    <p><b>Année:</b> %d</p>
                    <p><b>Pays:</b> %s</p>
                    <p><b>Ville:</b> %s</p>
                    <p><b>Morts:</b> %d</p>
                    <p><b>Blessés:</b> %d</p>
                    <p><b>Type d'attaque:</b> %s</p>
                </div>",
                iyear, country_txt, city, nkill, nwound, attacktype1_txt
            )
        )
}

# -----------------------------------------------------------------------------
#                    3. FONCTION DE CRÉATION DE CARTE RÉGIONALE
# -----------------------------------------------------------------------------

create_region_map <- function(data, region_config) {
    region_data <- prepare_data(data, region_config$name)
    
    leaflet(region_data) %>%
        # Fond de carte
        addProviderTiles(
            providers$CartoDB.Positron,
            options = providerTileOptions(minZoom = 2)
        ) %>%
        # Ajustement des limites
        fitBounds(
            region_config$bounds[1],
            region_config$bounds[2],
            region_config$bounds[3],
            region_config$bounds[4]
        ) %>%
        # Marqueurs des incidents
        addCircleMarkers(
            lng = ~longitude,
            lat = ~latitude,
            radius = ~sqrt(nkill + 1) * 3,
            color = region_config$color,
            fillOpacity = 0.7,
            stroke = FALSE,
            popup = ~popup_text,
            clusterOptions = markerClusterOptions(
                iconCreateFunction = JS("
                    function(cluster) {
                        return L.divIcon({
                            html: '<div style=\"
                                background-color: rgba(70, 130, 180, 0.8);
                                width: 40px;
                                height: 40px;
                                border-radius: 20px;
                                display: flex;
                                align-items: center;
                                justify-content: center;
                                color: white;
                                font-weight: bold;
                            \">' + cluster.getChildCount() + '</div>',
                            className: 'marker-cluster',
                            iconSize: L.point(40, 40)
                        });
                    }
                ")
            )
        ) %>%
        # Légende
        addLegend(
            position = "bottomright",
            title = "Nombre de victimes",
            colors = COLORS$attack,
            labels = c("0", "1-5", "6-10", "11-25", ">25"),
            opacity = 0.7
        )
}

# -----------------------------------------------------------------------------
#                    4. FONCTION DE CARTE MONDIALE
# -----------------------------------------------------------------------------

create_global_severity_map <- function(data) {
    prepared_data <- data %>%
        filter(!is.na(longitude), !is.na(latitude)) %>%
        mutate(
            severity = case_when(
                nkill <= 5 ~ "Faible",
                nkill <= 10 ~ "Moyen",
                TRUE ~ "Élevé"
            ),
            severity_color = COLORS$severity[severity],
            popup_text = sprintf(
                "<div class='popup-content'>
                    <h3>Détails de l'incident</h3>
                    <p><b>Année:</b> %d</p>
                    <p><b>Pays:</b> %s</p>
                    <p><b>Gravité:</b> %s</p>
                    <p><b>Morts:</b> %d</p>
                    <p><b>Blessés:</b> %d</p>
                    <p><b>Type d'attaque:</b> %s</p>
                </div>",
                iyear, country_txt, severity, nkill, nwound, attacktype1_txt
            )
        )
    
    leaflet(prepared_data) %>%
        addProviderTiles(
            providers$CartoDB.Positron,
            options = providerTileOptions(minZoom = 2)
        ) %>%
        addCircleMarkers(
            lng = ~longitude,
            lat = ~latitude,
            radius = ~sqrt(nkill + 1) * 2,
            color = ~severity_color,
            fillOpacity = 0.7,
            stroke = FALSE,
            popup = ~popup_text,
            clusterOptions = markerClusterOptions()
        ) %>%
        addLegend(
            position = "bottomright",
            colors = unname(COLORS$severity),
            labels = names(COLORS$severity),
            title = "Gravité des attaques",
            opacity = 0.7
        )
}

# -----------------------------------------------------------------------------
#                    5. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Création du dossier pour les cartes
    dir.create("visualizations/maps", recursive = TRUE, showWarnings = FALSE)
    
    # Chargement des données
    message("Chargement des données...")
    data <- read_csv("data/processed/gtd_cleaned.csv")
    
    # Création des cartes régionales
    message("Création des cartes régionales...")
    maps <- list()
    
    for(region_name in names(REGIONS_CONFIG)) {
        message(sprintf("Création de la carte pour %s...", region_name))
        maps[[region_name]] <- create_region_map(data, REGIONS_CONFIG[[region_name]])
        saveWidget(maps[[region_name]], 
                  sprintf("visualizations/maps/%s_map.html", region_name))
    }
    
    # Création de la carte mondiale
    message("Création de la carte mondiale...")
    global_map <- create_global_severity_map(data)
    saveWidget(global_map, "visualizations/maps/global_map.html")
    
    message("Toutes les cartes ont été générées avec succès!")
    return(maps)
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}
