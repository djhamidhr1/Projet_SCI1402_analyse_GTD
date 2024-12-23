# ==============================================================================
#                    VISUALISATIONS INTERACTIVES DES DONNÉES GTD
# ==============================================================================

library(tidyverse)
library(plotly)
library(leaflet)
library(viridis)
library(DT)
library(htmlwidgets)

# -----------------------------------------------------------------------------
#                    1. CARTE INTERACTIVE DES INCIDENTS
# -----------------------------------------------------------------------------

create_interactive_map <- function(data) {
    message("Création de la carte interactive...")
    
    # Préparation des données pour la carte
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

    # Création de la carte
    map <- leaflet(map_data) %>%
        addTiles() %>%
        addCircleMarkers(
            ~longitude, ~latitude,
            popup = ~popup_text,
            radius = ~sqrt(killed + 1) * 3,
            color = "red",
            fillOpacity = 0.6,
            clusterOptions = markerClusterOptions()
        ) %>%
        addLegend(
            position = "bottomright",
            title = "Nombre de victimes",
            colors = c("red"),
            labels = c("Taille proportionnelle au nombre de victimes")
        )
    
    # Sauvegarde de la carte
    saveWidget(map, "visualizations/interactive_map.html")
    return(map)
}

# -----------------------------------------------------------------------------
#                    2. TENDANCES TEMPORELLES INTERACTIVES
# -----------------------------------------------------------------------------

create_temporal_trends <- function(data) {
    message("Création des tendances temporelles interactives...")
    
    # Tendance annuelle des incidents
    yearly_trend <- data %>%
        count(year) %>%
        plot_ly(x = ~year, y = ~n, type = 'scatter', mode = 'lines+markers',
                name = 'Incidents') %>%
        layout(title = 'Évolution Annuelle des Incidents',
               xaxis = list(title = 'Année'),
               yaxis = list(title = 'Nombre d\'incidents'))
    
    # Distribution mensuelle par année
    monthly_trend <- data %>%
        count(year, month) %>%
        plot_ly(x = ~month, y = ~n, color = ~factor(year),
                type = 'scatter', mode = 'lines',
                name = ~paste('Année', year)) %>%
        layout(title = 'Distribution Mensuelle des Incidents',
               xaxis = list(title = 'Mois'),
               yaxis = list(title = 'Nombre d\'incidents'))
    
    saveWidget(yearly_trend, "visualizations/yearly_trend.html")
    saveWidget(monthly_trend, "visualizations/monthly_trend.html")
    
    return(list(yearly = yearly_trend, monthly = monthly_trend))
}

# -----------------------------------------------------------------------------
#                    3. DISTRIBUTIONS INTERACTIVES
# -----------------------------------------------------------------------------

create_interactive_distributions <- function(data) {
    message("Création des distributions interactives...")
    
    # Distribution des types d'attaques
    attack_dist <- data %>%
        count(attack_type) %>%
        plot_ly(x = ~reorder(attack_type, n), y = ~n,
                type = 'bar',
                marker = list(color = viridis(n()))) %>%
        layout(title = 'Distribution des Types d\'Attaques',
               xaxis = list(title = 'Type d\'attaque',
                          categoryorder = "total descending"),
               yaxis = list(title = 'Nombre d\'incidents'))
    
    # Box plot des victimes par région
    casualties_box <- plot_ly(data, y = ~killed, color = ~region,
                            type = "box") %>%
        layout(title = 'Distribution des Victimes par Région',
               xaxis = list(title = 'Région'),
               yaxis = list(title = 'Nombre de victimes',
                          type = 'log'))
    
    saveWidget(attack_dist, "visualizations/attack_distribution.html")
    saveWidget(casualties_box, "visualizations/casualties_boxplot.html")
    
    return(list(attacks = attack_dist, casualties = casualties_box))
}

# -----------------------------------------------------------------------------
#                    4. HEATMAPS INTERACTIVES
# -----------------------------------------------------------------------------

create_interactive_heatmaps <- function(data) {
    message("Création des heatmaps interactives...")
    
    # Heatmap des attaques par région et type
    attack_region_heatmap <- data %>%
        count(region, attack_type) %>%
        plot_ly(x = ~region, y = ~attack_type, z = ~n,
                type = "heatmap",
                colors = viridis(100)) %>%
        layout(title = 'Distribution des Attaques par Région et Type',
               xaxis = list(title = 'Région'),
               yaxis = list(title = 'Type d\'attaque'))
    
    # Heatmap temporelle (année/mois)
    temporal_heatmap <- data %>%
        count(year, month) %>%
        plot_ly(x = ~year, y = ~month, z = ~n,
                type = "heatmap",
                colors = viridis(100)) %>%
        layout(title = 'Distribution Temporelle des Incidents',
               xaxis = list(title = 'Année'),
               yaxis = list(title = 'Mois'))
    
    saveWidget(attack_region_heatmap, "visualizations/region_type_heatmap.html")
    saveWidget(temporal_heatmap, "visualizations/temporal_heatmap.html")
    
    return(list(region_type = attack_region_heatmap,
                temporal = temporal_heatmap))
}

# -----------------------------------------------------------------------------
#                    5. TABLEAUX INTERACTIFS
# -----------------------------------------------------------------------------

create_interactive_tables <- function(data) {
    message("Création des tableaux interactifs...")
    
    # Résumé par région
    region_summary <- data %>%
        group_by(region) %>%
        summarise(
            incidents = n(),
            total_killed = sum(killed, na.rm = TRUE),
            avg_killed = mean(killed, na.rm = TRUE),
            total_wounded = sum(wounded, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(desc(incidents))
    
    # Table interactive
    table <- datatable(region_summary,
                      options = list(pageLength = 10,
                                   scrollX = TRUE),
                      rownames = FALSE) %>%
        formatRound(columns = c("avg_killed"), digits = 2)
    
    saveWidget(table, "visualizations/interactive_table.html")
    return(table)
}

# -----------------------------------------------------------------------------
#                    6. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Création du dossier pour les visualisations
    dir.create("visualizations", showWarnings = FALSE)
    
    # Chargement des données
    message("Chargement des données...")
    data <- read_csv("data/processed/gtd_cleaned.csv")
    
    # Création des visualisations
    map <- create_interactive_map(data)
    trends <- create_temporal_trends(data)
    distributions <- create_interactive_distributions(data)
    heatmaps <- create_interactive_heatmaps(data)
    tables <- create_interactive_tables(data)
    
    message("\nToutes les visualisations ont été générées dans le dossier 'visualizations'!")
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}
