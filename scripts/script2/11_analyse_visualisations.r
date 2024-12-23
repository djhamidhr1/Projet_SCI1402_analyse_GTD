# ==============================================================================
#                    ANALYSE COMPLÈTE DES DONNÉES GTD
# ==============================================================================

library(tidyverse)
library(leaflet)
library(plotly)
library(viridis)
library(gridExtra)
library(scales)
library(treemapify)
library(corrplot)
library(gganimate)
library(htmlwidgets)

# -----------------------------------------------------------------------------
#                    1. ANALYSE TEMPORELLE
# -----------------------------------------------------------------------------

temporal_analysis <- function(data) {
    message("Création des analyses temporelles...")
    
    # Tendance annuelle
    p1 <- ggplot(data, aes(x = year)) +
        geom_line(stat = "count", color = "#440154FF") +
        geom_point(stat = "count", color = "#440154FF") +
        theme_minimal() +
        labs(title = "Évolution du nombre d'attaques par année",
             y = "Nombre d'attaques")
    
    # Distribution mensuelle
    p2 <- ggplot(data, aes(x = factor(month))) +
        geom_bar(fill = viridis(1)) +
        theme_minimal() +
        labs(title = "Distribution mensuelle des attaques",
             x = "Mois", y = "Nombre d'attaques")
    
    # Box plot des victimes par année
    p3 <- ggplot(data, aes(x = factor(year), y = killed)) +
        geom_boxplot(fill = viridis(12)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45)) +
        labs(title = "Distribution des victimes par année")
    
    # Tendance animée
    p4 <- ggplot(data, aes(x = year, y = killed)) +
        geom_point(aes(size = wounded, color = region)) +
        scale_color_viridis_d() +
        theme_minimal() +
        transition_time(year) +
        labs(title = "Année: {frame_time}")
    
    list(trend = p1, monthly = p2, victims = p3, animated = p4)
}

# -----------------------------------------------------------------------------
#                    2. ANALYSE GÉOGRAPHIQUE
# -----------------------------------------------------------------------------

geographic_analysis <- function(data) {
    message("Création des analyses géographiques...")
    
    # Carte interactive
    map1 <- leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
            lng = ~longitude,
            lat = ~latitude,
            radius = ~sqrt(killed + 1) * 2,
            popup = ~paste(
                "<strong>Pays:</strong>", country, "<br>",
                "<strong>Type:</strong>", attack_type, "<br>",
                "<strong>Victimes:</strong>", killed
            ),
            color = ~colorFactor(viridis(8), region)(region),
            clusterOptions = markerClusterOptions()
        ) %>%
        addLegend(
            position = "bottomright",
            pal = colorFactor(viridis(8), data$region),
            values = ~region,
            title = "Régions"
        )
    
    # Heatmap par région et type d'attaque
    region_attack_matrix <- table(data$region, data$attack_type)
    p1 <- corrplot(region_attack_matrix, 
                   method = "color",
                   type = "upper", 
                   order = "hclust",
                   col = viridis(100))
    
    # Barplot des pays les plus touchés
    p2 <- data %>%
        count(country) %>%
        arrange(desc(n)) %>%
        head(20) %>%
        ggplot(aes(x = reorder(country, n), y = n)) +
        geom_bar(stat = "identity", fill = viridis(1)) +
        coord_flip() +
        theme_minimal() +
        labs(title = "20 pays les plus touchés",
             x = "Pays", y = "Nombre d'attaques")
    
    list(map = map1, heatmap = p1, countries = p2)
}

# -----------------------------------------------------------------------------
#                    3. ANALYSE DES TYPES D'ATTAQUES
# -----------------------------------------------------------------------------

attack_type_analysis <- function(data) {
    message("Création des analyses par type d'attaque...")
    
    # Distribution des types d'attaques
    p1 <- ggplot(data, aes(x = reorder(attack_type, attack_type, 
                                      function(x) length(x)))) +
        geom_bar(fill = viridis(1)) +
        coord_flip() +
        theme_minimal() +
        labs(title = "Distribution des types d'attaques",
             x = "Type d'attaque",
             y = "Nombre d'attaques")
    
    # Treemap des types d'attaques
    p2 <- data %>%
        group_by(attack_type) %>%
        summarise(
            count = n(),
            total_killed = sum(killed, na.rm = TRUE)
        ) %>%
        ggplot(aes(area = count, 
                   fill = total_killed,
                   label = paste(attack_type, "\n", count))) +
        geom_treemap() +
        scale_fill_viridis() +
        labs(title = "Types d'attaques",
             subtitle = "Taille = fréquence, couleur = victimes")
    
    # Évolution temporelle
    p3 <- data %>%
        group_by(year, attack_type) %>%
        summarise(count = n(), .groups = "drop") %>%
        ggplot(aes(x = year, y = count, fill = attack_type)) +
        geom_area(position = "fill") +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = "Évolution des types d'attaques",
             x = "Année",
             y = "Proportion")
    
    list(distribution = p1, treemap = p2, evolution = p3)
}

# -----------------------------------------------------------------------------
#                    4. ANALYSE DES VICTIMES
# -----------------------------------------------------------------------------

casualty_analysis <- function(data) {
    message("Création des analyses de victimes...")
    
    # Distribution des victimes
    p1 <- ggplot(data, aes(x = killed)) +
        geom_histogram(fill = viridis(1), bins = 50) +
        scale_x_log10() +
        theme_minimal() +
        labs(title = "Distribution du nombre de victimes",
             subtitle = "Échelle logarithmique",
             x = "Nombre de victimes",
             y = "Fréquence")
    
    # Relation tués/blessés
    p2 <- ggplot(data, aes(x = killed, y = wounded)) +
        geom_point(alpha = 0.5, color = viridis(1)) +
        scale_x_log10() +
        scale_y_log10() +
        theme_minimal() +
        labs(title = "Relation entre tués et blessés",
             x = "Nombre de tués (log)",
             y = "Nombre de blessés (log)")
    
    # Évolution temporelle
    p3 <- data %>%
        group_by(year) %>%
        summarise(
            killed = sum(killed, na.rm = TRUE),
            wounded = sum(wounded, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        gather(key = "type", value = "count", -year) %>%
        ggplot(aes(x = year, y = count, color = type)) +
        geom_line() +
        scale_color_viridis_d() +
        theme_minimal() +
        labs(title = "Évolution du nombre de victimes",
             x = "Année",
             y = "Nombre de victimes")
    
    list(distribution = p1, relationship = p2, evolution = p3)
}

# -----------------------------------------------------------------------------
#                    5. CRÉATION DU DASHBOARD
# -----------------------------------------------------------------------------

create_dashboard <- function(plots) {
    message("Création du dashboard interactif...")
    
    # Conversion des graphiques en plotly
    trend_plotly <- ggplotly(plots$temporal$trend)
    countries_plotly <- ggplotly(plots$geographic$countries)
    attacks_plotly <- ggplotly(plots$attack_types$distribution)
    casualties_plotly <- ggplotly(plots$casualties$distribution)
    
    # Création du dashboard
    dashboard <- subplot(
        trend_plotly,
        countries_plotly,
        attacks_plotly,
        casualties_plotly,
        nrows = 2
    ) %>%
        layout(title = "Analyse du Terrorisme Global (1970-2015)",
               showlegend = TRUE)
    
    return(dashboard)
}

# -----------------------------------------------------------------------------
#                    6. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Création des dossiers
    dir.create("visualizations/dashboard", recursive = TRUE, showWarnings = FALSE)
    
    # Chargement des données
    message("Chargement des données...")
    data <- read_csv("data/processed/gtd_cleaned.csv")
    
    # Génération des visualisations
    message("\nGénération des visualisations...")
    visualizations <- list(
        temporal = temporal_analysis(data),
        geographic = geographic_analysis(data),
        attack_types = attack_type_analysis(data),
        casualties = casualty_analysis(data)
    )
    
    # Création et sauvegarde du dashboard
    message("\nCréation du dashboard...")
    dashboard <- create_dashboard(visualizations)
    saveWidget(dashboard, "visualizations/dashboard/terrorism_dashboard.html")
    
    # Sauvegarde des animations
    message("\nSauvegarde des animations...")
    anim_save("visualizations/dashboard/temporal_evolution.gif",
              visualizations$temporal$animated)
    
    message("\nToutes les visualisations ont été générées avec succès!")
    
    return(list(
        visualizations = visualizations,
        dashboard = dashboard
    ))
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}
