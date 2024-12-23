# ==============================================================================
#                    VISUALISATIONS ANIMÉES DES DONNÉES GTD
# ==============================================================================

library(tidyverse)
library(leaflet)
library(gganimate)
library(maps)
library(mapdata)
library(animation)
library(RColorBrewer)
library(gridExtra)
library(htmlwidgets)

# -----------------------------------------------------------------------------
#                    1. FONCTIONS DE BASE
# -----------------------------------------------------------------------------

create_base_map <- function() {
    world_map <- map_data("world")
    
    ggplot() +
        geom_map(
            data = world_map,
            map = world_map,
            aes(long, lat, map_id = region),
            color = "darkgrey",
            fill = "burlywood"
        ) +
        coord_fixed(1.3) +
        theme_minimal() +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank()
        )
}

prepare_data <- function(data) {
    data %>%
        filter(!is.na(latitude), !is.na(longitude)) %>%
        mutate(
            region_txt = as.factor(region_txt),
            year = iyear,
            month = imonth,
            # Calcul des statistiques par région et année
            group_by(region_txt, year) %>%
            mutate(
                region_mean = mean(nkill, na.rm = TRUE),
                kill_diff = nkill - region_mean,
                abs_kill_diff = abs(kill_diff)
            ) %>%
            ungroup()
        )
}

# -----------------------------------------------------------------------------
#                    2. CARTES STATIQUES
# -----------------------------------------------------------------------------

create_static_map <- function(data) {
    create_base_map() +
        geom_point(
            data = data,
            aes(x = longitude, y = latitude, 
                color = region_txt, size = nkill),
            alpha = 0.6
        ) +
        scale_color_brewer(palette = "Set3") +
        scale_size_continuous(range = c(0.5, 5)) +
        labs(
            title = "Distribution Mondiale du Terrorisme (1970-2015)",
            subtitle = "Taille des points proportionnelle au nombre de victimes",
            color = "Région",
            size = "Nombre de victimes"
        ) +
        theme(
            legend.position = "bottom",
            legend.box = "vertical",
            legend.text = element_text(size = 8)
        ) +
        guides(
            color = guide_legend(nrow = 2),
            size = guide_legend(nrow = 1)
        )
}

# -----------------------------------------------------------------------------
#                    3. ANALYSES TEMPORELLES
# -----------------------------------------------------------------------------

create_temporal_analysis <- function(data) {
    # Analyses annuelles
    yearly_stats <- data %>%
        group_by(year) %>%
        summarise(
            total_attacks = n(),
            avg_kills = mean(nkill, na.rm = TRUE),
            total_kills = sum(nkill, na.rm = TRUE),
            .groups = "drop"
        )
    
    # Graphique des attaques
    p1 <- ggplot(yearly_stats, aes(x = year, y = total_attacks)) +
        geom_line(color = "darkred", size = 1) +
        geom_smooth(method = "loess", color = "blue", alpha = 0.2) +
        theme_minimal() +
        labs(
            title = "Évolution du Nombre d'Attaques",
            x = "Année",
            y = "Nombre d'attaques"
        )
    
    # Graphique des victimes
    p2 <- ggplot(yearly_stats, aes(x = year)) +
        geom_line(aes(y = avg_kills), color = "darkred", size = 1) +
        geom_smooth(aes(y = avg_kills), method = "loess", 
                   color = "blue", alpha = 0.2) +
        theme_minimal() +
        labs(
            title = "Évolution du Nombre Moyen de Victimes",
            x = "Année",
            y = "Moyenne des victimes"
        )
    
    list(plots = list(attacks = p1, kills = p2), data = yearly_stats)
}

# -----------------------------------------------------------------------------
#                    4. ANIMATIONS
# -----------------------------------------------------------------------------

create_animated_map <- function(data) {
    create_base_map() +
        geom_point(
            data = data,
            aes(x = longitude, y = latitude,
                size = abs_kill_diff,
                color = kill_diff)
        ) +
        scale_color_gradient2(
            low = "blue",
            mid = "white",
            high = "red",
            midpoint = 0
        ) +
        scale_size_continuous(range = c(1, 8)) +
        transition_time(year) +
        labs(
            title = "Évolution des Attaques Terroristes - Année: {frame_time}",
            subtitle = "Différence par rapport à la moyenne régionale",
            size = "Écart à la moyenne",
            color = "Différence de victimes"
        ) +
        ease_aes('linear') +
        shadow_wake(wake_length = 0.1)
}

create_animated_heatmap <- function(data) {
    # Création d'une heatmap animée des régions au fil du temps
    data %>%
        group_by(year, region_txt) %>%
        summarise(
            attacks = n(),
            .groups = "drop"
        ) %>%
        ggplot(aes(x = year, y = region_txt, fill = attacks)) +
        geom_tile() +
        scale_fill_viridis_c() +
        theme_minimal() +
        transition_time(year) +
        labs(
            title = "Intensité des Attaques par Région - Année: {frame_time}",
            x = "Année",
            y = "Région",
            fill = "Nombre d'attaques"
        )
}

# -----------------------------------------------------------------------------
#                    5. VERSION INTERACTIVE
# -----------------------------------------------------------------------------

create_interactive_map <- function(data) {
    data <- data %>%
        mutate(
            popup_text = sprintf(
                "<strong>Année:</strong> %d<br/>
                <strong>Région:</strong> %s<br/>
                <strong>Victimes:</strong> %d<br/>
                <strong>Différence moyenne:</strong> %.2f",
                year, region_txt, nkill, kill_diff
            )
        )
    
    leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
            ~longitude, ~latitude,
            popup = ~popup_text,
            radius = ~sqrt(nkill + 1) * 2,
            color = ~colorFactor("Set3", region_txt)(region_txt),
            fillOpacity = 0.7,
            stroke = FALSE,
            clusterOptions = markerClusterOptions(
                spiderfyOnMaxZoom = TRUE,
                showCoverageOnHover = TRUE,
                zoomToBoundsOnClick = TRUE,
                maxClusterRadius = 30
            )
        ) %>%
        addLegend(
            position = "bottomright",
            pal = colorFactor("Set3", data$region_txt),
            values = ~region_txt,
            title = "Régions",
            opacity = 0.7
        )
}

# -----------------------------------------------------------------------------
#                    6. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Création des dossiers
    dir.create("visualizations/static", recursive = TRUE, showWarnings = FALSE)
    dir.create("visualizations/animated", recursive = TRUE, showWarnings = FALSE)
    
    # Chargement et préparation des données
    message("Chargement des données...")
    data <- read_csv("data/processed/gtd_cleaned.csv")
    prepared_data <- prepare_data(data)
    
    # Création des visualisations statiques
    message("Création des cartes statiques...")
    static_map <- create_static_map(prepared_data)
    ggsave("visualizations/static/global_terrorism_map.png", static_map,
           width = 12, height = 8)
    
    # Analyses temporelles
    message("Création des analyses temporelles...")
    temporal_analysis <- create_temporal_analysis(prepared_data)
    ggsave("visualizations/static/temporal_trends.png",
           grid.arrange(temporal_analysis$plots$attacks,
                       temporal_analysis$plots$kills,
                       ncol = 2),
           width = 15, height = 6)
    
    # Création des animations
    message("Création des animations...")
    animated_map <- create_animated_map(prepared_data)
    animated_heatmap <- create_animated_heatmap(prepared_data)
    
    anim_save("visualizations/animated/terrorism_map.gif", animated_map,
              nframes = 200, fps = 10, width = 800, height = 600)
    anim_save("visualizations/animated/terrorism_heatmap.gif", animated_heatmap,
              nframes = 200, fps = 10, width = 800, height = 600)
    
    # Création de la carte interactive
    message("Création de la carte interactive...")
    interactive_map <- create_interactive_map(prepared_data)
    saveWidget(interactive_map,
              "visualizations/animated/interactive_map.html")
    
    message("Toutes les visualisations ont été générées avec succès!")
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}
