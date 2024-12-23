# ==============================================================================
#                    ANALYSE DE CLUSTERING DES ATTAQUES TERRORISTES
# ==============================================================================

library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(scales)
library(viridis)
library(plotly)
library(htmlwidgets)

# -----------------------------------------------------------------------------
#                    1. PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------

prepare_clustering_data <- function(data) {
    message("Préparation des données pour le clustering...")
    
    # Sélection et préparation des variables
    clustering_vars <- data %>%
        select(
            killed = nkill,
            wounded = nwound,
            latitude,
            longitude,
            success,
            year
        ) %>%
        na.omit()
    
    # Standardisation des variables numériques
    scaled_vars <- clustering_vars %>%
        mutate(across(where(is.numeric), scale))
    
    return(list(
        original = clustering_vars,
        scaled = scaled_vars
    ))
}

# -----------------------------------------------------------------------------
#                    2. DÉTERMINATION DU NOMBRE OPTIMAL DE CLUSTERS
# -----------------------------------------------------------------------------

find_optimal_clusters <- function(data, max_k = 10) {
    message("Recherche du nombre optimal de clusters...")
    
    # Méthode du coude
    wss <- fviz_nbclust(data, kmeans,
                        method = "wss",
                        k.max = max_k) +
        labs(title = "Méthode du Coude") +
        theme_minimal()
    
    # Méthode de la silhouette
    silhouette <- fviz_nbclust(data, kmeans,
                              method = "silhouette",
                              k.max = max_k) +
        labs(title = "Méthode de la Silhouette") +
        theme_minimal()
    
    # Gap statistic
    gap <- fviz_nbclust(data, kmeans,
                        method = "gap_stat",
                        k.max = max_k) +
        labs(title = "Méthode Gap") +
        theme_minimal()
    
    return(list(
        wss = wss,
        silhouette = silhouette,
        gap = gap
    ))
}

# -----------------------------------------------------------------------------
#                    3. CLUSTERING K-MEANS
# -----------------------------------------------------------------------------

perform_kmeans <- function(data, k) {
    message(sprintf("Application du k-means avec k = %d...", k))
    
    set.seed(123)
    km_result <- kmeans(data, centers = k, nstart = 25)
    
    return(km_result)
}

# -----------------------------------------------------------------------------
#                    4. VISUALISATION DES CLUSTERS
# -----------------------------------------------------------------------------

create_cluster_visualizations <- function(data, km_result) {
    message("Création des visualisations des clusters...")
    
    # Ajout des clusters aux données originales
    data_clustered <- data %>%
        mutate(cluster = as.factor(km_result$cluster))
    
    # Visualisation principale des clusters
    p1 <- fviz_cluster(km_result, data = data,
                       palette = viridis(km_result$centers %>% nrow()),
                       ellipse.type = "convex",
                       ggtheme = theme_minimal()) +
        labs(title = "Visualisation des Clusters")
    
    # Distribution géographique interactive
    p2 <- plot_ly(data_clustered,
                  x = ~longitude,
                  y = ~latitude,
                  color = ~cluster,
                  type = "scatter",
                  mode = "markers",
                  marker = list(size = 8, opacity = 0.6)) %>%
        layout(title = "Distribution Géographique des Clusters",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    
    # Distribution temporelle
    p3 <- ggplot(data_clustered,
                 aes(x = year, fill = cluster)) +
        geom_density(alpha = 0.5) +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = "Distribution Temporelle des Clusters",
             x = "Année",
             y = "Densité")
    
    return(list(
        cluster_viz = p1,
        geo_dist = p2,
        time_dist = p3
    ))
}

# -----------------------------------------------------------------------------
#                    5. ANALYSE DES PROFILS DE CLUSTERS
# -----------------------------------------------------------------------------

analyze_cluster_profiles <- function(data, km_result) {
    message("Analyse des profils des clusters...")
    
    # Création des profils
    cluster_profiles <- data %>%
        mutate(cluster = km_result$cluster) %>%
        group_by(cluster) %>%
        summarise(
            nombre_attaques = n(),
            moy_tues = mean(killed, na.rm = TRUE),
            moy_blesses = mean(wounded, na.rm = TRUE),
            lat_moyenne = mean(latitude, na.rm = TRUE),
            long_moyenne = mean(longitude, na.rm = TRUE),
            taux_succes = mean(success, na.rm = TRUE)
        )
    
    # Visualisation des caractéristiques
    profile_heatmap <- cluster_profiles %>%
        gather(key = "variable", value = "value", -cluster) %>%
        ggplot(aes(x = cluster, y = variable, fill = value)) +
        geom_tile() +
        scale_fill_viridis() +
        theme_minimal() +
        labs(title = "Caractéristiques des Clusters",
             x = "Cluster",
             y = "Variable")
    
    # Statistiques des clusters
    stats_plot <- cluster_profiles %>%
        gather(key = "metric", value = "value", -cluster) %>%
        ggplot(aes(x = as.factor(cluster), y = value)) +
        geom_bar(stat = "identity", fill = viridis(1)) +
        facet_wrap(~metric, scales = "free_y") +
        theme_minimal() +
        labs(title = "Statistiques par Cluster",
             x = "Cluster",
             y = "Valeur")
    
    return(list(
        profiles = cluster_profiles,
        heatmap = profile_heatmap,
        stats = stats_plot
    ))
}

# -----------------------------------------------------------------------------
#                    6. VALIDATION DU CLUSTERING
# -----------------------------------------------------------------------------

validate_clustering <- function(data, km_result) {
    message("Validation du clustering...")
    
    # Silhouette moyenne
    sil <- silhouette(km_result$cluster, dist(data))
    avg_sil <- mean(sil[,3])
    
    # Visualisation de la silhouette
    sil_plot <- fviz_silhouette(sil) +
        theme_minimal() +
        labs(title = "Analyse de la Silhouette")
    
    # Variance expliquée
    var_exp <- km_result$betweenss / km_result$totss
    
    # Statistiques de validation
    validation_stats <- data.frame(
        Metric = c("Score Silhouette Moyen", "Variance Expliquée"),
        Value = c(avg_sil, var_exp)
    )
    
    return(list(
        silhouette = sil_plot,
        stats = validation_stats
    ))
}

# -----------------------------------------------------------------------------
#                    7. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Création du dossier pour les résultats
    dir.create("results/clustering", recursive = TRUE, showWarnings = FALSE)
    
    # Chargement des données
    message("Chargement des données...")
    data <- read_csv("data/processed/gtd_cleaned.csv")
    
    # Préparation des données
    prep_data <- prepare_clustering_data(data)
    
    # Recherche du nombre optimal de clusters
    optimal_k <- find_optimal_clusters(prep_data$scaled)
    
    # Sauvegarde des graphiques d'optimisation
    ggsave("results/clustering/elbow_method.png", optimal_k$wss)
    ggsave("results/clustering/silhouette_method.png", optimal_k$silhouette)
    ggsave("results/clustering/gap_method.png", optimal_k$gap)
    
    # Application du k-means avec k optimal (ici k=5)
    km_result <- perform_kmeans(prep_data$scaled, k = 5)
    
    # Création des visualisations
    viz <- create_cluster_visualizations(prep_data$scaled, km_result)
    
    # Analyse des profils
    profiles <- analyze_cluster_profiles(prep_data$original, km_result)
    
    # Validation
    validation <- validate_clustering(prep_data$scaled, km_result)
    
    # Sauvegarde des résultats
    saveRDS(km_result, "results/clustering/kmeans_model.rds")
    saveRDS(profiles$profiles, "results/clustering/cluster_profiles.rds")
    
    # Sauvegarde des visualisations
    ggsave("results/clustering/cluster_visualization.png", viz$cluster_viz)
    saveWidget(viz$geo_dist, "results/clustering/geographic_distribution.html")
    ggsave("results/clustering/temporal_distribution.png", viz$time_dist)
    ggsave("results/clustering/profiles_heatmap.png", profiles$heatmap)
    ggsave("results/clustering/cluster_stats.png", profiles$stats)
    ggsave("results/clustering/silhouette_analysis.png", validation$silhouette)
    
    # Sauvegarde des statistiques
    write_csv(validation$stats, "results/clustering/validation_stats.csv")
    
    message("Analyse de clustering terminée avec succès!")
    
    return(list(
        model = km_result,
        visualizations = viz,
        profiles = profiles,
        validation = validation
    ))
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}
