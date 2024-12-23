# ==============================================================================
#                    ANALYSE STATISTIQUE DES DONNÉES GTD
# ==============================================================================

library(tidyverse)
library(corrplot)
library(tsibble)
library(feasts)
library(lubridate)
library(broom)
library(scales)
library(fable)
library(gridExtra)

# -----------------------------------------------------------------------------
#                    1. CHARGEMENT ET PRÉPARATION
# -----------------------------------------------------------------------------

load_gtd_data <- function() {
    data <- read_csv("data/processed/gtd_cleaned.csv")
    return(data)
}

# -----------------------------------------------------------------------------
#                    2. ANALYSE EXPLORATOIRE (EDA)
# -----------------------------------------------------------------------------

analyze_distributions <- function(data) {
    message("\n=== ANALYSE DES DISTRIBUTIONS ===")
    
    # Distribution des attaques par type
    p1 <- ggplot(data, aes(x = attack_type)) +
        geom_bar() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Distribution des types d'attaques",
             x = "Type d'attaque", y = "Nombre d'incidents")
    
    # Distribution géographique
    p2 <- ggplot(data, aes(x = region)) +
        geom_bar() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Distribution par région",
             x = "Région", y = "Nombre d'incidents")
    
    # Distribution des victimes
    p3 <- ggplot(data, aes(x = killed)) +
        geom_histogram(binwidth = 1) +
        scale_x_log10(breaks = 10^(0:4)) +
        theme_minimal() +
        labs(title = "Distribution du nombre de victimes (échelle log)",
             x = "Nombre de victimes", y = "Fréquence")
    
    grid.arrange(p1, p2, p3, ncol = 2)
    
    return(list(p1 = p1, p2 = p2, p3 = p3))
}

analyze_correlations <- function(data) {
    message("\n=== ANALYSE DES CORRÉLATIONS ===")
    
    # Sélection des variables numériques
    numeric_data <- data %>%
        select(killed, wounded, latitude, longitude) %>%
        cor(use = "complete.obs")
    
    # Création de la matrice de corrélation
    corrplot(numeric_data, 
            method = "color",
            type = "upper",
            addCoef.col = "black",
            tl.col = "black",
            tl.srt = 45)
}

# -----------------------------------------------------------------------------
#                    3. STATISTIQUES DESCRIPTIVES
# -----------------------------------------------------------------------------

calculate_descriptive_stats <- function(data) {
    message("\n=== STATISTIQUES DESCRIPTIVES ===")
    
    # Statistiques générales
    stats_general <- data %>%
        summarise(
            total_incidents = n(),
            total_killed = sum(killed, na.rm = TRUE),
            total_wounded = sum(wounded, na.rm = TRUE),
            mean_killed = mean(killed, na.rm = TRUE),
            median_killed = median(killed, na.rm = TRUE),
            sd_killed = sd(killed, na.rm = TRUE)
        )
    
    # Statistiques par région
    stats_region <- data %>%
        group_by(region) %>%
        summarise(
            incidents = n(),
            total_killed = sum(killed, na.rm = TRUE),
            mean_killed = mean(killed, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(desc(incidents))
    
    # Statistiques par type d'attaque
    stats_attack <- data %>%
        group_by(attack_type) %>%
        summarise(
            incidents = n(),
            total_killed = sum(killed, na.rm = TRUE),
            mean_killed = mean(killed, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(desc(incidents))
    
    list(
        general = stats_general,
        by_region = stats_region,
        by_attack = stats_attack
    )
}

# -----------------------------------------------------------------------------
#                    4. TESTS STATISTIQUES
# -----------------------------------------------------------------------------

perform_chi_square_tests <- function(data) {
    message("\n=== TESTS DE CHI-CARRÉ ===")
    
    # Test d'indépendance entre région et type d'attaque
    contingency_table <- table(data$region, data$attack_type)
    chi_test <- chisq.test(contingency_table)
    
    # Résidus standardisés
    residuals <- chi_test$residuals
    
    list(
        test_result = chi_test,
        residuals = residuals
    )
}

perform_regression_analysis <- function(data) {
    message("\n=== ANALYSE DE RÉGRESSION ===")
    
    # Modèle de régression pour prédire le nombre de victimes
    model <- lm(killed ~ region + attack_type + wounded, data = data)
    
    # Résumé du modèle
    model_summary <- summary(model)
    
    # Coefficients avec tidying
    coef_tidy <- tidy(model)
    
    list(
        model = model,
        summary = model_summary,
        coefficients = coef_tidy
    )
}

# -----------------------------------------------------------------------------
#                    5. ANALYSE TEMPORELLE
# -----------------------------------------------------------------------------

analyze_time_series <- function(data) {
    message("\n=== ANALYSE DES SÉRIES TEMPORELLES ===")
    
    # Agrégation par mois
    monthly_incidents <- data %>%
        mutate(date = as.Date(date)) %>%
        group_by(date = floor_date(date, "month")) %>%
        summarise(
            incidents = n(),
            killed = sum(killed, na.rm = TRUE),
            .groups = "drop"
        )
    
    # Visualisation des tendances
    p1 <- ggplot(monthly_incidents, aes(x = date, y = incidents)) +
        geom_line() +
        theme_minimal() +
        labs(title = "Évolution du nombre d'incidents par mois",
             x = "Date", y = "Nombre d'incidents")
    
    # Décomposition de la série temporelle
    ts_data <- monthly_incidents %>%
        as_tsibble(index = date) %>%
        model(STL(incidents ~ season(window = 12)))
    
    components <- components(ts_data)
    
    # Visualisation de la décomposition
    p2 <- autoplot(components) +
        theme_minimal()
    
    list(
        time_series = monthly_incidents,
        decomposition = components,
        plot_trend = p1,
        plot_decomposition = p2
    )
}

# -----------------------------------------------------------------------------
#                    6. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Chargement des données
    data <- load_gtd_data()
    
    # Analyse exploratoire
    distribution_plots <- analyze_distributions(data)
    analyze_correlations(data)
    
    # Statistiques descriptives
    stats <- calculate_descriptive_stats(data)
    print(stats)
    
    # Tests statistiques
    chi_square_results <- perform_chi_square_tests(data)
    print(chi_square_results$test_result)
    
    regression_results <- perform_regression_analysis(data)
    print(regression_results$summary)
    
    # Analyse temporelle
    time_series_results <- analyze_time_series(data)
    print(time_series_results$decomposition)
    
    # Sauvegarde des résultats
    message("\nSauvegarde des résultats...")
    saveRDS(list(
        stats = stats,
        chi_square = chi_square_results,
        regression = regression_results,
        time_series = time_series_results
    ), "data/processed/statistical_analysis_results.rds")
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}


Analyse des distributions et sauvegarde des figures
analyze_distributions <- function(data, output_dir) {
  message("\n=== ANALYSE DES DISTRIBUTIONS ===")
  
  # Distribution des types d'attaques
  p1 <- ggplot(data, aes(x = fct_infreq(attack_type))) +
    geom_bar(fill = "skyblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Distribution des types d'attaques", x = "Type d'attaque", y = "Nombre d'incidents")
  ggsave(file.path(output_dir, "distribution_attacks.png"), p1)
  
  # Distribution par région
  p2 <- ggplot(data, aes(x = fct_infreq(region))) +
    geom_bar(fill = "orange") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Distribution par région", x = "Région", y = "Nombre d'incidents")
  ggsave(file.path(output_dir, "distribution_regions.png"), p2)
  
  # Distribution des victimes (log scale)
  p3 <- ggplot(data, aes(x = killed)) +
    geom_histogram(binwidth = 1, fill = "red") +
    scale_x_log10(breaks = 10^(0:4)) +
    theme_minimal() +
    labs(title = "Distribution du nombre de victimes (échelle log)",
         x = "Nombre de victimes", y = "Fréquence")
  ggsave(file.path(output_dir, "distribution_victims.png"), p3)
  
  # Affichage des graphiques
  grid.arrange(p1, p2, p3, ncol = 2)
}

# Analyse des corrélations
analyze_correlations <- function(data, output_dir) {
  message("\n=== ANALYSE DES CORRÉLATIONS ===")
  
  numeric_data <- data %>%
    select(killed, wounded, latitude, longitude) %>%
    drop_na()
  
  corr_matrix <- cor(numeric_data)
  png(file.path(output_dir, "correlation_matrix.png"), width = 800, height = 600)
  corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black")
  dev.off()
}

# -----------------------------------------------------------------------------
#                    4. STATISTIQUES DESCRIPTIVES
# -----------------------------------------------------------------------------

calculate_descriptive_stats <- function(data) {
  message("\n=== STATISTIQUES DESCRIPTIVES ===")
  
  stats_general <- data %>%
    summarise(
      total_incidents = n(),
      total_killed = sum(killed, na.rm = TRUE),
      total_wounded = sum(wounded, na.rm = TRUE),
      mean_killed = mean(killed, na.rm = TRUE),
      sd_killed = sd(killed, na.rm = TRUE)
    )
  
  print(stats_general)
  return(stats_general)
}

# -----------------------------------------------------------------------------
#                    5. ANALYSE TEMPORELLE
# -----------------------------------------------------------------------------

analyze_time_series <- function(data, output_dir) {
  message("\n=== ANALYSE DES SÉRIES TEMPORELLES ===")
  
  # Agrégation par mois
  monthly_data <- data %>%
    filter(!is.na(date)) %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(total_incidents = n(), .groups = "drop")
  
  # Visualisation
  p <- ggplot(monthly_data, aes(x = month, y = total_incidents)) +
    geom_line(color = "blue") +
    theme_minimal() +
    labs(title = "Nombre d'incidents par mois", x = "Mois", y = "Nombre total d'incidents")
  
  ggsave(file.path(output_dir, "time_series_incidents.png"), p)
  print(p)
}

# -----------------------------------------------------------------------------
#                    6. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
  # Création du répertoire de sortie
  output_dir <- here("outputs", "figures")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Chargement des données
  data <- load_gtd_data()
  
  # Analyse exploratoire
  analyze_distributions(data, output_dir)
  analyze_correlations(data, output_dir)
  
  # Statistiques descriptives
  stats <- calculate_descriptive_stats(data)
  
  # Analyse temporelle
  analyze_time_series(data, output_dir)
  
  message("\n=== ANALYSE STATISTIQUE TERMINÉE ===")
}

# Exécution de la fonction principale
main()
