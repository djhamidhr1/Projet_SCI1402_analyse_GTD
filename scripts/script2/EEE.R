# ==============================================================================
#               ANALYSE STATISTIQUE DES DONNÉES GTD 2
# ==============================================================================

# ===================== 1. CHARGEMENT DES PACKAGES =============================
packages <- c("tidyverse", "corrplot", "tsibble", "feasts", "lubridate", 
              "broom", "scales", "fable", "gridExtra", "here")

# Installation et chargement automatique des packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# ===================== 2. CHARGEMENT ET PRÉPARATION ============================
load_gtd_data <- function() {
  input_path <- here("data", "processed", "gtd_clean.csv")
  
  if (!file.exists(input_path)) {
    stop("Erreur : Le fichier 'gtd_clean.csv' est introuvable dans 'data/processed/'.")
  }
  
  message("\nChargement des données...")
  data <- read_csv(input_path, show_col_types = FALSE)
  return(data)
}

# ===================== 3. ANALYSE EXPLORATOIRE (EDA) ===========================
analyze_distributions <- function(data) {
  message("\n=== ANALYSE DES DISTRIBUTIONS ===")
  
  # Distribution des attaques par type
  p1 <- ggplot(data, aes(x = attack_type)) +
    geom_bar(fill = "skyblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution des types d'attaques",
         x = "Type d'attaque", y = "Nombre d'incidents")
  
  # Distribution géographique
  p2 <- ggplot(data, aes(x = region)) +
    geom_bar(fill = "orange") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution par région",
         x = "Région", y = "Nombre d'incidents")
  
  # Distribution des victimes
  p3 <- ggplot(data, aes(x = killed)) +
    geom_histogram(binwidth = 1, fill = "red") +
    scale_x_log10(breaks = 10^(0:4)) +
    theme_minimal() +
    labs(title = "Distribution du nombre de victimes (\u00e9chelle log)",
         x = "Nombre de victimes", y = "Fréquence")
  
  grid.arrange(p1, p2, p3, ncol = 2)
}

analyze_correlations <- function(data) {
  message("\n=== ANALYSE DES CORRÉLATIONS ===")
  
  numeric_data <- data %>%
    select(killed, wounded, latitude, longitude) %>%
    drop_na()
  
  corr_matrix <- cor(numeric_data)
  
  corrplot(corr_matrix, 
           method = "color", type = "upper", 
           addCoef.col = "black", tl.col = "black", tl.srt = 45)
}

# ===================== 4. STATISTIQUES DESCRIPTIVES ============================
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

# ===================== 5. TESTS STATISTIQUES ET RÉGRESSION =====================
perform_chi_square_tests <- function(data) {
  message("\n=== TEST DE CHI-CARRÉ ===")
  
  contingency_table <- table(data$region, data$attack_type)
  chi_test <- chisq.test(contingency_table)
  
  print(chi_test)
  return(chi_test)
}

perform_regression_analysis <- function(data) {
  message("\n=== ANALYSE DE RÉGRESSION ===")
  
  model <- lm(killed ~ wounded + region + attack_type, data = data)
  print(summary(model))
  return(model)
}

# ===================== 6. ANALYSE TEMPORELLE ==================================
analyze_time_series <- function(data) {
  message("\n=== ANALYSE DES SÉRIES TEMPORELLES ===")
  
  monthly_data <- data %>%
    mutate(date = as.Date(date)) %>%
    group_by(month = floor_date(date, "month")) %>%
    summarise(total_incidents = n(), .groups = "drop")
  
  p <- ggplot(monthly_data, aes(x = month, y = total_incidents)) +
    geom_line(color = "blue") +
    theme_minimal() +
    labs(title = "Nombre d'incidents par mois",
         x = "Mois", y = "Nombre total d'incidents")
  
  print(p)
}

# ===================== 7. FONCTION PRINCIPALE =================================
main <- function() {
  data <- load_gtd_data()
  
  # Analyses
  analyze_distributions(data)
  analyze_correlations(data)
  calculate_descriptive_stats(data)
  perform_chi_square_tests(data)
  perform_regression_analysis(data)
  analyze_time_series(data)
  
  message("\n=== ANALYSE STATISTIQUE TERMINÉE ===")
}

# ===================== 8. GESTION DES MODES ===================================
if (!interactive()) {
  message("Mode non interactif détecté : Sauvegarde des graphiques dans 'output.pdf'...")
  
  pdf("output.pdf", width = 11, height = 8.5)
  main()
  dev.off()
  
  message("Les graphiques ont été sauvegardés dans 'output.pdf'.")
} else {
  message("Mode interactif détecté : Exécution standard de la fonction principale.")
  main()
}

