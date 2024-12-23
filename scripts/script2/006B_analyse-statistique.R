# ==============================================================================
#                    ANALYSE STATISTIQUE DES DONNÉES GTD 
# ==============================================================================

# -----------------------------------------------------------------------------
#                    1. CHARGEMENT DES PACKAGES
# -----------------------------------------------------------------------------

# Installation des packages manquants
install_missing_packages <- function(pkgs) {
  to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(to_install)) {
    install.packages(to_install)
  }
}

required_packages <- c("tidyverse", "corrplot", "lubridate", "broom", 
                       "scales", "gridExtra", "here", "tsibble", "feasts", "fable")
install_missing_packages(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# -----------------------------------------------------------------------------
#                    2. CHARGEMENT ET PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------

# Fonction pour charger les données
load_gtd_data <- function() {
  input_path <- here("data", "processed", "gtd_clean.csv")
  
  if (!file.exists(input_path)) {
    stop("Erreur : Le fichier 'gtd_clean.csv' est introuvable dans 'data/processed/'.")
  }
  
  message("\nChargement des données...")
  data <- read_csv(input_path, locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)
  
  # Convertir les colonnes en UTF-8 pour éviter les erreurs
  data <- data %>%
    mutate(across(where(is.character), ~ enc2utf8(.)))
  
  # Créer des colonnes supplémentaires
  data <- data %>%
    mutate(total_casualties = replace_na(killed, 0) + replace_na(wounded, 0),
           date = as.Date(date))
  
  return(data)
}

# -----------------------------------------------------------------------------
#                    3. ANALYSE EXPLORATOIRE (EDA)
# -----------------------------------------------------------------------------

# Analyse des distributions et sauvegarde des figures
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
