# ==============================================================================
#                    ANALYSE EXPLORATOIRE DES DONNÉES GTD
# ==============================================================================

# Installation et chargement des packages
install_missing_packages <- function(pkgs) {
  to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(to_install)) {
    install.packages(to_install)
  }
}

required_packages <- c("tidyverse", "lubridate", "ggplot2", "scales", "here", "plotly", "DT")
install_missing_packages(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# Chemins d'accès
PATHS <- list(
  INPUT = here("data", "processed", "gtd_clean.csv"),
  OUTPUT = here("outputs", "figures")
)

# Vérification du fichier
if (!file.exists(PATHS$INPUT)) stop("Erreur : Le fichier gtd_clean.csv est introuvable.")

dir.create(PATHS$OUTPUT, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
#                    CHARGEMENT ET PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------
gtd_data <- read_csv(PATHS$INPUT, locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE) %>%
  mutate(
    across(where(is.character), ~ enc2utf8(.)),
    total_casualties = replace_na(killed, 0) + replace_na(wounded, 0),
    year = as.integer(year)
  ) %>%
  filter(!is.na(year) & year >= 1970 & year <= 2017)

# -----------------------------------------------------------------------------
#                    STATISTIQUES ET VISUALISATIONS
# -----------------------------------------------------------------------------
# Statistiques descriptives
cat("\n=== STATISTIQUES DESCRIPTIVES ===\n")
print(summary(gtd_data))

# Tableaux interactifs
datatable(head(gtd_data, 100), caption = "Aperçu des 100 premières lignes")

# Graphiques
current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# 1. Distribution des types d'attaques
distribution_attacks <- ggplot(gtd_data, aes(x = fct_infreq(attack_type))) +
  geom_bar(fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribution des types d'attaques", x = "Type d'attaque", y = "Nombre d'incidents")

ggsave(file.path(PATHS$OUTPUT, paste0("distribution_attacks_", current_time, ".png")), distribution_attacks)

# 2. Évolution annuelle
evolution_annual <- gtd_data %>%
  group_by(year) %>%
  summarise(total_incidents = n()) %>%
  ggplot(aes(x = year, y = total_incidents)) +
  geom_line(color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Évolution annuelle des incidents", x = "Année", y = "Nombre d'incidents")

ggsave(file.path(PATHS$OUTPUT, paste0("evolution_annual_", current_time, ".png")), evolution_annual)

# 3. Carte géographique (optionnelle)
if ("latitude" %in% colnames(gtd_data) & "longitude" %in% colnames(gtd_data)) {
  map_plot <- gtd_data %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    sample_n(5000) %>%
    ggplot(aes(x = longitude, y = latitude)) +
    geom_point(alpha = 0.5, color = "blue") +
    theme_minimal() +
    labs(title = "Répartition géographique des incidents", x = "Longitude", y = "Latitude")
  
  ggsave(file.path(PATHS$OUTPUT, paste0("geographic_distribution_", current_time, ".png")), map_plot)
}

# -----------------------------------------------------------------------------
#                    RÉCAPITULATIF
# -----------------------------------------------------------------------------
cat("\n=== RÉCAPITULATIF DE L'ANALYSE ===\n")
cat("Nombre total d'incidents analysés :", nrow(gtd_data), "\n")
cat("Période couverte : de", min(gtd_data$year), "à", max(gtd_data$year), "\n")
cat("=== ANALYSE EXPLORATOIRE TERMINÉE ===\n")
