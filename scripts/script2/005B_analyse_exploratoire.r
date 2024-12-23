# ==============================================================================
#                    ANALYSE EXPLORATOIRE DES DONNÉES GTD
# ==============================================================================

# -----------------------------------------------------------------------------
#                    1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------
# Chargement des packages nécessaires
library(tidyverse)   # Pour la manipulation des données
library(lubridate)   # Pour la gestion des dates
library(ggplot2)     # Pour les visualisations
library(scales)      # Pour le formatage des échelles
library(here)        # Pour la gestion des chemins
library(plotly)      # Pour les graphiques interactifs
library(DT)          # Pour les tableaux interactifs
library(viridis)     # Pour les palettes de couleurs

# Configuration des chemins
PATHS <- list(
  INPUT = here("data", "processed", "gtd_clean.csv"),
  OUTPUT = here("outputs", "figures")
)

# Vérification et création des dossiers
if (!file.exists(PATHS$INPUT)) stop("Erreur : Le fichier gtd_clean.csv est introuvable.")
dir.create(PATHS$OUTPUT, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
#                    2. CHARGEMENT ET PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------
gtd_data <- read_csv(PATHS$INPUT, locale = locale(encoding = "UTF-8")) %>%
  mutate(
    # Conversion des caractères en UTF-8
    across(where(is.character), ~ enc2utf8(.)),
    # Calcul des victimes totales
    total_casualties = replace_na(killed, 0) + replace_na(wounded, 0),
    # Conversion de l'année en entier
    year = as.integer(year),
    # Création d'une variable de décennie
    decade = paste0(floor(year/10)*10, "s")
  ) %>%
  # Filtrage des années valides
  filter(!is.na(year) & between(year, 1970, 2017))

# -----------------------------------------------------------------------------
#                    3. ANALYSES STATISTIQUES
# -----------------------------------------------------------------------------
# Statistiques descriptives générales
cat("\n=== STATISTIQUES DESCRIPTIVES ===\n")
print(summary(gtd_data))

# Statistiques par décennie
decade_stats <- gtd_data %>%
  group_by(decade) %>%
  summarise(
    incidents = n(),
    total_killed = sum(killed, na.rm = TRUE),
    total_wounded = sum(wounded, na.rm = TRUE),
    avg_casualties = mean(total_casualties, na.rm = TRUE)
  )
print("\nStatistiques par décennie:")
print(decade_stats)

# -----------------------------------------------------------------------------
#                    4. VISUALISATIONS
# -----------------------------------------------------------------------------
# Style commun pour les graphiques
theme_set(theme_minimal() + 
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
)

# Timestamp pour les fichiers
current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# 1. Distribution des types d'attaques
distribution_attacks <- ggplot(gtd_data, aes(x = fct_infreq(attack_type))) +
  geom_bar(fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Distribution des types d'attaques",
    x = "Type d'attaque",
    y = "Nombre d'incidents"
  )

# 2. Évolution temporelle
evolution_annual <- gtd_data %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "red", size = 1) +
  geom_smooth(method = "loess", color = "blue", alpha = 0.2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Évolution annuelle des incidents terroristes",
    x = "Année",
    y = "Nombre d'incidents"
  )

# 3. Carte de chaleur par région et année
heatmap_region_year <- gtd_data %>%
  count(year, region) %>%
  ggplot(aes(x = year, y = region, fill = n)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(
    title = "Évolution des incidents par région",
    x = "Année",
    y = "Région",
    fill = "Nombre\nd'incidents"
  )

# 4. Top 20 des pays les plus touchés
top_countries <- gtd_data %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(
    title = "20 pays les plus touchés",
    x = "Pays",
    y = "Nombre d'incidents"
  )

# Sauvegarde des graphiques
plots <- list(
  distribution_attacks = distribution_attacks,
  evolution_annual = evolution_annual,
  heatmap_region_year = heatmap_region_year,
  top_countries = top_countries
)

for (name in names(plots)) {
  ggsave(
    file.path(PATHS$OUTPUT, paste0(name, "_", current_time, ".png")),
    plots[[name]],
    width = 12,
    height = 8,
    dpi = 300
  )
}

# -----------------------------------------------------------------------------
#                    5. ANALYSE INTERACTIVE
# -----------------------------------------------------------------------------
# Tableau interactif des données
if (interactive()) {
  datatable(
    gtd_data %>% 
      select(year, country, region, city, attack_type, killed, wounded, total_casualties) %>%
      head(1000),
    caption = "Aperçu des 1000 premiers incidents",
    options = list(pageLength = 25, scrollX = TRUE)
  )
}

# Graphique interactif de l'évolution temporelle
if (interactive()) {
  plot_ly(
    data = gtd_data %>% count(year),
    x = ~year,
    y = ~n,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
    layout(
      title = "Évolution interactive des incidents terroristes",
      xaxis = list(title = "Année"),
      yaxis = list(title = "Nombre d'incidents")
    )
}

# -----------------------------------------------------------------------------
#                    6. RAPPORT FINAL
# -----------------------------------------------------------------------------
cat("\n=== RÉCAPITULATIF DE L'ANALYSE ===\n")
cat("Nombre total d'incidents analysés:", nrow(gtd_data), "\n")
cat("Période couverte: de", min(gtd_data$year), "à", max(gtd_data$year), "\n")
cat("Nombre de pays touchés:", n_distinct(gtd_data$country), "\n")
cat("Total des victimes:", sum(gtd_data$total_casualties, na.rm = TRUE), "\n")
cat("=== ANALYSE EXPLORATOIRE TERMINÉE ===\n")