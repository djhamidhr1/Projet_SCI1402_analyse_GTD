# ==============================================================================
#                 IMPORTATION, ANALYSE DES DONNÉES AVEC R SANS SPARK
# ==============================================================================

# -----------------------------------------------------------------------------
#                1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------
# Chargement des packages nécessaires
library(tidyverse)   # Manipulation et analyse des données
library(ggplot2)     # Visualisation des données

# Configuration des chemins d'accès
file_path <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/raw/globalterrorismdb_0718dist.csv"

# -----------------------------------------------------------------------------
#                2. IMPORTATION ET NETTOYAGE DES DONNÉES
# -----------------------------------------------------------------------------
message("\n--- Importation des données ---")
data <- read_csv(file_path)

# Aperçu des données
message("\n--- Aperçu des données : 5 premières lignes ---")
print(head(data, 5))

# Afficher les noms des colonnes
message("\n--- Noms des colonnes ---")
print(colnames(data))

# Résumé statistique des données
message("\n--- Résumé statistique des données ---")
print(summary(data))

# Vérification des valeurs manquantes
message("\n--- Vérification des valeurs manquantes ---")
print(colSums(is.na(data)))

# Sélection des colonnes pertinentes pour l'analyse
data_clean <- data %>%
  select(
    eventid, year = iyear, month = imonth, day = iday, 
    country = country_txt, region = region_txt, state = provstate, 
    city, latitude, longitude, attack_type = attacktype1_txt,
    killed = nkill, wounded = nwound, target = targtype1_txt,
    group = gname, success
  )

# Suppression des lignes avec des valeurs manquantes
data_clean <- drop_na(data_clean)

# Vérification après nettoyage
message("\n--- Données après suppression des NA ---")
print(head(data_clean, 5))
print(colSums(is.na(data_clean)))

# -----------------------------------------------------------------------------
#                3. ANALYSE ET VISUALISATION DES DONNÉES
# -----------------------------------------------------------------------------

# 3.1 Tendance des attaques par région au fil des ans
message("\n--- Tendance des attaques par région ---")
attacks_by_year_region <- data_clean %>%
  group_by(year, region) %>%
  summarise(count = n(), .groups = "drop")

ggplot(attacks_by_year_region, aes(x = year, y = count, color = region)) +
  geom_line(size = 1) +
  labs(
    title = "Tendances des attaques terroristes par région (1970-2017)",
    x = "Année",
    y = "Nombre d'attaques",
    color = "Région"
  ) +
  theme_minimal()

# 3.2 Top 10 des pays les plus touchés
message("\n--- Top 10 des pays les plus touchés ---")
top_countries <- data_clean %>%
  count(country, sort = TRUE) %>%
  slice(1:10)

ggplot(top_countries, aes(x = reorder(country, -n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(
    title = "Top 10 des pays les plus touchés",
    x = "Pays",
    y = "Nombre d'attaques"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3.3 Nombre total d'attaques par région
message("\n--- Nombre total d'attaques par région ---")
attacks_by_region <- data_clean %>%
  count(region, sort = TRUE)

ggplot(attacks_by_region, aes(x = reorder(region, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Nombre total d'attaques par région",
    x = "Région",
    y = "Nombre d'attaques"
  ) +
  theme_minimal()

# 3.4 Types d'attaques spécifiques par région
message("\n--- Types d'attaques spécifiques par région ---")

# Fonction pour afficher les types d'attaques pour une région spécifique
plot_attack_types_by_region <- function(region_name) {
  region_data <- data_clean %>%
    filter(region == region_name) %>%
    count(attack_type, sort = TRUE)
  
  ggplot(region_data, aes(x = reorder(attack_type, n), y = n)) +
    geom_bar(stat = "identity", fill = "grey") +
    coord_flip() +
    labs(
      title = paste("Types d'attaques dans la région :", region_name),
      x = "Type d'attaque",
      y = "Nombre d'attaques"
    ) +
    theme_minimal()
}

# Exemple d'appel pour la région "Middle East & North Africa"
plot_attack_types_by_region("Middle East & North Africa")

# -----------------------------------------------------------------------------
#                4. FIN DU SCRIPT
# -----------------------------------------------------------------------------
message("\n=== Fin du traitement des données ===")
