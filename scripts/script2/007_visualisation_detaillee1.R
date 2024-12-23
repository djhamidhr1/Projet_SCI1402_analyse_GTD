# ==============================================================================
#                    VISUALISATIONS DÉTAILLÉES 1
# ==============================================================================

# -----------------------------------------------------------------------------
#                    1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------
# Chargement des packages nécessaires
library(tidyverse)
library(corrplot)
library(gridExtra)
library(scales)
library(viridis)
library(maps)
library(ggplot2)
library(dplyr)

# Chargement des données
cat("\n=== CHARGEMENT DES DONNÉES ===\n")
data_path <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv"

# Lecture des données
gtd_data <- read_csv(data_path, show_col_types = FALSE)
cat("Données chargées avec succès !")

# -----------------------------------------------------------------------------
#                    2. DISTRIBUTIONS ET TENDANCES GÉNÉRALES
# -----------------------------------------------------------------------------
cat("\n\n=== CRÉATION DES VISUALISATIONS DE DISTRIBUTION ===\n")

# Distribution par type d'attaque
p1 <- ggplot(gtd_data, aes(x = attack_type)) +
  geom_bar(fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribution des Types d'Attaques",
       x = "Type d'attaque",
       y = "Nombre d'incidents")

print(p1)

# Évolution temporelle par type d'attaque
p2 <- gtd_data %>%
  count(year, attack_type) %>%
  ggplot(aes(x = year, y = n, color = attack_type)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Évolution des Types d'Attaques dans le Temps",
       x = "Année",
       y = "Nombre d'incidents",
       color = "Type d'attaque")

print(p2)

# -----------------------------------------------------------------------------
#                    3. ANALYSE GÉOGRAPHIQUE
# -----------------------------------------------------------------------------
cat("\n=== CRÉATION DES VISUALISATIONS GÉOGRAPHIQUES ===\n")

# Distribution par région
p3 <- ggplot(gtd_data, aes(x = region)) +
  geom_bar(fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Nombre d'Incidents par Région",
       x = "Région",
       y = "Nombre d'incidents")

print(p3)

# Carte des incidents
world <- map_data("world")
p4 <- ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "white", fill = "lightgray") +
  geom_point(data = gtd_data,
             aes(x = longitude, y = latitude, color = killed),
             alpha = 0.5) +
  scale_color_viridis() +
  theme_minimal() +
  labs(title = "Distribution Géographique des Incidents",
       color = "Nombre de victimes")

print(p4)

# -----------------------------------------------------------------------------
#                    4. ANALYSE DES VICTIMES
# -----------------------------------------------------------------------------
cat("\n=== CRÉATION DES VISUALISATIONS DES VICTIMES ===\n")

# Distribution des victimes
p5 <- ggplot(gtd_data, aes(x = killed)) +
  geom_histogram(bins = 50, fill = "darkred", alpha = 0.7) +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Distribution du Nombre de Victimes (échelle log)",
       x = "Nombre de victimes",
       y = "Fréquence")

print(p5)

# Victimes par région
p6 <- gtd_data %>%
  group_by(region) %>%
  summarize(total_killed = sum(killed, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(region, total_killed), y = total_killed)) +
  geom_col(fill = "darkred", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Nombre Total de Victimes par Région",
       x = "Région",
       y = "Nombre total de victimes")

print(p6)

# -----------------------------------------------------------------------------
#                    5. ANALYSE DE CORRÉLATION
# -----------------------------------------------------------------------------
cat("\n=== CRÉATION DES VISUALISATIONS DE CORRÉLATION ===\n")

# Matrice de corrélation
numeric_data <- gtd_data %>%
  select(killed, wounded, year) %>%
  cor(use = "complete.obs")

corrplot(numeric_data,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Matrice de Corrélation")

# Relation entre tués et blessés
p7 <- ggplot(gtd_data, aes(x = killed, y = wounded)) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_smooth(method = "lm", color = "blue") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Relation entre Nombre de Tués et de Blessés",
       x = "Nombre de tués (log)",
       y = "Nombre de blessés (log)")

print(p7)

# -----------------------------------------------------------------------------
#                    6. ANALYSE TEMPORELLE
# -----------------------------------------------------------------------------
cat("\n=== CRÉATION DES VISUALISATIONS TEMPORELLES ===\n")

# Tendance annuelle générale
p8 <- gtd_data %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "darkred") +
  geom_smooth(method = "loess", color = "blue") +
  theme_minimal() +
  labs(title = "Évolution du Nombre d'Incidents par Année",
       x = "Année",
       y = "Nombre d'incidents")

print(p8)

# Prétraitement des données : Regroupement par année
gtd_data_summary <- gtd_data %>%
  group_by(iyear) %>%
  summarise(number_of_attacks = n())  # Compter le nombre d'attaques par année

# Création du graphique
ggplot(data = gtd_data_summary, aes(x = factor(iyear), y = number_of_attacks)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Graphique en barres
  theme_minimal() +  # Utilisation d'un thème épuré
  labs(title = "Number of terrorist attacks by year, worldwide",
       x = "Year",
       y = "Number of attacks") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = comma)  # Format des chiffres avec séparateur de milliers


# Ajouter une colonne pour les couleurs 
gtd_data_summary <- gtd_data_summary %>%
  mutate(color = as.factor(iyear))

# Création le graphique avec couleur dégradée
ggplot(data = gtd_data_summary, aes(x = factor(iyear), y = number_of_attacks, fill = iyear)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_viridis(option = "plasma", direction = -1) +  # Palette dégradée
  labs(title = "Number of terrorist attacks by year, worldwide",
       x = "Year",
       y = "Number of attacks",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = comma)  # Format des nombres

# -----------------------------------------------------------------------------
#                    7. STATISTIQUES FINALES
# -----------------------------------------------------------------------------
cat("\n=== STATISTIQUES FINALES ===\n")
cat("Nombre total d'incidents:", nrow(gtd_data), "\n")
cat("Période couverte:", min(gtd_data$year), "-", max(gtd_data$year), "\n")
cat("Nombre total de victimes:", sum(gtd_data$killed, na.rm = TRUE), "\n")
cat("Nombre total de blessés:", sum(gtd_data$wounded, na.rm = TRUE), "\n")

cat("\n=== ANALYSE TERMINÉE ===\n")