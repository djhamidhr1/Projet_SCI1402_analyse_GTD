# ==========================
# 1. CHARGEMENT DES BIBLIOTHÈQUES
# ==========================
library(dplyr)          # Manipulation de données
library(ggplot2)        # Visualisations
library(tidyr)          # Transformation des données
library(gridExtra)      # Affichage de plusieurs graphiques
library(RColorBrewer)   # Palettes de couleurs
library(plotly)         # Graphiques interactifs

# ==========================
# 2. CHARGEMENT DES DONNÉES
# ==========================
cat("\n=== CHARGEMENT DES DONNÉES ===\n")
data_path <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv"

if (file.exists(data_path)) {
  gtd_data <- read.csv(data_path, stringsAsFactors = FALSE)
  cat("Données chargées avec succès !\n")
} else {
  stop("Erreur : fichier non trouvé.")
}

# ==========================
# 3. ANALYSE DES DONNÉES
# ==========================

# 3.1 Attaques par région
gtd_data_summary <- gtd_data %>%
  group_by(region_txt) %>%
  summarise(attack_count = n()) %>%
  arrange(desc(attack_count))

print(gtd_data_summary)

# 3.2 Tableau croisé pour les attaques par type et région
by_region2 <- gtd_data %>%
  count(region_txt, attacktype1_txt) %>%
  pivot_wider(names_from = attacktype1_txt, values_from = n, values_fill = 0)

# ==========================
# 4. VISUALISATION DES DONNÉES
# ==========================

# 4.1 Histogramme des attaques par région
ggplot(gtd_data_summary, aes(x = reorder(region_txt, -attack_count), y = attack_count, fill = region_txt)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Number of terrorist attacks by region", x = "", y = "Number of attacks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 9))

# 4.2 Graphique empilé : Attaques par type et région
by_region_long <- by_region2 %>%
  pivot_longer(cols = -region_txt, names_to = "attacktype1_txt", values_to = "count")

ggplot(by_region_long, aes(x = count, y = reorder(region_txt, count), fill = attacktype1_txt)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Terrorist attacks by type and region", x = "Number of attacks", y = "") +
  scale_fill_brewer(palette = "Paired", name = "Attack Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

# ==========================
# 5. GRAPHIQUES VIOLINS
# ==========================
p1 <- ggplot(gtd_data, aes(x = factor(suicide), y = iyear, fill = factor(suicide))) +
  geom_violin(trim = FALSE) +
  labs(title = "Suicidal attacks", x = "Suicide", y = "Year") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

p2 <- ggplot(gtd_data, aes(x = factor(success), y = iyear, fill = factor(success))) +
  geom_violin(trim = FALSE) +
  labs(title = "Attack outcome", x = "Success", y = "Year") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

p3 <- ggplot(gtd_data, aes(x = factor(individual), y = iyear, fill = factor(individual))) +
  geom_violin(trim = FALSE) +
  labs(title = "Individual attacks", x = "Individual", y = "Year") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

# Affichage des graphiques violins
grid.arrange(p1, p2, p3, ncol = 1)

# ==========================
# 6. GRAPHES KDE
# ==========================
ggplot(gtd_data, aes(x = iyear, fill = factor(success), color = factor(success))) +
  geom_density(alpha = 0.5) +
  labs(title = "Success rate of an attack over time", x = "Year", y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# ==========================
# 7. CAMEMBERTS
# ==========================
# Préparation des données
attack_data <- gtd_data %>%
  count(attacktype1_txt) %>%
  mutate(attacktype1_txt = ifelse(row_number() > 5, "Other", attacktype1_txt)) %>%
  group_by(attacktype1_txt) %>%
  summarise(n = sum(n))

weapon_data <- gtd_data %>%
  count(weaptype1_txt) %>%
  mutate(weaptype1_txt = ifelse(row_number() > 5, "Melee and other", weaptype1_txt)) %>%
  group_by(weaptype1_txt) %>%
  summarise(n = sum(n))

# Camembert : attaques par type
ggplot(attack_data, aes(x = "", y = n, fill = attacktype1_txt)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Terrorist attacks by type", fill = "Attack Type") +
  geom_text(aes(label = paste0(n, " (", round(100 * n / sum(n), 1), "%)")), position = position_stack(vjust = 0.5)) +
  theme_void()

# Camembert : armes par type
ggplot(weapon_data, aes(x = "", y = n, fill = weaptype1_txt)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Terrorist weapons by type", fill = "Weapon Type") +
  geom_text(aes(label = paste0(n, " (", round(100 * n / sum(n), 1), "%)")), position = position_stack(vjust = 0.5)) +
  theme_void()

# ==========================
# 8. MESSAGE FINAL
# ==========================
cat("Toutes les analyses et visualisations ont été générées avec succès !\n")
