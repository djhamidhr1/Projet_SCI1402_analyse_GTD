# Chargement des bibliothèques nécessaires
library(tidyverse)
library(cluster)
library(scales)
library(RColorBrewer)
library(gridExtra)

# Configuration des paramètres graphiques
theme_set(theme_minimal())

# Lecture des données
data <- read.csv("globalterrorismdb_0617dist.csv", encoding = "latin1")

# Suppression des valeurs aberrantes
data <- data %>%
  filter(nkill <= 4, nwound <= 7) %>%
  as_tibble()

# Sélection des caractéristiques pour le clustering
features <- c(
  "longitude",
  "latitude",
  "nwound",
  "nkill",
  "natlty1_txt",
  "targtype1_txt",
  "targsubtype1_txt",
  "weaptype1_txt",
  "attacktype1_txt"
)

# Préparation des données pour le clustering
X <- data %>%
  select(all_of(features)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), ~ model.matrix(~.-1, data.frame(.)) %>% as.data.frame())) %>%
  select_if(function(x) var(x, na.rm = TRUE) > 0.05)

# Remplacement des NA par 0
X[is.na(X)] <- 0

# K-means clustering
set.seed(42)
km <- kmeans(scale(X), centers = 5, nstart = 25)

# Ajout des clusters aux données
data$Cluster <- km$cluster
data$ClusterName <- paste("Cluster", km$cluster)

# Préparation des données pour le profilage
numerical_cols <- names(data)[sapply(data, is.numeric)]
exclude_cols <- c(
  "eventid", "Cluster", "region", "country", "iyear",
  "natlty1", "natlty2", "natlty3", "imonth", "iday",
  "guncertain1", "guncertain2", "guncertain3"
)

# Suppression des colonnes contenant certains mots-clés
exclude_cols <- c(
  exclude_cols,
  grep("type|mode|ransom", names(data), value = TRUE)
)

X_profiling <- data %>%
  select(all_of(setdiff(numerical_cols, exclude_cols))) %>%
  mutate(across(everything(), ~scale(.))) %>%
  mutate(ClusterName = data$ClusterName)

# Visualisation 1: Heatmap des moyennes par cluster
cluster_means <- X_profiling %>%
  select(-longitude, -latitude) %>%
  group_by(ClusterName) %>%
  summarise(across(everything(), mean)) %>%
  column_to_rownames("ClusterName") %>%
  t()

heatmap_plot <- function(data_matrix) {
  heatmap(
    data_matrix,
    col = colorRampPalette(c("blue", "white", "red"))(100),
    margins = c(10, 10),
    main = "Moyenne des caractéristiques par cluster"
  )
}

# Visualisation 2: Scatter plot géographique par cluster
geo_plot <- ggplot(X_profiling, aes(x = longitude, y = latitude, color = ClusterName)) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Distribution géographique des clusters")

# Analyse des types d'attaque par cluster
attack_types <- data %>%
  select(attacktype1_txt, ClusterName) %>%
  table() %>%
  prop.table(margin = 2) %>%
  as.data.frame() %>%
  ggplot(aes(x = ClusterName, y = attacktype1_txt, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(title = "Types d'attaque par cluster")

# Affichage des visualisations
par(mfrow = c(2, 2))
heatmap_plot(cluster_means)
print(geo_plot)
print(attack_types)

# Calcul de la similarité entre clusters et régions
similarity <- mean(data$region_txt == data$ClusterName) * 100
cat("Similarité entre les labels de cluster et de région:", round(similarity, 2), "%\n")
