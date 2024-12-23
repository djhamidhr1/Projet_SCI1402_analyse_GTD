# ==============================================================================
#                    CONFIGURATION DE L'ENVIRONNEMENT R
# ==============================================================================

# -----------------------------------------------------------------------------
#                    1. CHARGEMENT DES PACKAGES
# -----------------------------------------------------------------------------

cat("=== CHARGEMENT DES PACKAGES ===\n")

# 1.1 Data manipulation et visualisation
library(tidyverse)  # Inclut ggplot2, dplyr, tidyr, readr, etc.
library(scales)     # Formatage des échelles
library(gridExtra)  # Organisation des graphiques

# 1.2 Visualisation géographique
library(leaflet)    # Cartes interactives
library(sf)         # Données géographiques
library(maps)       # Données cartographiques
library(mapdata)    # Données supplémentaires

# 1.3 Palettes et couleurs
library(RColorBrewer)  # Palettes de couleurs
library(viridis)       # Palettes accessibles

# 1.4 Visualisation avancée
library(plotly)      # Graphiques interactifs
library(treemapify)  # Création de treemaps
library(corrplot)    # Matrices de corrélation
library(gganimate)   # Animations
library(animation)   # Support d'animation

# 1.5 Analyse statistique
library(cluster)     # Analyse de clustering
library(factoextra)  # Visualisation clusters

# 1.6 Manipulation temporelle
library(lubridate)   # Gestion des dates

# -----------------------------------------------------------------------------
#                    2. VÉRIFICATION DE L'ENVIRONNEMENT
# -----------------------------------------------------------------------------

cat("\n=== VÉRIFICATION DE L'ENVIRONNEMENT ===\n")

# 2.1 Affichage des packages chargés
cat("\nPackages actuellement chargés:\n")
print(.packages())

# 2.2 Vérification de la version de R
cat("\nVersion de R:", R.version.string, "\n")

# 2.3 Vérification du répertoire de travail
cat("\nRépertoire de travail:", getwd(), "\n")

# -----------------------------------------------------------------------------
#                    3. PARAMÈTRES GÉNÉRAUX
# -----------------------------------------------------------------------------

# 3.1 Configuration des options de base
options(
    digits = 4,              # Nombre de décimales
    stringsAsFactors = FALSE,# Ne pas convertir les chaînes en facteurs
    timeout = 120,           # Timeout pour les téléchargements
    encoding = "UTF-8"       # Encodage par défaut
)

# 3.2 Configuration de ggplot2 pour une meilleure visualisation
theme_set(theme_minimal())  # Thème par défaut pour ggplot2

cat("\n=== CONFIGURATION TERMINÉE ===\n")
