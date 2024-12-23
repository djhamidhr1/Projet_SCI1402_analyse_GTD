---
title: "Analyse de la Base de Données sur le Terrorisme Global (GTD)"
author: "Votre Nom"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    toc_depth: 3
    theme: united
  pdf_document:
    toc: true
    toc_depth: 3
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 10,
  fig.height = 6
)

# Chargement des packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(viridis)
library(skimr)
library(tibble)
```

## Introduction

Ce rapport présente une analyse approfondie de la Global Terrorism Database (GTD). L'objectif est d'examiner les tendances du terrorisme mondial à travers différentes dimensions : géographique, temporelle, et par type d'attaque.

### Importation des Données

```{r import}
# Définir le chemin et importer les données
chemin <- "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/raw/globalterrorismdb_0718dist.csv"
donnees_gtd <- read.csv(chemin, encoding = "UTF-8")
```

### Aperçu des Données

```{r apercu}
# Dimensions du jeu de données
print("Dimensions du jeu de données :")
dim(donnees_gtd)

# Aperçu des premières lignes
print("Aperçu des données :")
head(donnees_gtd)
```

## Analyse Exploratoire

### Distribution des Types d'Attaques

```{r types_attaques}
# Distribution des types d'attaques
plot_attack_types <- donnees_gtd %>%
  ggplot(aes(x = fct_infreq(attacktype1_txt))) +
  geom_bar(fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Distribution des types d'attaques",
    x = "Type d'attaque",
    y = "Nombre d'incidents"
  ) +
  theme_minimal()

plot_attack_types
```

### Pays les Plus Touchés

```{r pays_touches}
# Top 20 des pays les plus touchés
plot_top_countries <- donnees_gtd %>%
  count(country_txt, sort = TRUE) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(country_txt, n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(
    title = "20 pays les plus touchés",
    x = "Pays",
    y = "Nombre d'incidents"
  ) +
  theme_minimal()

plot_top_countries
```

## Analyse Temporelle

### Évolution Annuelle des Incidents

```{r evolution_annuelle}
plot_yearly_trend <- donnees_gtd %>%
  count(iyear) %>%
  ggplot(aes(x = iyear, y = n)) +
  geom_line(color = "red", size = 1) +
  geom_smooth(method = "loess", color = "blue", alpha = 0.2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Évolution annuelle des incidents terroristes",
    x = "Année",
    y = "Nombre d'incidents"
  ) +
  theme_minimal()

plot_yearly_trend
```

### Tendances Mensuelles

```{r tendances_mensuelles}
monthly_trends <- donnees_gtd %>%
  group_by(iyear, imonth) %>%
  summarise(
    nb_incidents = n(),
    victimes = sum(nkill, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(imonth))

plot_monthly_trend <- monthly_trends %>%
  ggplot(aes(x = imonth, y = nb_incidents, group = iyear)) +
  geom_line(alpha = 0.5) +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal() +
  labs(
    title = "Tendances mensuelles des incidents terroristes",
    x = "Mois",
    y = "Nombre d'incidents"
  )

plot_monthly_trend
```

## Analyse des Impacts

### Statistiques Générales

```{r stats_generales}
# Statistiques générales
stats_general <- donnees_gtd %>%
  summarise(
    total_incidents = n(),
    total_killed = sum(nkill, na.rm = TRUE),
    total_wounded = sum(nwound, na.rm = TRUE),
    mean_killed = mean(nkill, na.rm = TRUE),
    median_killed = median(nkill, na.rm = TRUE),
    sd_killed = sd(nkill, na.rm = TRUE)
  )

print(stats_general)
```

### Analyse par Région

```{r stats_region}
stats_region <- donnees_gtd %>%
  group_by(region_txt) %>%
  summarise(
    incidents = n(),
    total_killed = sum(nkill, na.rm = TRUE),
    mean_killed = mean(nkill, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(incidents))

print(stats_region)
```

## Analyse des Groupes et Méthodes

### Groupes Terroristes les Plus Actifs

```{r groupes_terroristes}
top_groups <- donnees_gtd %>%
  filter(!is.na(gname) & gname != "Unknown") %>%
  group_by(gname) %>%
  summarise(
    nb_incidents = n(),
    total_victimes = sum(nkill, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(nb_incidents)) %>%
  slice_head(n = 15)

plot_top_groups <- top_groups %>%
  ggplot(aes(x = reorder(gname, nb_incidents), y = nb_incidents)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "15 Groupes terroristes les plus actifs",
    x = "Groupe",
    y = "Nombre d'incidents"
  )

plot_top_groups
```

### Types d'Armes Utilisées

```{r types_armes}
weapons_analysis <- donnees_gtd %>%
  group_by(weaptype1_txt) %>%
  summarise(
    nb_incidents = n(),
    total_victimes = sum(nkill, na.rm = TRUE),
    moyenne_victimes = mean(nkill, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(nb_incidents))

plot_weapons <- weapons_analysis %>%
  ggplot(aes(x = reorder(weaptype1_txt, nb_incidents), y = nb_incidents)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Types d'armes utilisées",
    x = "Type d'arme",
    y = "Nombre d'incidents"
  )

plot_weapons
```

## Conclusion

Cette analyse a permis de mettre en évidence plusieurs aspects importants du terrorisme mondial :
- Les tendances temporelles des incidents
- La distribution géographique des attaques
- Les types d'attaques les plus courants
- Les groupes les plus actifs et leurs méthodes

Ces informations peuvent être utiles pour mieux comprendre les dynamiques du terrorisme global et potentiellement aider à développer des stratégies de prévention plus efficaces.