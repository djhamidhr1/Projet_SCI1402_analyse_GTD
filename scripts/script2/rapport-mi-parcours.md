# SCI 1402 - Projet en science des données
# Rapport de mi-parcours (TN2)
## Analyse de la base de données mondiale sur le terrorisme (GTD)

**Préparé par :** HAMID DIGORDORD  
**No d'étudiant :** 23110186  
**Date :** Décembre 2024

**TÉLUQ**  
**Professeur responsable :** Richard Hotte  
**Ressource à l'encadrement :** Fatima Bensalma

---

## 1. État d'avancement du projet

### 1.1 Objectifs atteints
- Configuration complète de l'environnement de développement
- Installation des packages et dépendances nécessaires
- Import et nettoyage initial des données GTD
- Premières analyses exploratoires réalisées

### 1.2 Travaux réalisés

#### 1.2.1 Configuration de l'environnement
- Installation réussie de R, RStudio et Apache Spark
- Configuration du repository GitHub
- Mise en place de la structure du projet

#### 1.2.2 Préparation des données
- Téléchargement de la base GTD depuis Kaggle
- Nettoyage des valeurs manquantes et aberrantes
- Standardisation des formats de dates et localisations

#### 1.2.3 Analyses préliminaires
- Statistiques descriptives générales :
  - Nombre total d'incidents : XXX
  - Distribution temporelle des attaques
  - Répartition géographique
- Premières visualisations :
  - Évolution temporelle des attaques
  - Cartographie des incidents
  - Distribution des types d'attaques

## 2. Résultats intermédiaires

### 2.1 Analyses descriptives
```R
# Exemple de résultats statistiques
Total incidents : XXX
Période couverte : 1970-2017
Pays les plus touchés : [Liste]
Types d'attaques principaux : [Liste]
```

### 2.2 Visualisations préliminaires
[Insérer ici les principales visualisations réalisées]

### 2.3 Observations initiales
- Tendances temporelles identifiées
- Patterns géographiques observés
- Corrélations préliminaires découvertes

## 3. Difficultés rencontrées et solutions

### 3.1 Défis techniques
1. **Gestion des données volumineuses**
   - Problème : Limite de mémoire avec R standard
   - Solution : Utilisation d'Apache Spark pour le traitement

2. **Valeurs manquantes**
   - Problème : Données incomplètes pour certaines années
   - Solution : Stratégies de nettoyage et d'imputation

### 3.2 Ajustements méthodologiques
- Modifications apportées au plan initial
- Nouvelles approches adoptées

## 4. Prochaines étapes

### 4.1 Analyses prévues
- Développement des modèles prédictifs
- Analyses de clustering avancées
- Création du tableau de bord interactif

### 4.2 Planning actualisé
| Tâche | Début prévu | Fin prévue | État |
|-------|-------------|------------|------|
| Modélisation | Semaine 8 | Semaine 11 | À venir |
| Visualisations | Semaine 9 | Semaine 12 | En cours |
| Documentation | Semaine 10 | Semaine 14 | À venir |

## 5. Code et Documentation

### 5.1 Scripts développés
```R
# Exemple de script clé développé
library(tidyverse)
library(sparklyr)

# Configuration Spark
sc <- spark_connect(master = "local")

# Import des données
gtd_data <- spark_read_csv(sc, "gtd_data", 
                          "path/to/data.csv")

# Analyses de base
summary_stats <- gtd_data %>%
  group_by(year) %>%
  summarise(
    incidents = n(),
    casualties = sum(nkill, na.rm = TRUE)
  )
```

### 5.2 Documentation technique
- Structure des données
- Procédures de nettoyage
- Méthodologies d'analyse

## 6. Conclusion et perspectives

### 6.1 État global du projet
- Avancement conforme au planning initial
- Objectifs intermédiaires atteints
- Ajustements mineurs nécessaires

### 6.2 Prochaines étapes clés
1. Finalisation des analyses statistiques
2. Développement des modèles prédictifs
3. Création du tableau de bord
4. Rédaction du rapport final

## Annexes

### A. Références bibliographiques
1. Documentation GTD
2. Articles méthodologiques
3. Sources techniques

### B. Outputs techniques
- Logs d'exécution
- Statistiques détaillées
- Visualisations supplémentaires

---
*Rapport soumis dans le cadre du cours SCI 1402 - TÉLUQ*
