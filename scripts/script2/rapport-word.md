# Université TÉLUQ
# SCI 1402 - Projet en science des données

# Rapport de mi-parcours (TN2)
## Analyse de la base de données mondiale sur le terrorisme (GTD)

**Présenté à :** Fatima Bensalma  
**Préparé par :** HAMID DIGORGORD  
**Code permanent :** 23110186  
**Date :** Décembre 2024

---

## Table des matières
1. [État d'avancement du projet](#1)
2. [Analyses réalisées](#2)
3. [Visualisations développées](#3)
4. [Défis et solutions](#4)
5. [Prochaines étapes](#5)
6. [Conclusion](#6)

---

## 1. État d'avancement du projet

### 1.1 Configuration et préparation

L'environnement de développement a été mis en place avec succès, comprenant l'installation et la configuration de :

- **Outils de développement :**
  - R et RStudio
  - Git pour le versioning
  - Packages spécialisés (41 au total)

- **Bibliothèques principales :**
  - tidyverse pour la manipulation des données
  - ggplot2 pour la visualisation
  - maps et leaflet pour l'analyse géographique
  - stats et scales pour l'analyse statistique

### 1.2 Import et traitement des données

Les données de la GTD ont été importées et nettoyées :

**Statistiques initiales :**
- 181,691 incidents terroristes
- 135 variables
- Période couverte : 1970-2017

**Traitement réalisé :**
- Nettoyage des valeurs manquantes
- Standardisation des formats
- Validation des coordonnées géographiques

## 2. Analyses réalisées

### 2.1 Statistiques descriptives

**Métriques globales :**
- Nombre total d'incidents : 181,593
- Total des victimes :
  - Décès : 411,617
  - Blessés : 523,661

**Moyennes par incident :**
- Victimes : 2.403 personnes
- Blessés : 3.168 personnes

### 2.2 Analyses temporelles

L'analyse temporelle révèle :
- Une augmentation significative des incidents depuis 2000
- Des pics d'activité en 2014-2016
- Une tendance générale à la hausse sur la période récente

### 2.3 Distribution géographique

La couverture géographique montre :
- Une présence sur tous les continents
- Des zones particulièrement touchées :
  - Moyen-Orient
  - Asie du Sud
  - Afrique du Nord
  - Amérique latine

## 3. Visualisations développées

### 3.1 Cartographie

Développement de plusieurs types de cartes :
- Distribution mondiale des incidents
- Cartes de chaleur par région
- Visualisation des points chauds

### 3.2 Graphiques temporels

Création de visualisations montrant :
- Évolution annuelle des attaques
- Tendances régionales
- Patterns saisonniers

### 3.3 Analyses d'impact

Visualisations des impacts incluant :
- Distribution des victimes
- Corrélation tués/blessés
- Impact par type d'attaque

## 4. Défis et solutions

### 4.1 Défis techniques

1. **Gestion du volume de données**
   - Solution : Optimisation des requêtes
   - Utilisation de structures de données efficaces

2. **Traitement des données manquantes**
   - Solution : Stratégies de nettoyage adaptées
   - Conservation maximale d'informations utiles

### 4.2 Optimisations

- Vectorisation des opérations
- Structures de données optimisées
- Requêtes efficaces

## 5. Prochaines étapes

### 5.1 Analyses prévues

1. **Modélisation prédictive**
   - Préparation des caractéristiques
   - Sélection des algorithmes
   - Développement des modèles

2. **Analyses avancées**
   - Clustering des incidents
   - Analyse des séries temporelles
   - Modèles de propagation

### 5.2 Livrables planifiés

1. **Dashboard interactif**
   - Interface utilisateur Shiny
   - Visualisations interactives
   - Filtres dynamiques

2. **Documentation**
   - Guide technique
   - Manuel utilisateur
   - Rapport méthodologique

## 6. Conclusion

Les analyses préliminaires ont permis d'identifier des patterns significatifs dans les données GTD. La prochaine phase du projet se concentrera sur la modélisation prédictive et le développement d'outils de visualisation interactifs.

## Annexe : Statistiques clés

```
Dimensions des données :
- Observations : 181,593
- Variables : 135
- Période : 1970-2017

Couverture géographique :
- Pays : > 200
- Régions : 12
- Géolocalisation : 97.5% des incidents
```

---

**Références**

1. Documentation GTD
2. Documentation des packages R utilisés
3. Méthodologie d'analyse terroriste

