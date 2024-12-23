# SCI 1402 - Rapport de mi-parcours (TN2)
## Analyse de la base de données mondiale sur le terrorisme (GTD)

### 1. État d'avancement du projet

#### 1.1 Configuration et préparation
- Installation réussie de l'environnement de développement
- Configuration de 41 packages spécialisés incluant :
  - Data manipulation : tidyverse, data.table
  - Visualisation : ggplot2, plotly, corrplot
  - Analyse géographique : maps, leaflet
  - Analyse statistique : stats, scales

#### 1.2 Import et nettoyage des données
**Statistiques initiales :**
- 181,691 incidents terroristes
- 135 variables
- Période : 1970-2017

**Traitement des données :**
- Nettoyage des valeurs manquantes significatives :
  - location (126,196 NA)
  - summary (66,129 NA)
  - approxdate (172,452 NA)
- Variables clés sélectionnées :
  - Identification : eventid
  - Temporel : year, month, day
  - Géographique : country, region, state, city, latitude, longitude
  - Descriptif : attack_type, target
  - Impact : killed, wounded
  - Résultat : success

### 2. Analyses réalisées

#### 2.1 Statistiques descriptives globales
- **Nombre total d'incidents :** 181,593
- **Victimes :**
  - Tués : 411,617
  - Blessés : 523,661
- **Moyenne par incident :**
  - Tués : 2.403
  - Blessés : 3.168

#### 2.2 Analyses temporelles
- **Distribution annuelle :**
  - Augmentation significative depuis 2000
  - Pics observés en 2014-2016
  - Tendance générale à la hausse sur la période récente

#### 2.3 Analyse géographique
- **Couverture mondiale :**
  - Latitude : -53.16° à 74.63°
  - Longitude : données couvrant tous les continents
- **Points chauds identifiés :**
  - Moyen-Orient
  - Asie du Sud
  - Afrique du Nord
  - Amérique latine

#### 2.4 Types d'attaques
- Distribution par type d'attaque réalisée
- Évolution temporelle des méthodes analysée
- Corrélation avec les impacts établie

### 3. Visualisations développées

#### 3.1 Cartes et distributions spatiales
- Carte mondiale des incidents
- Densité des attaques par région
- Points chauds géographiques

#### 3.2 Analyses temporelles
- Évolution annuelle des incidents
- Tendances par région
- Patterns saisonniers

#### 3.3 Analyses d'impact
- Distribution des victimes
- Relation tués/blessés
- Impact par type d'attaque

### 4. Défis rencontrés et solutions

#### 4.1 Défis techniques
1. **Volume de données**
   - Solution : Utilisation de data.table et dplyr pour l'efficacité
   - Optimisation des requêtes d'agrégation

2. **Données manquantes**
   - Solution : Stratégie de nettoyage adaptative
   - Conservation maximale d'informations utiles

#### 4.2 Optimisations réalisées
- Vectorisation des opérations
- Utilisation de fonctions optimisées pour les grands ensembles de données
- Structures de données efficaces pour les analyses géographiques

### 5. Prochaines étapes

#### 5.1 Analyses planifiées
1. **Modélisation prédictive**
   - Préparation des features
   - Sélection des algorithmes
   - Framework d'évaluation

2. **Analyses avancées**
   - Clustering des incidents
   - Analyse des séries temporelles
   - Modèles de propagation

#### 5.2 Développements prévus
1. **Dashboard interactif**
   - Interface utilisateur avec Shiny
   - Visualisations interactives
   - Filtres dynamiques

2. **Documentation**
   - Documentation technique
   - Guide utilisateur
   - Rapport méthodologique

### 6. Conclusion
Les analyses préliminaires révèlent des patterns significatifs dans les données GTD. La prochaine phase se concentrera sur la modélisation prédictive et le développement d'outils interactifs de visualisation.

### Annexe : Métriques clés
```r
Dimensions des données :
- Lignes : 181,593
- Colonnes : 135
- Période : 1970-2017

Couverture géographique :
- Pays : > 200
- Régions : 12
- Coordonnées : disponibles pour 97.5% des incidents
```
