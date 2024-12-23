# Projet en science des données (SCI 1402)
# Projet d'analyse GTD (Global Terrorism Database)

## Description
Ce projet analyse la base de données mondiale sur le terrorisme (GTD) dans le cadre du cours SCI1402 à la TÉLUQ. L'objectif est d'explorer les tendances et patterns des activités terroristes entre 1970 et 2017.

## Structure du projet

Projet_SCI1402_analyse_GTD/
├── data/
│   ├── raw/        # Données brutes
│   ├── processed/  # Données nettoyées
│   └── temp/       # Fichiers temporaires
├── scripts/
│   ├── 01_installation_packages.R
│   ├── 02_configuration_environnement.R
│   ├── 03_configuration_spark.R
│   ├── 04_data_import_spark.R
│   ├── 05_data_import_spark.R
│   ├── 06_nattoyage_data.R  
│   ├── 07_analyse_statistique.R  
│   ├── 08_analyse_exploratoire.R
│   ├── 09_clustering_terrorisme.R
│   ├── 10_modelisation_predictive.R
│   └── 19_visualisation_interactive.R
├── results/   
├── visualizations/ # maps
├── outputs/        # Figures
│   ├── figures/    # Graphiques
│   ├── tables/     # Tableaux de résultats
│   └── models/     # Modèles sauvegardés
└── docs/
    ├── methodology/  # Documentation méthodologique
    ├── references/   # Sources et références
    └── rapports/     
              ├── methodology/  # Plan de projet documenté
              ├── references/   # Rapport mi-parcours
              └── rapports/ # Rapports Produit final fonctionnel


##  Utilisation:
1. Cloner le repository
2. Installer les dépendances R requises 
3. Exécuter les scripts dans l'ordre numérique

## Données
Les données GTD sont disponibles sur Kaggle. Le jeu de données couvre la période 1970-2017.

## Auteur
[HAMID DIGORDORD]
## Encadrement : Fatima Bensalma
Université TÉLUQ - Cours SCI1402 
