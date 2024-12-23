Projet_SCI1402_analyse_GTD
📊 Analyse de la Base de Données Mondiale sur le Terrorisme (GTD)
Description du Projet
Analyse approfondie de la Global Terrorism Database (1970-2017), comprenant l'exploration des données, l'analyse statistique, la visualisation et la modélisation prédictive. L'objectif est d'identifier les tendances et facteurs de risque liés au terrorisme mondial.
Caractéristiques de l'Étude

Analyse de 181,691 incidents terroristes
Couverture de 200 pays
Période : 1970-2017
Traitement de 172,452 dates approximatives
Analyse de 4,556 coordonnées géographiques
Étude de 12 régions principales

Technologies Utilisées

Langages : R, Python, Html
Environnements : RStudio, Google Colab
Frameworks :

tidyverse (traitement des données)
ggplot2 (visualisation)
shiny (tableaux de bord interactifs)
Apache Spark (gestion des données volumineuses)


Versioning : Git/GitHub

Structure du Projet
Projet_SCI1402_analyse_GTD/
├── data/
│   ├── raw/          # Données brutes
│   ├── processed/    # Données nettoyées
│   └── temp/         # Fichiers temporaires
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
│   └── 11_visualisation_interactive.R
├── results/
├── visualizations/   # Cartes
├── outputs/
│   ├── figures/     # Graphiques
│   ├── tables/      # Tableaux de résultats
│   └── models/      # Modèles sauvegardés
└── docs/
    ├── methodology/ # Documentation méthodologique
    ├── references/ # Sources et références
    └── rapports/   # Rapports finaux
    
Résultats Principaux

Taux de succès des attaques : 88.96%
Augmentation significative des incidents depuis 2000
Zones principales touchées : Moyen-Orient et Asie du Sud
Total des victimes : 411,617
Total des blessés : 523,661

Méthodologie

Nettoyage et préparation des données
Analyse exploratoire
Création de visualisations interactives
Développement de modèles prédictifs
Analyse statistique approfondie

Contexte Académique
Projet réalisé dans le cadre du cours SCI1402 - Science des données à la TÉLUQ.
Installation et Utilisation

Cloner le dépôt :

bashCopygit clone https://github.com/djhamidhr1/Projet_SCI1402_analyse_GTD.git

Installer les dépendances R :

RCopysource("scripts/01_installation_packages.R")

Configurer l'environnement :

RCopysource("scripts/02_configuration_environnement.R")
source("scripts/03_configuration_spark.R")

Exécuter les analyses :

RCopy# Suivre l'ordre numérique des scripts
source("scripts/04_data_import_spark.R")
...
source("scripts/11_visualisation_interactive.R")

Cloner le dépôt
Installer les dépendances R requises
Exécuter les scripts dans l'ordre indiqué
Consulter les résultats dans le dossier output

Contact
Réalisé par HAMID DIGORGORD
TÉLUQ - 2024
