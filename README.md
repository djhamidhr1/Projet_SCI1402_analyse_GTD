# Projet_SCI1402_analyse_GTD

## 🔍 Analyse de la Base de Données Mondiale sur le terrorisme (GTD)

### Description du Projet
Analyse approfondie de la Global Terrorism Database (1970-2017), comprenant l'exploration des données, l'analyse statistique, la visualisation et la modélisation prédictive. L'objectif est d'identifier les tendances et facteurs de risque liés au terrorisme mondial.

### Caractéristiques de l'Étude
* Analyse de 181,691 incidents terroristes
* Couverture de 200 pays
* Période : 1970-2017
* Traitement de 172,452 dates approximatives
* Analyse de 4,556 coordonnées géographiques
* 12 régions principales

### Technologies Utilisées
* **Langages** : R, Python, HTML
* **Environnements** : RStudio, Google Colab
* **Frameworks** :
  * Tidyverse (traitement données)
  * ggplot2 (visualisation)
  * Shiny (tableaux interactifs)
  * Apache Spark (gestion des données volumineuses)
* **Contrôle de version** : Git/GitHub

### Structure du Projet
```
data/                       # Données brutes et traitées
├── 01_preparation/        
├── 02_analyse/           
└── 03_resultats/         

scripts/
├── 01_installation_packages.R
├── 02_configuration_environnement.R
├── 03_configuration_spark.R
├── 04_data_import_spark.R
├── 05_analyse_exploratoire.R
├── 06_clustering_terrorisme.R
├── 07_modelisation_predictive.R
└── 08_visualisation.R

docs/
├── methodology/           # Documentation
├── references/           # Sources et références
└── rapports/             # Rapports finaux
```

### Installation et Utilisation

#### 1. Cloner le dépôt
```bash
git clone https://github.com/djhamidhr1/Projet_SCI1402_analyse_GTD.git
```

#### 2. Installer les dépendances R
```R
source("scripts/01_installation_packages.R")
```

#### 3. Configurer l'environnement
```R
source("scripts/02_configuration_environnement.R")
source("scripts/03_configuration_spark.R")
```

#### 4. Exécuter les analyses
```R
source("scripts/04_data_import_spark.R")
```

### Contact
Réalisé par HAMID DIGORGORD  
Ressource à l'encadrement : Fatima Bensalma
TÉLUQ - 2024


