# === MODÉLISATION PRÉDICTIVE DES ATTAQUES TERRORISTES ===

# Chargement des bibliothèques nécessaires
library(tidyverse)    # Pour la manipulation des données
library(caret)        # Pour la division des données et les métriques
library(randomForest) # Pour le modèle Random Forest
library(xgboost)      # Pour le modèle XGBoost
library(ROCR)         # Pour les courbes ROC
library(nnet)         # Pour le modèle Neural Network
library(plotly)       # Pour les graphiques interactifs
library(viridis)      # Pour les palettes de couleurs
library(readr)        # Pour la lecture des fichiers CSV

# === 1. CHARGEMENT DES DONNÉES ===
cat("\n=== CHARGEMENT DES DONNÉES ===\n")
# Lecture du fichier CSV avec read_csv
gtd_data <- read_csv("C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv", 
                     show_col_types = FALSE)
cat("Données chargées avec succès !\n")

# Aperçu des premières lignes
head(gtd_data)

# === 2. PRÉPARATION DES DONNÉES ===
cat("\n=== PRÉPARATION DES DONNÉES ===\n")
# Sélectionner la cible (success) et les variables prédictives importantes
# "success" est la variable cible binaire (1 = succès, 0 = échec)

data <- gtd_data %>%
  select(success, iyear, region, attacktype1, targtype1, natlty1) %>%
  drop_na()  # Retirer les lignes avec des valeurs manquantes

# Vérifier les niveaux des facteurs et supprimer les variables avec un seul niveau
data <- data %>% mutate(across(where(is.character), as.factor))
data <- data %>% select_if(~!is.factor(.) || nlevels(.) > 1)

# Assurer que la variable 'success' est binaire
data$success <- as.factor(ifelse(data$success == TRUE, 1, 0))

# Diviser les données en train (70%) et test (30%)
set.seed(123)  # Pour la reproductibilité
train_index <- createDataPartition(data$success, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# === 3. ENTRAÎNEMENT DES MODÈLES ===
cat("\n=== ENTRAÎNEMENT DES MODÈLES ===\n")

# 3.1 Régression Logistique
cat("\n--- Modèle : Régression Logistique ---\n")
logistic_model <- glm(success ~ ., data = train_data, family = binomial)
logistic_pred <- predict(logistic_model, test_data, type = "response")
logistic_pred_class <- ifelse(logistic_pred > 0.5, 1, 0)

# 3.2 Random Forest
cat("\n--- Modèle : Random Forest ---\n")
rf_model <- randomForest(success ~ ., data = train_data, ntree = 100)
rf_pred <- predict(rf_model, test_data, type = "class")

# 3.3 XGBoost
cat("\n--- Modèle : XGBoost ---\n")
# Conversion en format matriciel pour XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% select(-success)), label = as.numeric(train_data$success) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(-success)))

xgb_model <- xgboost(data = train_matrix, max.depth = 3, eta = 0.1, nrounds = 100, objective = "binary:logistic")
xgb_pred <- predict(xgb_model, test_matrix)
xgb_pred_class <- ifelse(xgb_pred > 0.5, 1, 0)

# 3.4 Neural Network
cat("\n--- Modèle : Neural Network ---\n")
nn_model <- nnet(success ~ ., data = train_data, size = 5, trace = FALSE)
nn_pred <- predict(nn_model, test_data, type = "raw")
nn_pred_class <- ifelse(nn_pred > 0.5, 1, 0)

# === 4. CALCUL DES MÉTRIQUES ===
cat("\n=== CALCUL DES MÉTRIQUES ===\n")
# Fonction pour calculer les métriques (Accuracy, Precision, Recall, F1)
calculate_metrics <- function(actual, predicted) {
  confusion <- confusionMatrix(as.factor(predicted), as.factor(actual))
  list(
    Accuracy = round(confusion$overall['Accuracy'], 3),
    Precision = round(confusion$byClass['Pos Pred Value'], 3),
    Recall = round(confusion$byClass['Sensitivity'], 3),
    F1 = round(2 * (confusion$byClass['Pos Pred Value'] * confusion$byClass['Sensitivity']) /
                 (confusion$byClass['Pos Pred Value'] + confusion$byClass['Sensitivity']), 3)
  )
}

# Calculer les métriques pour chaque modèle
metrics <- list(
  Logistic_Regression = calculate_metrics(test_data$success, logistic_pred_class),
  Random_Forest = calculate_metrics(test_data$success, rf_pred),
  XGBoost = calculate_metrics(test_data$success, xgb_pred_class),
  Neural_Network = calculate_metrics(test_data$success, nn_pred_class)
)

print(metrics)

# === 5. VISUALISATION DES RÉSULTATS ===
cat("\n=== VISUALISATION DES RÉSULTATS ===\n")

# Afficher les métriques dans un graphique interactif
metrics_df <- do.call(rbind, metrics) %>%
  as.data.frame() %>%
  rownames_to_column("Modèle")

plot_ly(metrics_df, x = ~Modèle, y = ~Accuracy, type = 'bar', name = 'Accuracy') %>%
  add_trace(y = ~Precision, name = 'Precision') %>%
  add_trace(y = ~Recall, name = 'Recall') %>%
  add_trace(y = ~F1, name = 'F1') %>%
  layout(title = "Performance des modèles",
         barmode = 'group',
         xaxis = list(title = "Modèles"),
         yaxis = list(title = "Score"))

# === FIN DU SCRIPT ===
cat("\n=== MODÉLISATION TERMINÉE ===\n")
