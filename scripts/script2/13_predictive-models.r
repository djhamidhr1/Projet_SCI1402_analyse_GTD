# ==============================================================================
#                    MODÉLISATION PRÉDICTIVE DES ATTAQUES TERRORISTES
# ==============================================================================

library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(ROCR)
library(e1071)
library(nnet)
library(plotly)
library(viridis)

# -----------------------------------------------------------------------------
#                    1. PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------

prepare_data_for_modeling <- function(data) {
    message("Préparation des données pour la modélisation...")
    
    # Sélection et préparation des variables
    model_data <- data %>%
        mutate(
            # Variable cible : succès de l'attaque (binaire)
            success = as.factor(success),
            # Variables temporelles
            month = as.factor(month),
            weekday = as.factor(weekday),
            # Variables catégorielles
            region = as.factor(region),
            country = as.factor(country),
            attack_type = as.factor(attack_type),
            target_type = as.factor(target_type),
            weapon_type = as.factor(weapon_type)
        ) %>%
        # Sélection des variables pour le modèle
        select(success, year, month, weekday, region, country, 
               attack_type, target_type, weapon_type, 
               latitude, longitude, killed, wounded)
    
    # Gestion des valeurs manquantes
    model_data <- model_data %>%
        mutate_if(is.numeric, ~replace_na(., median(., na.rm = TRUE))) %>%
        mutate_if(is.factor, ~replace_na(., "Unknown"))
    
    return(model_data)
}

# -----------------------------------------------------------------------------
#                    2. DIVISION DES DONNÉES
# -----------------------------------------------------------------------------

split_data <- function(data, train_ratio = 0.8) {
    message("Division des données en ensembles d'entraînement et de test...")
    
    # Création de l'index pour la division
    set.seed(42)
    train_index <- createDataPartition(data$success, p = train_ratio, list = FALSE)
    
    # Division des données
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
    
    return(list(train = train_data, test = test_data))
}

# -----------------------------------------------------------------------------
#                    3. ENTRAÎNEMENT DES MODÈLES
# -----------------------------------------------------------------------------

train_models <- function(train_data, test_data) {
    message("Entraînement des modèles...")
    
    # Configuration du contrôle d'entraînement
    ctrl <- trainControl(
        method = "cv",
        number = 5,
        classProbs = TRUE,
        summaryFunction = twoClassSummary
    )
    
    # 1. Régression Logistique
    message("Entraînement du modèle de régression logistique...")
    logistic_model <- train(
        success ~ .,
        data = train_data,
        method = "glm",
        family = "binomial",
        trControl = ctrl,
        metric = "ROC"
    )
    
    # 2. Random Forest
    message("Entraînement du modèle Random Forest...")
    rf_model <- train(
        success ~ .,
        data = train_data,
        method = "rf",
        trControl = ctrl,
        metric = "ROC",
        ntree = 100
    )
    
    # 3. XGBoost
    message("Entraînement du modèle XGBoost...")
    xgb_model <- train(
        success ~ .,
        data = train_data,
        method = "xgbTree",
        trControl = ctrl,
        metric = "ROC"
    )
    
    # 4. Neural Network
    message("Entraînement du réseau de neurones...")
    nnet_model <- train(
        success ~ .,
        data = train_data,
        method = "nnet",
        trControl = ctrl,
        metric = "ROC",
        trace = FALSE
    )
    
    return(list(
        logistic = logistic_model,
        random_forest = rf_model,
        xgboost = xgb_model,
        neural_net = nnet_model
    ))
}

# -----------------------------------------------------------------------------
#                    4. ÉVALUATION DES MODÈLES
# -----------------------------------------------------------------------------

evaluate_models <- function(models, test_data) {
    message("Évaluation des modèles...")
    
    # Fonction pour calculer les métriques
    calculate_metrics <- function(model, name) {
        # Prédictions
        pred <- predict(model, test_data)
        prob <- predict(model, test_data, type = "prob")
        
        # Matrice de confusion
        conf_matrix <- confusionMatrix(pred, test_data$success)
        
        # Courbe ROC
        pred_obj <- prediction(prob[,2], test_data$success)
        perf_obj <- performance(pred_obj, "tpr", "fpr")
        auc <- performance(pred_obj, "auc")@y.values[[1]]
        
        # Métriques
        metrics <- data.frame(
            Model = name,
            Accuracy = conf_matrix$overall["Accuracy"],
            Precision = conf_matrix$byClass["Precision"],
            Recall = conf_matrix$byClass["Recall"],
            F1_Score = conf_matrix$byClass["F1"],
            AUC = auc
        )
        
        return(list(
            metrics = metrics,
            roc_curve = list(
                fpr = perf_obj@x.values[[1]],
                tpr = perf_obj@y.values[[1]]
            )
        ))
    }
    
    # Calcul des métriques pour chaque modèle
    evaluations <- list(
        logistic = calculate_metrics(models$logistic, "Régression Logistique"),
        random_forest = calculate_metrics(models$random_forest, "Random Forest"),
        xgboost = calculate_metrics(models$xgboost, "XGBoost"),
        neural_net = calculate_metrics(models$neural_net, "Réseau de Neurones")
    )
    
    return(evaluations)
}

# -----------------------------------------------------------------------------
#                    5. VISUALISATION DES RÉSULTATS
# -----------------------------------------------------------------------------

visualize_results <- function(evaluations) {
    message("Création des visualisations des résultats...")
    
    # Compilation des métriques
    all_metrics <- bind_rows(
        evaluations$logistic$metrics,
        evaluations$random_forest$metrics,
        evaluations$xgboost$metrics,
        evaluations$neural_net$metrics
    )
    
    # Graphique des métriques
    metrics_plot <- all_metrics %>%
        gather(key = "Metric", value = "Value", -Model) %>%
        ggplot(aes(x = Model, y = Value, fill = Metric)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis_d() +
        labs(title = "Comparaison des Performances des Modèles",
             y = "Valeur",
             x = "Modèle")
    
    # Courbes ROC
    roc_data <- data.frame()
    for(model_name in names(evaluations)) {
        roc_data <- bind_rows(
            roc_data,
            data.frame(
                Model = model_name,
                FPR = evaluations[[model_name]]$roc_curve$fpr,
                TPR = evaluations[[model_name]]$roc_curve$tpr
            )
        )
    }
    
    roc_plot <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
        geom_line() +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        theme_minimal() +
        scale_color_viridis_d() +
        labs(title = "Courbes ROC",
             x = "Taux de Faux Positifs",
             y = "Taux de Vrais Positifs")
    
    return(list(metrics = metrics_plot, roc = roc_plot))
}

# -----------------------------------------------------------------------------
#                    6. IMPORTANCE DES VARIABLES
# -----------------------------------------------------------------------------

analyze_feature_importance <- function(models) {
    message("Analyse de l'importance des variables...")
    
    # Importance des variables pour Random Forest
    rf_importance <- varImp(models$random_forest)$importance
    rf_importance$Variable <- rownames(rf_importance)
    
    # Visualisation
    importance_plot <- rf_importance %>%
        arrange(Overall) %>%
        mutate(Variable = factor(Variable, levels = Variable)) %>%
        ggplot(aes(x = Variable, y = Overall)) +
        geom_bar(stat = "identity", fill = viridis(1)) +
        coord_flip() +
        theme_minimal() +
        labs(title = "Importance des Variables (Random Forest)",
             x = "Variables",
             y = "Importance")
    
    return(importance_plot)
}

# -----------------------------------------------------------------------------
#                    7. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Création du dossier pour les résultats
    dir.create("results/models", recursive = TRUE, showWarnings = FALSE)
    
    # Chargement des données
    message("Chargement des données...")
    data <- read_csv("data/processed/gtd_cleaned.csv")
    
    # Préparation des données
    model_data <- prepare_data_for_modeling(data)
    
    # Division des données
    data_split <- split_data(model_data)
    
    # Entraînement des modèles
    models <- train_models(data_split$train, data_split$test)
    
    # Évaluation des modèles
    evaluations <- evaluate_models(models, data_split$test)
    
    # Visualisation des résultats
    plots <- visualize_results(evaluations)
    importance_plot <- analyze_feature_importance(models)
    
    # Sauvegarde des résultats
    message("\nSauvegarde des résultats...")
    saveRDS(models, "results/models/trained_models.rds")
    
    # Sauvegarde des graphiques
    ggsave("results/models/metrics_comparison.png", plots$metrics,
           width = 12, height = 8)
    ggsave("results/models/roc_curves.png", plots$roc,
           width = 10, height = 8)
    ggsave("results/models/feature_importance.png", importance_plot,
           width = 10, height = 8)
    
    message("Analyse prédictive terminée avec succès!")
    
    return(list(
        models = models,
        evaluations = evaluations,
        plots = plots,
        importance = importance_plot
    ))
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}
