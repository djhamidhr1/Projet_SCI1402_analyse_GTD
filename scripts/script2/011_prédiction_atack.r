# ==============================================================================
#                    PRÉDICTION DES FUTURES ATTAQUES TERRORISTES
# ==============================================================================

library(tidyverse)
library(forecast)
library(tsibble)
library(prophet)
library(zoo)
library(plotly)
library(viridis)
library(lubridate)

# -----------------------------------------------------------------------------
#                    1. PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------

prepare_time_series_data <- function(data) {
    message("Préparation des données temporelles...")
    
    # Agrégation mensuelle
    monthly_data <- data %>%
        mutate(date = make_date(year, month, 1)) %>%
        group_by(date) %>%
        summarise(
            attacks = n(),
            casualties = sum(killed, na.rm = TRUE),
            .groups = "drop"
        )
    
    # Agrégation par région
    regional_data <- data %>%
        mutate(date = make_date(year, month, 1)) %>%
        group_by(date, region) %>%
        summarise(
            attacks = n(),
            casualties = sum(killed, na.rm = TRUE),
            .groups = "drop"
        )
    
    list(
        monthly = monthly_data,
        regional = regional_data
    )
}

# -----------------------------------------------------------------------------
#                    2. MODÈLES DE PRÉDICTION
# -----------------------------------------------------------------------------

# Modèle ARIMA
train_arima_model <- function(data, horizon = 12) {
    message("Entraînement du modèle ARIMA...")
    
    # Création de la série temporelle
    ts_data <- ts(data$attacks, 
                  frequency = 12,
                  start = c(year(min(data$date)), month(min(data$date))))
    
    # Ajustement automatique du modèle ARIMA
    model <- auto.arima(ts_data, seasonal = TRUE)
    
    # Prédictions
    predictions <- forecast(model, h = horizon)
    
    return(list(
        model = model,
        predictions = predictions
    ))
}

# Modèle Prophet
train_prophet_model <- function(data, horizon = 12) {
    message("Entraînement du modèle Prophet...")
    
    # Préparation des données pour Prophet
    prophet_data <- data %>%
        rename(ds = date, y = attacks)
    
    # Entraînement du modèle
    model <- prophet(prophet_data, yearly.seasonality = TRUE, 
                    weekly.seasonality = FALSE, daily.seasonality = FALSE)
    
    # Création du dataframe futur
    future <- make_future_dataframe(model, periods = horizon, freq = "month")
    
    # Prédictions
    predictions <- predict(model, future)
    
    return(list(
        model = model,
        predictions = predictions
    ))
}

# Modèle par région
train_regional_models <- function(data, horizon = 12) {
    message("Entraînement des modèles régionaux...")
    
    # Création d'un modèle Prophet pour chaque région
    regional_models <- data %>%
        split(.$region) %>%
        map(function(region_data) {
            prophet_data <- region_data %>%
                rename(ds = date, y = attacks)
            
            model <- prophet(prophet_data, yearly.seasonality = TRUE,
                           weekly.seasonality = FALSE, daily.seasonality = FALSE)
            
            future <- make_future_dataframe(model, periods = horizon, freq = "month")
            predictions <- predict(model, future)
            
            list(
                model = model,
                predictions = predictions
            )
        })
    
    return(regional_models)
}

# -----------------------------------------------------------------------------
#                    3. VISUALISATION DES PRÉDICTIONS
# -----------------------------------------------------------------------------

plot_predictions <- function(data, arima_pred, prophet_pred) {
    message("Création des visualisations des prédictions...")
    
    # Préparation des données de prédiction ARIMA
    arima_df <- data.frame(
        date = seq.Date(from = max(data$date) + months(1),
                       by = "month", length.out = length(arima_pred$mean)),
        prediction = as.numeric(arima_pred$mean),
        lower = as.numeric(arima_pred$lower[,"95%"]),
        upper = as.numeric(arima_pred$upper[,"95%"]),
        model = "ARIMA"
    )
    
    # Préparation des données de prédiction Prophet
    prophet_df <- data.frame(
        date = prophet_pred$ds,
        prediction = prophet_pred$yhat,
        lower = prophet_pred$yhat_lower,
        upper = prophet_pred$yhat_upper,
        model = "Prophet"
    ) %>%
        filter(date > max(data$date))
    
    # Création du graphique
    p <- plot_ly() %>%
        # Données historiques
        add_lines(data = data,
                 x = ~date, y = ~attacks,
                 name = "Historique",
                 line = list(color = "black")) %>%
        # Prédictions ARIMA
        add_lines(data = arima_df,
                 x = ~date, y = ~prediction,
                 name = "ARIMA",
                 line = list(color = "blue", dash = "dash")) %>%
        add_ribbons(data = arima_df,
                   x = ~date, ymin = ~lower, ymax = ~upper,
                   name = "IC ARIMA",
                   fillcolor = "rgba(0,0,255,0.2)",
                   line = list(color = "transparent")) %>%
        # Prédictions Prophet
        add_lines(data = prophet_df,
                 x = ~date, y = ~prediction,
                 name = "Prophet",
                 line = list(color = "red", dash = "dash")) %>%
        add_ribbons(data = prophet_df,
                   x = ~date, ymin = ~lower, ymax = ~upper,
                   name = "IC Prophet",
                   fillcolor = "rgba(255,0,0,0.2)",
                   line = list(color = "transparent")) %>%
        layout(title = "Prédiction des Attaques Terroristes",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Nombre d'attaques"))
    
    return(p)
}

plot_regional_predictions <- function(data, regional_models) {
    message("Création des visualisations des prédictions régionales...")
    
    # Création d'un graphique pour chaque région
    plots <- imap(regional_models, function(model, region) {
        region_data <- data %>%
            filter(region == !!region)
        
        pred_df <- model$predictions %>%
            filter(ds > max(region_data$date))
        
        plot_ly() %>%
            add_lines(data = region_data,
                     x = ~date, y = ~attacks,
                     name = "Historique",
                     line = list(color = "black")) %>%
            add_lines(data = pred_df,
                     x = ~ds, y = ~yhat,
                     name = "Prédiction",
                     line = list(color = "red", dash = "dash")) %>%
            add_ribbons(data = pred_df,
                       x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper,
                       name = "Intervalle de confiance",
                       fillcolor = "rgba(255,0,0,0.2)",
                       line = list(color = "transparent")) %>%
            layout(title = paste("Prédictions pour", region),
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Nombre d'attaques"))
    })
    
    return(plots)
}

# -----------------------------------------------------------------------------
#                    4. ÉVALUATION DES MODÈLES
# -----------------------------------------------------------------------------

evaluate_predictions <- function(data, arima_model, prophet_model) {
    message("Évaluation des modèles de prédiction...")
    
    # Division des données pour validation
    train_size <- floor(0.8 * nrow(data))
    train_data <- data[1:train_size, ]
    test_data <- data[(train_size+1):nrow(data), ]
    
    # Entraînement et prédiction sur les données de test
    arima_test <- train_arima_model(train_data, horizon = nrow(test_data))
    prophet_test <- train_prophet_model(train_data, horizon = nrow(test_data))
    
    # Calcul des métriques
    metrics <- data.frame(
        Model = c("ARIMA", "Prophet"),
        RMSE = c(
            sqrt(mean((test_data$attacks - as.numeric(arima_test$predictions$mean))^2)),
            sqrt(mean((test_data$attacks - prophet_test$predictions$yhat[(train_size+1):nrow(data)])^2))
        ),
        MAE = c(
            mean(abs(test_data$attacks - as.numeric(arima_test$predictions$mean))),
            mean(abs(test_data$attacks - prophet_test$predictions$yhat[(train_size+1):nrow(data)]))
        )
    )
    
    return(metrics)
}

# -----------------------------------------------------------------------------
#                    5. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Création du dossier pour les résultats
    dir.create("results/predictions", recursive = TRUE, showWarnings = FALSE)
    
    # Chargement des données
    message("Chargement des données...")
    data <- read_csv("data/processed/gtd_cleaned.csv")
    
    # Préparation des données
    ts_data <- prepare_time_series_data(data)
    
    # Entraînement des modèles
    arima_results <- train_arima_model(ts_data$monthly)
    prophet_results <- train_prophet_model(ts_data$monthly)
    regional_models <- train_regional_models(ts_data$regional)
    
    # Création des visualisations
    global_pred_plot <- plot_predictions(
        ts_data$monthly,
        arima_results$predictions,
        prophet_results$predictions
    )
    
    regional_plots <- plot_regional_predictions(
        ts_data$regional,
        regional_models
    )
    
    # Évaluation des modèles
    metrics <- evaluate_predictions(
        ts_data$monthly,
        arima_results$model,
        prophet_results$model
    )
    
    # Sauvegarde des résultats
    message("\nSauvegarde des résultats...")
    saveRDS(list(
        arima = arima_results,
        prophet = prophet_results,
        regional = regional_models
    ), "results/predictions/time_series_models.rds")
    
    # Sauvegarde des visualisations
    htmlwidgets::saveWidget(global_pred_plot,
                          "results/predictions/global_predictions.html")
    
    for (region in names(regional_plots)) {
        htmlwidgets::saveWidget(
            regional_plots[[region]],
            sprintf("results/predictions/%s_predictions.html",
                   make.names(region))
        )
    }
    
    # Sauvegarde des métriques
    write_csv(metrics, "results/predictions/model_metrics.csv")
    
    message("Prédictions terminées avec succès!")
    
    return(list(
        models = list(
            arima = arima_results,
            prophet = prophet_results,
            regional = regional_models
        ),
        plots = list(
            global = global_pred_plot,
            regional = regional_plots
        ),
        metrics = metrics
    ))
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}
