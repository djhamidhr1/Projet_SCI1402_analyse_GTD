# ==============================================================================
#                    NETTOYAGE DES DONNÉES GTD (Global Terrorism Database)
# ==============================================================================

# Configuration de l'encodage et de l'environnement
Sys.setlocale("LC_ALL", "UTF-8")
options(encoding = "UTF-8")

# Chargement des packages nécessaires
library(tidyverse)
library(stringi)
library(lubridate)

# -----------------------------------------------------------------------------
#                    1. DÉFINITION DES CHEMINS
# -----------------------------------------------------------------------------

PATHS <- list(
    INPUT = "data/raw/globalterrorismdb_0718dist.csv",
    OUTPUT = "data/processed/gtd_clean.csv",
    TEMP = "data/temp"
)

# Création des dossiers nécessaires
for (path in PATHS) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------------------------------------------------------
#                    2. FONCTIONS DE NETTOYAGE
# -----------------------------------------------------------------------------

# Fonction pour nettoyer le texte en toute sécurité
safe_clean_text <- function(text) {
    if (is.na(text)) return(NA_character_)
    if (text == "") return(NA_character_)
    
    cleaned <- text %>%
        stri_encode(., "UTF-8", "UTF-8") %>%
        stri_trim_both() %>%
        str_squish()
        
    if (cleaned == "") return(NA_character_)
    return(cleaned)
}

# Nettoyage des dates
clean_dates <- function(data) {
    message("Nettoyage des dates...")
    
    data %>%
        mutate(
            # Validation des composants de la date
            year = iyear,
            month = if_else(between(imonth, 1, 12), imonth, NA_integer_),
            day = if_else(between(iday, 1, 31), iday, NA_integer_),
            
            # Création de la date complète
            date = make_date(year, month, day)
        )
}

# Nettoyage des localisations
clean_locations <- function(data) {
    message("Nettoyage des localisations...")
    
    data %>%
        mutate(
            # Nettoyage des champs textuels
            country = safe_clean_text(country_txt),
            region = safe_clean_text(region_txt),
            city = case_when(
                is.na(city) ~ NA_character_,
                city == "Unknown" ~ "Unknown",
                TRUE ~ safe_clean_text(city)
            ),
            
            # Validation des coordonnées
            latitude = if_else(between(latitude, -90, 90), latitude, NA_real_),
            longitude = if_else(between(longitude, -180, 180), longitude, NA_real_)
        )
}

# Nettoyage des informations d'attaque
clean_attack_info <- function(data) {
    message("Nettoyage des informations d'attaque...")
    
    data %>%
        mutate(
            # Nettoyage des types
            attack_type = safe_clean_text(attacktype1_txt),
            target_type = safe_clean_text(targtype1_txt),
            weapon_type = safe_clean_text(weaptype1_txt),
            
            # Validation des données numériques
            killed = if_else(is.na(nkill) | nkill < 0, 0, nkill),
            wounded = if_else(is.na(nwound) | nwound < 0, 0, nwound),
            total_casualties = killed + wounded,
            
            # Standardisation des indicateurs
            success = as.logical(success),
            is_suicide = as.logical(suicide)
        )
}

# Traitement des valeurs manquantes
handle_missing_values <- function(data) {
    message("Traitement des valeurs manquantes...")
    
    data %>%
        # Remplacement des valeurs manquantes textuelles
        mutate(across(where(is.character), ~replace_na(., "Unknown"))) %>%
        # Remplacement des valeurs manquantes numériques
        mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
        # Remplacement des valeurs manquantes logiques
        mutate(across(where(is.logical), ~replace_na(., FALSE)))
}

# -----------------------------------------------------------------------------
#                    3. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    message("\n=== DÉBUT DU NETTOYAGE DES DONNÉES ===\n")
    
    tryCatch({
        # Lecture des données
        message("Lecture du fichier source...")
        gtd_data <- read_csv(
            PATHS$INPUT,
            locale = locale(encoding = "UTF-8"),
            show_col_types = FALSE
        )
        
        # Application des nettoyages
        gtd_clean <- gtd_data %>%
            clean_dates() %>%
            clean_locations() %>%
            clean_attack_info() %>%
            handle_missing_values()
        
        # Sauvegarde des résultats
        message("\nSauvegarde des données nettoyées...")
        write_csv(gtd_clean, PATHS$OUTPUT)
        
        # Affichage des statistiques
        message("\nStatistiques finales:")
        summary_stats <- gtd_clean %>%
            summarise(
                total_incidents = n(),
                period = paste(min(year), "-", max(year)),
                total_countries = n_distinct(country),
                total_killed = sum(killed),
                total_wounded = sum(wounded)
            )
        print(summary_stats)
        
        message("\n=== NETTOYAGE TERMINÉ AVEC SUCCÈS ===")
        
    }, error = function(e) {
        message("\nERREUR lors du nettoyage : ", e$message)
        message("Backtrace :")
        print(e$trace)
    })
}

# Exécution du script
if (!interactive()) {
    main()
}
