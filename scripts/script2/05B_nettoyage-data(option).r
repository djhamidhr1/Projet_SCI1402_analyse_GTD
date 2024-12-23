# ==============================================================================
#                    NETTOYAGE DES DONNÉES GTD (Global Terrorism Database)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(janitor)

# -----------------------------------------------------------------------------
#                    1. CHARGEMENT ET AFFICHAGE INITIAL
# -----------------------------------------------------------------------------

load_and_display_gtd <- function(file_path) {
    # Chargement avec types spécifiques
    gtd_data <- read_csv(file_path, show_col_types = FALSE)
    
    message("\n=== APERÇU INITIAL DES DONNÉES GTD ===")
    
    # Dimensions
    message(sprintf("\nDimensions: %d lignes et %d colonnes", 
                   nrow(gtd_data), ncol(gtd_data)))
    
    # Aperçu des premières lignes
    message("\nPremières lignes:")
    print(head(gtd_data))
    
    # Résumé des colonnes principales
    message("\nRésumé des colonnes principales:")
    summary_cols <- c("iyear", "imonth", "iday", "country_txt", "region_txt",
                     "attacktype1_txt", "targtype1_txt", "nkill", "nwound")
    print(summary(gtd_data[summary_cols]))
    
    return(gtd_data)
}

# -----------------------------------------------------------------------------
#                    2. FONCTIONS DE NETTOYAGE
# -----------------------------------------------------------------------------

clean_dates <- function(data) {
    data %>%
        mutate(
            # Création d'une date complète
            date = make_date(iyear, imonth, iday),
            # Gestion des dates invalides
            imonth = if_else(imonth == 0, NA_integer_, imonth),
            iday = if_else(iday == 0, NA_integer_, iday)
        )
}

clean_locations <- function(data) {
    data %>%
        mutate(
            # Nettoyage des locations
            country_txt = str_trim(country_txt),
            region_txt = str_trim(region_txt),
            city = if_else(city == "Unknown", NA_character_, str_trim(city)),
            # Validation des coordonnées
            latitude = if_else(abs(latitude) > 90, NA_real_, latitude),
            longitude = if_else(abs(longitude) > 180, NA_real_, longitude)
        )
}

clean_attack_info <- function(data) {
    data %>%
        mutate(
            # Standardisation des types d'attaque
            attacktype1_txt = str_to_title(attacktype1_txt),
            # Nettoyage des cibles
            targtype1_txt = str_to_title(targtype1_txt),
            # Validation des données numériques
            nkill = if_else(nkill < 0, NA_real_, nkill),
            nwound = if_else(nwound < 0, NA_real_, nwound)
        )
}

handle_missing_values <- function(data) {
    data %>%
        # Remplacement des valeurs manquantes numériques
        mutate(
            nkill = if_else(is.na(nkill), 0, nkill),
            nwound = if_else(is.na(nwound), 0, nwound),
            # Autres colonnes numériques à 0 si NA
            across(starts_with("n") & where(is.numeric), 
                  ~if_else(is.na(.), 0, .))
        ) %>%
        # Remplacement des valeurs manquantes textuelles
        mutate(
            across(where(is.character), 
                  ~if_else(is.na(.) | . == "", "Unknown", .))
        )
}

remove_duplicates <- function(data) {
    # Identification des colonnes clés pour les doublons
    key_columns <- c("iyear", "imonth", "iday", "country_txt", 
                    "city", "latitude", "longitude", "attacktype1_txt")
    
    n_initial <- nrow(data)
    data_cleaned <- data %>% distinct(across(all_of(key_columns)), .keep_all = TRUE)
    n_final <- nrow(data_cleaned)
    
    message(sprintf("\nDoublons supprimés: %d lignes", n_initial - n_final))
    return(data_cleaned)
}

# -----------------------------------------------------------------------------
#                    3. FONCTION DE NORMALISATION
# -----------------------------------------------------------------------------

normalize_gtd_data <- function(data) {
    # Sélection et renommage des colonnes principales
    data %>%
        select(
            event_id = eventid,
            year = iyear,
            month = imonth,
            day = iday,
            date,
            country = country_txt,
            region = region_txt,
            city,
            latitude,
            longitude,
            attack_type = attacktype1_txt,
            target_type = targtype1_txt,
            target_subtype = targsubtype1_txt,
            weapon_type = weaptype1_txt,
            killed = nkill,
            wounded = nwound,
            property_damage = property,
            group_name = gname,
            success
        )
}

# -----------------------------------------------------------------------------
#                    4. STATISTIQUES FINALES
# -----------------------------------------------------------------------------

display_final_stats <- function(data) {
    message("\n=== STATISTIQUES FINALES ===")
    
    # Statistiques générales
    message("\nNombre d'événements par année:")
    print(table(data$year))
    
    message("\nNombre d'événements par type d'attaque:")
    print(table(data$attack_type))
    
    message("\nStatistiques des victimes:")
    summary_stats <- data %>%
        summarise(
            total_killed = sum(killed, na.rm = TRUE),
            total_wounded = sum(wounded, na.rm = TRUE),
            max_killed = max(killed, na.rm = TRUE),
            max_wounded = max(wounded, na.rm = TRUE)
        )
    print(summary_stats)
}

# -----------------------------------------------------------------------------
#                    5. FONCTION PRINCIPALE
# -----------------------------------------------------------------------------

main <- function() {
    # Chemins des fichiers
    input_file <- "data/raw/globalterrorismdb_0718dist.csv"
    output_file <- "data/processed/gtd_cleaned.csv"
    
    # Chargement et affichage initial
    message("Chargement des données...")
    gtd_data <- load_and_display_gtd(input_file)
    
    # Nettoyage des données
    message("\nNettoyage des données...")
    cleaned_data <- gtd_data %>%
        clean_dates() %>%
        clean_locations() %>%
        clean_attack_info() %>%
        handle_missing_values() %>%
        remove_duplicates() %>%
        normalize_gtd_data()
    
    # Affichage des statistiques finales
    display_final_stats(cleaned_data)
    
    # Sauvegarde des données nettoyées
    message("\nSauvegarde des données nettoyées...")
    write_csv(cleaned_data, output_file)
    message(sprintf("Données sauvegardées dans: %s", output_file))
}

# Exécution si le script est lancé directement
if (!interactive()) {
    main()
}
