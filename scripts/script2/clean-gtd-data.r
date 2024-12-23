# ==============================================================================
#                    NETTOYAGE DES DONNÉES GTD 
# ==============================================================================

# Configuration initiale
Sys.setlocale("LC_ALL", "UTF-8")
options(encoding = "UTF-8")

# -----------------------------------------------------------------------------
#                    1. PACKAGES ET CONFIGURATION
# -----------------------------------------------------------------------------
library(tidyverse)  # Manipulation de données
library(stringi)    # Nettoyage de texte
library(lubridate)  # Manipulation de dates

# Configuration des chemins
PATHS <- list(
  INPUT = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/raw/globalterrorismdb_0718dist.csv",
  OUTPUT = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/processed/gtd_clean.csv",
  TEMP = "C:/Users/digor/OneDrive/Documents/Projet_SCI1402_analyse_GTD/data/temp"
)

# Création des répertoires
for (path in PATHS) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------------------------------------------------------
#                    2. FONCTIONS DE NETTOYAGE
# -----------------------------------------------------------------------------

# 2.1 Nettoyage de texte sécurisé
safe_clean_text <- function(text) {
  if (is.null(text) || all(is.na(text))) return(NA_character_)
  
  text <- as.character(text)
  text <- tryCatch(
    {
      text %>%
        stri_encode("UTF-8", "UTF-8", to_raw = TRUE) %>%
        stri_trim_both() %>%
        str_squish()
    },
    warning = function(w) NA_character_,
    error = function(e) NA_character_
  )
  
  text[text == ""] <- NA_character_
  return(text)
}

# 2.2 Nettoyage des dates
clean_dates <- function(data) {
  message("Nettoyage des dates...")
  
  data %>%
    mutate(
      year = iyear,
      month = if_else(between(imonth, 1, 12), imonth, NA_integer_),
      day = if_else(between(iday, 1, 31), iday, NA_integer_),
      date = make_date(year, month, day)
    )
}

# 2.3 Nettoyage des localisations
clean_locations <- function(data) {
  message("Nettoyage des localisations...")
  
  if (!"country_txt" %in% colnames(data)) {
    stop("ERREUR : La colonne 'country_txt' est introuvable.")
  }
  
  data %>%
    mutate(
      country = safe_clean_text(country_txt),
      region = safe_clean_text(region_txt),
      city = case_when(
        city == "Unknown" ~ "Unknown",
        TRUE ~ safe_clean_text(city)
      ),
      latitude = if_else(between(latitude, -90, 90), latitude, NA_real_),
      longitude = if_else(between(longitude, -180, 180), longitude, NA_real_)
    )
}

# 2.4 Nettoyage des informations d'attaque
clean_attack_info <- function(data) {
  message("Nettoyage des informations d'attaque...")
  
  data %>%
    mutate(
      attack_type = safe_clean_text(attacktype1_txt),
      target_type = safe_clean_text(targtype1_txt),
      weapon_type = safe_clean_text(weaptype1_txt),
      killed = if_else(nkill < 0 | is.na(nkill), 0, nkill),
      wounded = if_else(nwound < 0 | is.na(nwound), 0, nwound),
      total_casualties = killed + wounded,
      success = as.logical(success)
    )
}

# 2.5 Gestion des valeurs manquantes
handle_missing_values <- function(data) {
  message("Gestion des valeurs manquantes...")
  
  data %>%
    mutate(across(where(is.character), ~replace_na(., "Unknown"))) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
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
    
    # Gestion des lignes problématiques
    if (nrow(problems(gtd_data)) > 0) {
      message("Suppression des lignes problématiques...")
      gtd_data <- gtd_data %>% filter(!row_number() %in% problems(gtd_data)$row)
    }
    
    # Application du nettoyage
    gtd_clean <- gtd_data %>%
      clean_dates() %>%
      clean_locations() %>%
      clean_attack_info() %>%
      handle_missing_values()
    
    # Sauvegarde et statistiques
    write_csv(gtd_clean, PATHS$OUTPUT)
    
    message("\nStatistiques finales :")
    summary_stats <- gtd_clean %>%
      summarise(
        total_incidents = n(),
        total_countries = n_distinct(country),
        period = paste(min(year, na.rm = TRUE), "-", max(year, na.rm = TRUE)),
        total_killed = sum(killed, na.rm = TRUE),
        total_wounded = sum(wounded, na.rm = TRUE)
      )
    print(summary_stats)
    
    message("\n=== NETTOYAGE TERMINÉ AVEC SUCCÈS ===")
    
  }, error = function(e) {
    message("\nERREUR : ", e$message)
  })
}

# Exécution du script
if (!interactive()) {
  main()
} else {
  message("Exécution interactive...")
  main()
}