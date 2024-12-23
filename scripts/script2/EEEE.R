# Liste des packages à installer
packages <- c("tidyverse", "corrplot", "lubridate", "broom", 
              "scales", "gridExtra", "here", "tsibble", 
              "feasts", "fable")

# Vérifier et installer les packages manquants
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)  # Installer le package
    library(pkg, character.only = TRUE)         # Charger le package
  } else {
    library(pkg, character.only = TRUE)         # Charger le package s'il existe déjà
  }
}

# Confirmation
print("Tous les packages sont installés et chargés avec succès!")




# Installer remotes s'il n'est pas déjà installé
if (!require(remotes)) install.packages("remotes")

# Installer un package depuis GitHub
remotes::install_github("nom_utilisateur/nom_du_repository")




# Installer remotes si nécessaire
if (!require(remotes)) install.packages("remotes")

# Installer le package depuis GitHub
remotes::install_github("djhamidhr1/Projet_SCI1402_analyse_GTD")


packages <- c("tidyverse", "corrplot", "tsibble", "feasts", "lubridate", 
              "broom", "scales", "fable", "gridExtra", "here")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}




if (!interactive()) {
  pdf("output.pdf")  # Sauvegarde tous les graphiques dans un fichier PDF
  main()
  dev.off()
} else {
  main()
}

install.packages(c("tidyverse", "plotly", "leaflet", "viridis", "DT", "htmlwidgets"
                   
install.packages(c("tidyverse", "corrplot", "lubridate", "broom", "scales", "gridExtra", "here", "tsibble", "feasts", "fable"))