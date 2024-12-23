# ==============================================================================
#                    CONFIGURATION DE SPARK
# ==============================================================================

# -----------------------------------------------------------------------------
#                    1. CHARGEMENT DE SPARKLYR
# -----------------------------------------------------------------------------

cat("=== CONFIGURATION DE SPARK ===\n")

# Chargement du package sparklyr
library(sparklyr)

# -----------------------------------------------------------------------------
#                    2. INSTALLATION DE SPARK
# -----------------------------------------------------------------------------

# 2.1 Installation de la version spécifiée de Spark
cat("\nInstallation de Spark...\n")
spark_install(version = "3.5.1")

# -----------------------------------------------------------------------------
#                    3. CONFIGURATION DE SPARK
# -----------------------------------------------------------------------------

# 3.1 Fonction de configuration
configure_spark <- function() {
    config <- spark_config()
    
    # Configuration de la mémoire
    config$`sparklyr.shell.driver-memory` <- "2G"
    config$`sparklyr.shell.executor-memory` <- "2G"
    
    # Configuration additionnelle (à décommenter si nécessaire)
    # config$spark.executor.cores <- 2
    # config$spark.executor.instances <- 1
    # config$spark.dynamicAllocation.enabled <- "true"
    
    return(config)
}

# -----------------------------------------------------------------------------
#                    4. CONNEXION À SPARK
# -----------------------------------------------------------------------------

# 4.1 Fonction de connexion
connect_to_spark <- function() {
    tryCatch({
        config <- configure_spark()
        sc <- spark_connect(
            master = "local",
            version = "3.5.1",
            config = config
        )
        cat("Connexion à Spark établie avec succès!\n")
        return(sc)
    }, error = function(e) {
        cat("Erreur lors de la connexion à Spark:", conditionMessage(e), "\n")
        return(NULL)
    })
}

# -----------------------------------------------------------------------------
#                    5. FONCTIONS UTILITAIRES
# -----------------------------------------------------------------------------

# 5.1 Fonction pour arrêter Spark
stop_spark <- function(sc) {
    if (!is.null(sc)) {
        spark_disconnect(sc)
        cat("Déconnexion de Spark effectuée.\n")
    }
}

# 5.2 Fonction pour vérifier l'état de Spark
check_spark_status <- function(sc) {
    if (!is.null(sc) && spark_connection_is_open(sc)) {
        cat("Spark est connecté et fonctionnel.\n")
        cat("Version de Spark:", spark_version(sc), "\n")
    } else {
        cat("Spark n'est pas connecté.\n")
    }
}

cat("\n=== CONFIGURATION SPARK TERMINÉE ===\n")
cat("\nPour utiliser Spark:\n")
cat("1. sc <- connect_to_spark()\n")
cat("2. check_spark_status(sc)\n")
cat("3. Pour arrêter: stop_spark(sc)\n")
