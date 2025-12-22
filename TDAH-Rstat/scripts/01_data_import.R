# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 01 : Importation des données
# ==============================================================================
# Description: Importation et préparation initiale des fichiers MICS6
# Auteur: Asma BELKAHLA
# Date: 2025-12-22
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(here)
library(janitor)

# Créer les répertoires nécessaires
dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("data", "metadata"), showWarnings = FALSE, recursive = TRUE)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║            IMPORTATION DES DONNÉES MICS6                           ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# 2. IMPORTATION DES FICHIERS CSV ============================================

cat("📥 Importation des fichiers CSV depuis data/raw/...\n\n")

# Liste de tous les fichiers CSV dans le dossier raw
csv_files <- list.files(here("data", "raw"), pattern = "\\.csv$", full.names = TRUE)

# Initialiser une liste pour stocker les données
mics_data <- list()

# Lire chaque fichier CSV
for (file in csv_files) {
  # Extraire le nom du fichier sans extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  cat("  → Lecture de:", file_name, "\n")
  
  tryCatch({
    # Lire le fichier CSV
    # Utiliser show_col_types = FALSE pour éviter les messages
    df <- read_csv(
      file,
      locale = locale(encoding = "UTF-8"),
      show_col_types = FALSE,
      guess_max = 10000  # Augmenter le nombre de lignes pour deviner le type
    )
    
    # Nettoyer les noms de colonnes
    df <- df %>% clean_names()
    
    # Ajouter à la liste
    mics_data[[file_name]] <- df
    
    cat("    ✅ Succès:", nrow(df), "lignes,", ncol(df), "colonnes\n")
    
  }, error = function(e) {
    cat("    ❌ Erreur:", e$message, "\n")
  })
}

# 3. VÉRIFICATION DES IMPORTATIONS ===========================================

cat("\n📊 Résumé de l'importation:\n\n")

for (name in names(mics_data)) {
  df <- mics_data[[name]]
  cat(sprintf("  %-5s: %7d observations × %3d variables\n", 
              name, nrow(df), ncol(df)))
}

cat("\n✅ Importation terminée!\n")

# 4. SAUVEGARDE DES DONNÉES IMPORTÉES =======================================

cat("\n💾 Sauvegarde des données importées...\n")

# Sauvegarder dans un fichier RData
save(mics_data, file = here("data", "processed", "01_imported_data.RData"))
cat("  ✅ Données sauvegardées: data/processed/01_imported_data.RData\n")

# Sauvegarder également en format RDS pour chaque dataframe individuel
for (name in names(mics_data)) {
  saveRDS(mics_data[[name]], here("data", "processed", paste0(name, "_raw.rds")))
}
cat("  ✅ Fichiers individuels sauvegardés en format RDS\n")

# 5. CRÉATION D'UN MÉTADONNÉES DE BASE =======================================

metadata <- tibble(
  fichier = names(mics_data),
  observations = sapply(mics_data, nrow),
  variables = sapply(mics_data, ncol),
  colonnes = sapply(mics_data, function(x) paste(names(x), collapse = ", "))
)

write_csv(metadata, here("data", "metadata", "metadata_import.csv"))
cat("  ✅ Métadonnées sauvegardées: data/metadata/metadata_import.csv\n")

# 6. PREMIÈRE EXPLORATION DES DONNÉES ========================================

cat("\n🔍 Première exploration des données:\n\n")

# Afficher la structure des données
cat("Structure des données importées:\n")
str(mics_data, max.level = 1)

cat("\n🚀 Prochaine étape: Exécuter 02_data_cleaning.R\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================