# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 01 : Import des données
# ==============================================================================
# Description: Import et première inspection des fichiers MICS6
# Auteur: Asma BELKAHLA
# Date: 2025-12-22
# ==============================================================================

# 1. CONFIGURATION ============================================================

# Nettoyage de l'environnement
rm(list = ls())
gc()

# Chargement des packages nécessaires
library(tidyverse)
library(readr)
library(haven)  # Pour lire .dta ou .sav si nécessaire
library(janitor) # Pour nettoyer les noms de colonnes

# Configuration des options
options(
  scipen = 999,  # Désactiver la notation scientifique
  encoding = "UTF-8"
)

# Définir la racine du projet (sans utiliser here pour éviter les problèmes)
project_root <- getwd()

# Créer les dossiers nécessaires
dir.create(file.path(project_root, "data", "processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(project_root, "data", "metadata"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(project_root, "reports", "figures"), showWarnings = FALSE, recursive = TRUE)

# 2. FONCTIONS UTILITAIRES ====================================================

#' Import et inspection d'un fichier CSV MICS6
#'
#' @param filename Nom du fichier (sans le chemin)
#' @param path Chemin vers le dossier des données
#' @return Un tibble avec les données importées
import_mics_file <- function(filename, path = file.path(project_root, "data", "raw")) {
  
  filepath <- file.path(path, filename)
  
  # Vérifier que le fichier existe
  if (!file.exists(filepath)) {
    stop(paste("Fichier introuvable:", filepath))
  }
  
  cat("\n", strrep("=", 70), "\n")
  cat("Import de:", filename, "\n")
  cat(strrep("=", 70), "\n")
  
  # Import selon l'extension
  if (str_ends(filename, ".csv")) {
    data <- read_csv(filepath, show_col_types = FALSE)
  } else if (str_ends(filename, ".dta")) {
    data <- read_dta(filepath)
  } else if (str_ends(filename, ".sav")) {
    data <- read_sav(filepath)
  } else {
    stop("Format de fichier non supporté")
  }
  
  # Nettoyer les noms de colonnes
  data <- clean_names(data)
  
  # Afficher les informations de base
  cat("\n📊 Dimensions:", nrow(data), "lignes x", ncol(data), "colonnes\n")
  cat("📝 Premières colonnes:\n")
  print(names(data)[1:min(10, ncol(data))])
  
  cat("\n💾 Taille mémoire:", format(object.size(data), units = "Mb"), "\n")
  cat(strrep("=", 70), "\n")
  
  return(data)
}

#' Générer un rapport de structure pour chaque dataset
#'
#' @param data Dataset à analyser
#' @param name Nom du dataset
generate_structure_report <- function(data, name) {
  
  report <- tibble(
    dataset = name,
    variable = names(data),
    type = sapply(data, class),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    pct_missing = round(sapply(data, function(x) sum(is.na(x)) / length(x) * 100), 2),
    n_unique = sapply(data, function(x) length(unique(x)))
  )
  
  return(report)
}

# 3. IMPORT DES FICHIERS MICS6 ===============================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║       IMPORT DES DONNÉES MICS6 TUNISIE 2023                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")

# Vérifier le répertoire de travail
cat("📁 Répertoire de travail:", getwd(), "\n")
cat("📁 Fichiers disponibles dans data/raw:\n")
available_files <- list.files("data/raw")
print(available_files)

# Liste des fichiers à importer
mics_files <- c(
  "hh.csv",  # Ménages
  "hl.csv",  # Liste des membres
  "wm.csv",  # Femmes 15-49 ans
  "mn.csv",  # Hommes 15-49 ans
  "bh.csv",  # Historique des naissances
  "ch.csv",  # Enfants
  "fs.csv"   # Sécurité alimentaire
)

# Import de tous les fichiers
mics_data <- list()

for (file in mics_files) {
  
  # Extraire le nom du dataset (sans extension)
  dataset_name <- str_remove(file, "\\.csv$")
  
  # Importer le fichier
  tryCatch({
    mics_data[[dataset_name]] <- import_mics_file(file)
  }, error = function(e) {
    cat("❌ ERREUR lors de l'import de", file, ":", e$message, "\n")
    mics_data[[dataset_name]] <- NULL
  })
}

# 4. GÉNÉRATION DES RAPPORTS DE STRUCTURE ====================================

cat("\n📋 Génération des rapports de structure...\n")

structure_reports <- list()

for (name in names(mics_data)) {
  if (!is.null(mics_data[[name]])) {
    structure_reports[[name]] <- generate_structure_report(mics_data[[name]], name)
  }
}

# Combiner tous les rapports
all_structures <- bind_rows(structure_reports)

# Sauvegarder le rapport complet
write_csv(
  all_structures,
  file.path(project_root, "data", "metadata", "mics6_structure_report.csv")
)

cat("✅ Rapport de structure sauvegardé dans data/metadata/\n")

# 5. RÉSUMÉ DES IMPORTS =======================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    RÉSUMÉ DES IMPORTS                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

import_summary <- tibble(
  dataset = names(mics_data),
  n_rows = sapply(mics_data, nrow),
  n_cols = sapply(mics_data, ncol),
  size_mb = sapply(mics_data, function(x) {
    round(as.numeric(object.size(x)) / 1024^2, 2)
  })
)

print(import_summary)

cat("\n📊 Datasets importés avec succès:", length(mics_data), "/", length(mics_files), "\n")

# 6. VARIABLES CLÉS À IDENTIFIER ==============================================

cat("\n🔍 Recherche des variables clés pour l'analyse...\n\n")

# Variables attendues (à adapter selon les données réelles)
key_variables <- list(
  
  # Variables d'identification
  identifiants = c("hh1", "hh2", "ln", "cluster", "wm1", "bh1"),
  
  # Variables périnatales
  perinatales = c("age_mere", "ordre_naissance", "intervalle", "poids_naissance"),
  
  # Variables démographiques
  demographiques = c("age", "sexe", "milieu", "region"),
  
  # Variables socio-économiques
  socioeconomiques = c("education", "richesse", "quintile", "emploi"),
  
  # Variables familiales
  familiales = c("taille_menage", "structure_famille", "nb_enfants")
)

cat("📝 Variables clés à vérifier dans les prochaines étapes:\n")
for (category in names(key_variables)) {
  cat("  -", category, ":", paste(key_variables[[category]], collapse = ", "), "\n")
}

# 7. SAUVEGARDE DES DONNÉES IMPORTÉES =========================================

cat("\n💾 Sauvegarde des données importées...\n")

# Sauvegarder chaque dataset en format .rds (plus efficace)
for (name in names(mics_data)) {
  if (!is.null(mics_data[[name]])) {
    saveRDS(
      mics_data[[name]],
      file.path(project_root, "data", "processed", paste0(name, "_imported.rds"))
    )
    cat("  ✅", name, "sauvegardé\n")
  }
}

# Sauvegarder aussi l'environnement complet
save(
  mics_data,
  import_summary,
  all_structures,
  file = file.path(project_root, "data", "processed", "01_imported_data.RData")
)

cat("\n✨ Import terminé avec succès!\n")
cat("📁 Fichiers disponibles dans: data/processed/\n")
cat("\n🚀 Prochaine étape: Exécuter 02_data_cleaning.R\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================