# PROJET TDAH TUNISIE - MICS6 2023
# Script 01 : Import des données SPSS
# Auteur: Asma BELKAHLA

# CONFIGURATION

rm(list = ls())
gc()

library(tidyverse)
library(haven)
library(labelled)
library(janitor)

project_root <- getwd()
options(scipen = 999, encoding = "UTF-8")

dir.create(file.path(project_root, "data", "processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(project_root, "data", "metadata"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(project_root, "reports", "figures"), showWarnings = FALSE, recursive = TRUE)

cat("Import des données MICS6 Tunisie 2023\n\n")
sav_files <- list.files(file.path(project_root, "data", "raw"), 
                        pattern = "\\.sav$", 
                        full.names = TRUE,
                        ignore.case = TRUE)

if (length(sav_files) == 0) {
  stop("Fichiers SPSS introuvables. Placez les fichiers .sav dans data/raw/")
}

# Identifier les fichiers MICS6 par leur nom
mics_files_map <- list(
  bh = grep("bh\\.sav$", sav_files, value = TRUE, ignore.case = TRUE)[1],
  ch = grep("ch\\.sav$", sav_files, value = TRUE, ignore.case = TRUE)[1],
  fs = grep("fs\\.sav$", sav_files, value = TRUE, ignore.case = TRUE)[1],
  hh = grep("hh\\.sav$", sav_files, value = TRUE, ignore.case = TRUE)[1],
  hl = grep("hl\\.sav$", sav_files, value = TRUE, ignore.case = TRUE)[1],
  mn = grep("mn\\.sav$", sav_files, value = TRUE, ignore.case = TRUE)[1],
  wm = grep("wm\\.sav$", sav_files, value = TRUE, ignore.case = TRUE)[1]
)

mics_files_map <- Filter(Negate(is.na), mics_files_map)

import_spss_file <- function(filepath, dataset_name) {
  cat("Import de", dataset_name, ":", basename(filepath), "\n")
  data <- read_sav(filepath, user_na = TRUE)
  cat("  Dimensions:", nrow(data), "lignes x", ncol(data), "colonnes\n")
  return(data)
}

mics_data_spss <- list()

for (name in names(mics_files_map)) {
  tryCatch({
    mics_data_spss[[name]] <- import_spss_file(mics_files_map[[name]], toupper(name))
  }, error = function(e) {
    cat("❌ ERREUR lors de l'import de", name, ":", e$message, "\n\n")
    mics_data_spss[[name]] <- NULL
  })
}

# 5. GÉNÉRATION DU RAPPORT DE STRUCTURE =======================================

cat("📋 Génération du rapport de structure avec labels SPSS...\n")

generate_spss_structure <- function(data, name) {
  
  structure_df <- tibble(
    dataset = name,
    variable = names(data),
    type = sapply(data, function(x) class(x)[1]),
    is_labelled = sapply(data, is.labelled),
    n_labels = sapply(data, function(x) {
      if (is.labelled(x)) length(val_labels(x)) else 0
    }),
    variable_label = sapply(data, function(x) {
      lbl <- attr(x, "label")
      if (is.null(lbl)) "" else as.character(lbl)
    }),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    pct_missing = round(sapply(data, function(x) sum(is.na(x)) / length(x) * 100), 2),
    n_unique = sapply(data, function(x) length(unique(x)))
  )
  
  return(structure_df)
}

structure_reports_spss <- list()
for (name in names(mics_data_spss)) {
  if (!is.null(mics_data_spss[[name]])) {
    structure_reports_spss[[name]] <- generate_spss_structure(mics_data_spss[[name]], name)
  }
}

all_structures_spss <- bind_rows(structure_reports_spss)

# Sauvegarder le rapport
write_csv(
  all_structures_spss,
  file.path(project_root, "data", "metadata", "mics6_spss_structure_report.csv")
)

cat("✅ Rapport de structure SPSS sauvegardé\n\n")

# 6. EXTRACTION DES LABELS ====================================================

cat("🏷️  Extraction des dictionnaires de labels...\n")

extract_all_labels <- function(data, dataset_name) {
  
  labels_list <- list()
  
  for (var_name in names(data)) {
    if (is.labelled(data[[var_name]])) {
      var_labels <- val_labels(data[[var_name]])
      
      if (length(var_labels) > 0) {
        labels_list[[var_name]] <- tibble(
          dataset = dataset_name,
          variable = var_name,
          value = as.numeric(var_labels),
          label = names(var_labels)
        )
      }
    }
  }
  
  if (length(labels_list) > 0) {
    return(bind_rows(labels_list))
  } else {
    return(NULL)
  }
}

# Extraire tous les labels
all_labels <- list()
for (name in names(mics_data_spss)) {
  if (!is.null(mics_data_spss[[name]])) {
    labels_df <- extract_all_labels(mics_data_spss[[name]], name)
    if (!is.null(labels_df)) {
      all_labels[[name]] <- labels_df
    }
  }
}

labels_dictionary <- bind_rows(all_labels)

if (nrow(labels_dictionary) > 0) {
  write_csv(
    labels_dictionary,
    file.path(project_root, "data", "metadata", "mics6_labels_dictionary.csv")
  )
  cat("✅ Dictionnaire des labels sauvegardé (", nrow(labels_dictionary), "labels)\n\n")
}

# 7. RÉSUMÉ DES IMPORTS =======================================================

cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    RÉSUMÉ DES IMPORTS SPSS                         ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

import_summary_spss <- tibble(
  dataset = names(mics_data_spss),
  n_rows = sapply(mics_data_spss, nrow),
  n_cols = sapply(mics_data_spss, ncol),
  n_labelled = sapply(mics_data_spss, function(x) sum(sapply(x, is.labelled))),
  size_mb = sapply(mics_data_spss, function(x) {
    round(as.numeric(object.size(x)) / 1024^2, 2)
  })
)

print(import_summary_spss)

cat("\n📊 Datasets importés avec succès:", length(mics_data_spss), "/ 7\n")

# 8. VÉRIFICATION DES VARIABLES CLÉS ==========================================

cat("\n🔍 Vérification des variables clés dans BH...\n\n")

if (!is.null(mics_data_spss$bh)) {
  # Chercher en majuscules ET minuscules
  bh_names_lower <- tolower(names(mics_data_spss$bh))
  bh_vars_check <- c("bh4c", "bh4f", "bh9c", "bh9f", "magebrt", "brthord", 
                     "birthint", "windex5", "welevel", "hh6", "hh7")
  
  cat("Variables clés dans BH:\n")
  for (v in bh_vars_check) {
    idx <- which(bh_names_lower == v)
    if (length(idx) > 0) {
      actual_name <- names(mics_data_spss$bh)[idx[1]]
      var_label <- attr(mics_data_spss$bh[[actual_name]], "label")
      cat("  ✅", actual_name)
      if (!is.null(var_label)) cat(" -", var_label)
      cat("\n")
    } else {
      cat("  ❌", v, "- NON TROUVÉ\n")
    }
  }
  
  # Afficher un échantillon de BH
  cat("\n📊 Aperçu des données BH (premières lignes):\n")
  print(head(mics_data_spss$bh[, 1:15]))
  
  # Statistiques sur BH9C (âge en mois)
  if ("BH9C" %in% names(mics_data_spss$bh)) {
    cat("\n📈 Statistiques BH9C (âge en mois):\n")
    print(summary(mics_data_spss$bh$BH9C))
  }
}

# 9. CONVERSION OPTIONNELLE EN FACTEURS =======================================

cat("\n🔄 Conversion des variables labellisées...\n")

# Option 1: Garder les variables labellisées (recommandé)
mics_data <- mics_data_spss

# Option 2: Convertir en facteurs (si nécessaire pour certaines analyses)
# Décommenter si vous préférez des facteurs
# mics_data <- lapply(mics_data_spss, function(df) {
#   as_factor(df, levels = "labels")
# })

cat("✅ Données prêtes (format labelled préservé)\n")

# 10. SAUVEGARDE ==============================================================

cat("\n💾 Sauvegarde des données SPSS importées...\n")

# Sauvegarder chaque dataset
for (name in names(mics_data)) {
  if (!is.null(mics_data[[name]])) {
    saveRDS(
      mics_data[[name]],
      file.path(project_root, "data", "processed", paste0(name, "_spss.rds"))
    )
    cat("  ✅", name, "sauvegardé\n")
  }
}

# Sauvegarder l'environnement complet
save(
  mics_data,
  mics_data_spss,
  import_summary_spss,
  all_structures_spss,
  labels_dictionary,
  mics_files_map,
  file = file.path(project_root, "data", "processed", "01_imported_data_SPSS.RData")
)

cat("\n✨ Import SPSS terminé avec succès!\n")
cat("📁 Fichiers disponibles dans: data/processed/\n")
cat("📚 Métadonnées disponibles dans: data/metadata/\n")
cat("   - mics6_spss_structure_report.csv (structure complète)\n")
cat("   - mics6_labels_dictionary.csv (tous les labels)\n")
cat("\n🚀 Prochaine étape: Exécuter 02_data_cleaning_SPSS.R\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================