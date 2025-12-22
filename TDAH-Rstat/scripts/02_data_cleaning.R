# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 02 : Nettoyage et préparation des données
# ==============================================================================
# Description: Nettoyage, recodage et fusion des fichiers MICS6
# Auteur: Asma BELKAHLA
# Date: 2025-12-22
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(here)
library(janitor)
library(labelled)

# Charger les données importées
load(here("data", "processed", "01_imported_data.RData"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║            NETTOYAGE DES DONNÉES MICS6                             ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# 2. NETTOYAGE DU FICHIER BH (HISTORIQUE DES NAISSANCES) ====================

cat("📋 Nettoyage de bh.csv (historique des naissances)...\n")

bh_clean <- mics_data$bh %>%
  # Renommer les variables en minuscules pour cohérence
  rename_with(tolower) %>%
  
  # Variables clés pour l'analyse
  select(
    # Identifiants
    hh1, hh2, ln, bhln, psu, stratum,
    
    # Variables périnatales
    brthord,        # Ordre de naissance
    magebrt,        # Âge maternel à la naissance
    birthint,       # Intervalle intergénésique
    
    # Date de naissance (si besoin de calculer l'âge)
    bh4d, bh4m, bh4y,  # Jour, mois, année de naissance
    
    # Survie de l'enfant
    bh5,            # Enfant vivant (1=oui, 2=non)
    bh9c,           # Âge actuel (en mois complets)
    
    # Variables socio-économiques
    welevel,        # Niveau d'éducation maternel
    windex5,        # Quintile de richesse
    windex10,       # Décile de richesse
    wscore,         # Score de richesse continu
    
    # Autres
    insurance,      # Assurance santé
    disability,     # Handicap
    
    # Pondération
    wmweight
  ) %>%
  
  # Créer un identifiant unique pour chaque enfant
  mutate(
    child_id = paste(hh1, hh2, bhln, sep = "_"),
    
    # Recodage des variables
    
    # Âge maternel à la naissance - catégories
    age_mere_cat = case_when(
      magebrt < 20 ~ "< 20 ans",
      magebrt >= 20 & magebrt < 35 ~ "20-34 ans",
      magebrt >= 35 ~ "≥ 35 ans",
      TRUE ~ NA_character_
    ),
    age_mere_risque = ifelse(magebrt < 20 | magebrt >= 35, 1, 0),
    
    # Ordre de naissance - catégories
    ordre_cat = case_when(
      brthord == 1 ~ "Premier",
      brthord == 2 ~ "Deuxième",
      brthord == 3 ~ "Troisième",
      brthord >= 4 ~ "Quatrième ou plus",
      TRUE ~ NA_character_
    ),
    ordre_risque = ifelse(brthord >= 4, 1, 0),
    
    # Intervalle intergénésique (en mois)
    intervalle_cat = case_when(
      is.na(birthint) | brthord == 1 ~ "Premier né",
      birthint < 24 ~ "< 24 mois",
      birthint >= 24 & birthint < 36 ~ "24-35 mois",
      birthint >= 36 ~ "≥ 36 mois",
      TRUE ~ NA_character_
    ),
    intervalle_risque = ifelse(!is.na(birthint) & birthint < 24, 1, 0),
    
    # Éducation maternelle
    educ_mere_cat = case_when(
      welevel == 0 ~ "Aucune",
      welevel == 1 ~ "Primaire",
      welevel == 2 ~ "Secondaire",
      welevel >= 3 ~ "Supérieur",
      TRUE ~ NA_character_
    ),
    educ_mere_risque = ifelse(welevel <= 1, 1, 0),
    
    # Richesse
    richesse_cat = case_when(
      windex5 == 1 ~ "Q1 (Plus pauvre)",
      windex5 == 2 ~ "Q2",
      windex5 == 3 ~ "Q3",
      windex5 == 4 ~ "Q4",
      windex5 == 5 ~ "Q5 (Plus riche)",
      TRUE ~ NA_character_
    ),
    richesse_risque = ifelse(windex5 <= 2, 1, 0),
    
    # Enfant vivant
    enfant_vivant = ifelse(bh5 == 1, 1, 0),
    
    # Âge actuel en années
    age_annees = bh9c / 12
  ) %>%
  
  # Filtrer uniquement les enfants vivants
  filter(enfant_vivant == 1) %>%
  
  # Convertir les catégories en facteurs
  mutate(
    age_mere_cat = factor(age_mere_cat, levels = c("< 20 ans", "20-34 ans", "≥ 35 ans")),
    ordre_cat = factor(ordre_cat, levels = c("Premier", "Deuxième", "Troisième", "Quatrième ou plus")),
    intervalle_cat = factor(intervalle_cat, levels = c("Premier né", "< 24 mois", "24-35 mois", "≥ 36 mois")),
    educ_mere_cat = factor(educ_mere_cat, levels = c("Aucune", "Primaire", "Secondaire", "Supérieur")),
    richesse_cat = factor(richesse_cat, levels = c("Q1 (Plus pauvre)", "Q2", "Q3", "Q4", "Q5 (Plus riche)"))
  )

cat("  ✅ bh.csv nettoyé:", nrow(bh_clean), "enfants vivants\n")

# 3. NETTOYAGE DU FICHIER HL (MEMBRES DU MÉNAGE) ============================

cat("📋 Nettoyage de hl.csv (membres du ménage)...\n")

hl_clean <- mics_data$hl %>%
  rename_with(tolower) %>%
  select(
    hh1, hh2, hl1, hl3, hl4, hl6,
    ed4,           # Niveau d'éducation
    psu, stratum
  ) %>%
  mutate(
    person_id = paste(hh1, hh2, hl1, sep = "_"),
    
    # Sexe
    sexe = case_when(
      hl4 == 1 ~ "Masculin",
      hl4 == 2 ~ "Féminin",
      TRUE ~ NA_character_
    ),
    
    # Relation avec le chef de ménage
    relation_cm = case_when(
      hl3 == 1 ~ "Chef de ménage",
      hl3 == 2 ~ "Époux/Épouse",
      hl3 == 3 ~ "Fils/Fille",
      hl3 == 4 ~ "Gendre/Belle-fille",
      hl3 >= 5 ~ "Autre",
      TRUE ~ NA_character_
    ),
    
    # Âge
    age = hl6,
    
    # Groupe d'âge
    groupe_age = case_when(
      age < 5 ~ "0-4 ans",
      age >= 5 & age < 10 ~ "5-9 ans",
      age >= 10 & age < 15 ~ "10-14 ans",
      age >= 15 & age < 18 ~ "15-17 ans",
      age >= 18 ~ "18+ ans",
      TRUE ~ NA_character_
    )
  )

cat("  ✅ hl.csv nettoyé:", nrow(hl_clean), "individus\n")

# 4. NETTOYAGE DU FICHIER HH (MÉNAGES) ======================================

cat("📋 Nettoyage de hh.csv (ménages)...\n")

hh_clean <- mics_data$hh %>%
  rename_with(tolower) %>%
  select(
    hh1, hh2, hh6, hh7,
    starts_with("hh"),
    psu, stratum
  ) %>%
  mutate(
    menage_id = paste(hh1, hh2, sep = "_"),
    
    # Milieu de résidence
    milieu = case_when(
      hh6 == 1 ~ "Urbain",
      hh6 == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    
    # Région (à adapter selon le codage de hh7)
    region = as.character(hh7)
  )

# Calculer la taille du ménage à partir de hl
taille_menage <- hl_clean %>%
  group_by(hh1, hh2) %>%
  summarise(
    taille_menage = n(),
    nb_enfants = sum(age < 18, na.rm = TRUE),
    nb_adultes = sum(age >= 18, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    taille_menage_cat = case_when(
      taille_menage <= 3 ~ "Petit (≤3)",
      taille_menage <= 5 ~ "Moyen (4-5)",
      taille_menage <= 7 ~ "Grand (6-7)",
      taille_menage > 7 ~ "Très grand (>7)",
      TRUE ~ NA_character_
    ),
    taille_menage_risque = ifelse(taille_menage > 7, 1, 0)
  )

hh_clean <- hh_clean %>%
  left_join(taille_menage, by = c("hh1", "hh2"))

cat("  ✅ hh.csv nettoyé:", nrow(hh_clean), "ménages\n")

# 5. FUSION DES DONNÉES POUR CRÉER LE DATASET ANALYTIQUE ====================

cat("\n🔗 Fusion des fichiers pour créer le dataset analytique...\n")

# Dataset principal : partir de bh (enfants) et joindre les autres infos
dataset_analytique <- bh_clean %>%
  
  # Joindre les infos du ménage
  left_join(
    hh_clean %>% select(hh1, hh2, milieu, region, taille_menage, 
                        nb_enfants, nb_adultes, taille_menage_cat, 
                        taille_menage_risque),
    by = c("hh1", "hh2")
  ) %>%
  
  # Joindre les infos de l'enfant depuis hl (sexe notamment)
  left_join(
    hl_clean %>% 
      filter(hl3 == 3) %>%  # Fils/Fille uniquement
      select(hh1, hh2, hl1, sexe, age, groupe_age),
    by = c("hh1", "hh2", "ln" = "hl1")
  )

cat("  ✅ Dataset analytique créé:", nrow(dataset_analytique), "observations\n")

# 6. EXCLUSIONS ET CRITÈRES D'ÉLIGIBILITÉ ===================================

cat("\n🔍 Application des critères d'éligibilité...\n")

# Critères d'inclusion pour l'analyse TDAH (enfants d'âge scolaire typiquement)
dataset_final <- dataset_analytique %>%
  filter(
    !is.na(magebrt),      # Âge maternel disponible
    !is.na(brthord),      # Ordre de naissance disponible
    !is.na(windex5),      # Information socio-économique disponible
    !is.na(milieu),       # Information sur le milieu
    age_annees >= 3,      # Enfants de 3 ans et plus
    age_annees <= 17      # Enfants de moins de 18 ans
  )

cat("\n📊 Résumé des exclusions:\n")
cat("  - Départ:", nrow(dataset_analytique), "enfants\n")
cat("  - Exclus (données manquantes ou âge):", 
    nrow(dataset_analytique) - nrow(dataset_final), "\n")
cat("  - Échantillon final:", nrow(dataset_final), "enfants\n")

# 7. STATISTIQUES DESCRIPTIVES RAPIDES =======================================

cat("\n📈 Statistiques descriptives de l'échantillon final:\n\n")

# Sexe
cat("Distribution par sexe:\n")
print(table(dataset_final$sexe, useNA = "ifany"))

# Âge maternel
cat("\nÂge maternel à la naissance:\n")
cat("  - Moyenne:", round(mean(dataset_final$magebrt, na.rm = TRUE), 1), "ans\n")
cat("  - Médiane:", median(dataset_final$magebrt, na.rm = TRUE), "ans\n")

# Ordre de naissance
cat("\nOrdre de naissance:\n")
print(table(dataset_final$ordre_cat, useNA = "ifany"))

# Milieu de résidence
cat("\nMilieu de résidence:\n")
print(table(dataset_final$milieu, useNA = "ifany"))

# Quintile de richesse
cat("\nQuintile de richesse:\n")
print(table(dataset_final$richesse_cat, useNA = "ifany"))

# 8. VÉRIFICATION DE LA QUALITÉ DES DONNÉES =================================

cat("\n🔍 Vérification de la qualité des données:\n\n")

# Taux de données manquantes par variable clé
missing_summary <- dataset_final %>%
  summarise(across(
    c(magebrt, brthord, birthint, welevel, windex5, 
      milieu, taille_menage, sexe),
    ~sum(is.na(.)) / n() * 100,
    .names = "missing_{.col}"
  )) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  mutate(variable = str_remove(variable, "missing_"))

print(missing_summary)

# 9. SAUVEGARDE DES DONNÉES NETTOYÉES ========================================

cat("\n💾 Sauvegarde des données nettoyées...\n")

# Sauvegarder le dataset final
saveRDS(dataset_final, here("data", "processed", "dataset_final.rds"))
cat("  ✅ Dataset final sauvegardé: dataset_final.rds\n")

# Sauvegarder aussi les datasets intermédiaires
saveRDS(bh_clean, here("data", "processed", "bh_clean.rds"))
saveRDS(hl_clean, here("data", "processed", "hl_clean.rds"))
saveRDS(hh_clean, here("data", "processed", "hh_clean.rds"))
cat("  ✅ Datasets intermédiaires sauvegardés\n")

# Sauvegarder un dictionnaire des variables
dictionnaire <- tibble(
  variable = names(dataset_final),
  type = sapply(dataset_final, class),
  n_unique = sapply(dataset_final, function(x) length(unique(x))),
  exemple = sapply(dataset_final, function(x) {
    vals <- na.omit(x)[1:3]
    paste(vals, collapse = ", ")
  })
)

write_csv(dictionnaire, here("data", "metadata", "dictionnaire_variables.csv"))
cat("  ✅ Dictionnaire des variables sauvegardé\n")

# Sauvegarder l'environnement
save(
  dataset_final,
  bh_clean, hl_clean, hh_clean,
  missing_summary,
  file = here("data", "processed", "02_cleaned_data.RData")
)

cat("\n✨ Nettoyage terminé avec succès!\n")
cat("📁 Fichiers disponibles dans: data/processed/\n")
cat("\n🚀 Prochaine étape: Exécuter 03_feature_engineering.R\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================