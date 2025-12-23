
# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 02 : Nettoyage des données SPSS
# ==============================================================================
# Description: Nettoyage des fichiers SPSS avec préservation des labels
# Auteur: Asma BELKAHLA
# Date: 2024-12-23
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(haven)
library(labelled)
library(janitor)

# Définir la racine du projet
project_root <- getwd()

# Charger les données SPSS importées
load(file.path(project_root, "data", "processed", "01_imported_data_SPSS.RData"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║       NETTOYAGE DES DONNÉES MICS6 (FORMAT SPSS)                   ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# 2. NETTOYAGE DU FICHIER BH (HISTORIQUE DES NAISSANCES) ====================

cat("📋 Nettoyage de bh.sav (historique des naissances)...\n")

# Vérifier les variables disponibles
cat("\n🔍 Variables dans BH:\n")
cat("  Total:", ncol(mics_data$bh), "variables\n")
cat("  Variables clés:", paste(names(mics_data$bh)[1:20], collapse = ", "), "\n\n")

bh_clean <- mics_data$bh %>%
  # Convertir les noms en minuscules (mais préserver les labels)
  rename_with(tolower) %>%
  
  # Sélectionner les variables clés
  select(
    # Identifiants
    hh1, hh2, ln, bhln, psu, stratum,
    
    # Variables périnatales
    brthord,        # Ordre de naissance
    magebrt,        # Âge maternel à la naissance
    birthint,       # Intervalle intergénésique
    
    # Date/Âge de l'enfant
    bh4d, bh4m, bh4y,  # Date de naissance (jour, mois, année)
    bh4c, bh4f,        # Date CMC et flag
    bh9c, bh9n, bh9u,  # Âge en mois (plusieurs versions)
    
    # Survie de l'enfant
    bh5, bh6,          # Vivant/décédé
    
    # Variables socio-économiques
    welevel,           # Éducation maternelle
    windex5, windex10, # Quintile/Décile de richesse
    wscore,            # Score de richesse
    
    # Milieu et région (du ménage)
    hh6, hh7,
    
    # Autres
    any_of(c("insurance", "disability", "wmweight"))
  ) %>%
  
  # Créer les variables dérivées
  mutate(
    # Identifiant unique enfant
    child_id = paste(hh1, hh2, bhln, sep = "_"),
    
    # === CALCUL DE L'ÂGE ===
    # CMC enquête = décembre 2023 = (2023-1900)*12 + 12 = 1488
    cmc_enquete = 1488,
    
    # Méthode 1: Depuis BH4C (CMC de naissance)
    age_mois_m1 = if_else(!is.na(bh4c) & bh4c > 0, 
                          cmc_enquete - bh4c, 
                          NA_real_),
    
    # Méthode 2: Depuis BH9C (âge déclaré en mois)
    age_mois_m2 = bh9c,
    
    # Méthode 3: Calculer depuis année/mois de naissance
    age_mois_m3 = if_else(!is.na(bh4y) & !is.na(bh4m),
                          (2023 - bh4y) * 12 + (12 - bh4m),
                          NA_real_),
    
    # Choisir la meilleure estimation (priorité: BH4C > BH9C > calculé)
    age_mois = coalesce(age_mois_m1, age_mois_m2, age_mois_m3),
    age_annees = age_mois / 12,
    
    # === STATUT VITAL ===
    # BH5: 1=vivant, 2=décédé
    enfant_vivant = case_when(
      bh5 == 1 ~ 1,
      bh5 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # === FACTEURS PÉRINATAUX ===
    
    # Âge maternel
    age_mere_cat = case_when(
      is.na(magebrt) ~ NA_character_,
      magebrt < 20 ~ "< 20 ans",
      magebrt >= 20 & magebrt < 35 ~ "20-34 ans",
      magebrt >= 35 ~ "≥ 35 ans"
    ),
    age_mere_risque = if_else(magebrt < 20 | magebrt >= 35, 1, 0),
    
    # Ordre de naissance
    ordre_cat = case_when(
      is.na(brthord) ~ NA_character_,
      brthord == 1 ~ "Premier",
      brthord == 2 ~ "Deuxième",
      brthord == 3 ~ "Troisième",
      brthord >= 4 ~ "Quatrième ou plus"
    ),
    ordre_risque = if_else(brthord >= 4, 1, 0),
    
    # Intervalle intergénésique
    intervalle_cat = case_when(
      is.na(birthint) | brthord == 1 ~ "Premier né",
      birthint < 24 ~ "< 24 mois",
      birthint >= 24 & birthint < 36 ~ "24-35 mois",
      birthint >= 36 ~ "≥ 36 mois"
    ),
    intervalle_risque = if_else(!is.na(birthint) & birthint < 24, 1, 0),
    
    # === FACTEURS SOCIO-ÉCONOMIQUES ===
    
    # Éducation maternelle
    educ_mere_cat = case_when(
      is.na(welevel) ~ NA_character_,
      welevel == 0 ~ "Aucune",
      welevel == 1 ~ "Primaire",
      welevel == 2 ~ "Secondaire",
      welevel >= 3 ~ "Supérieur"
    ),
    educ_mere_risque = if_else(welevel <= 1, 1, 0),
    
    # Richesse
    richesse_cat = case_when(
      is.na(windex5) ~ NA_character_,
      windex5 == 1 ~ "Q1 (Plus pauvre)",
      windex5 == 2 ~ "Q2",
      windex5 == 3 ~ "Q3",
      windex5 == 4 ~ "Q4",
      windex5 == 5 ~ "Q5 (Plus riche)"
    ),
    richesse_risque = if_else(windex5 <= 2, 1, 0),
    
    # Milieu de résidence
    milieu = case_when(
      is.na(hh6) ~ NA_character_,
      hh6 == 1 ~ "Urbain",
      hh6 == 2 ~ "Rural"
    ),
    milieu_risque = if_else(hh6 == 2, 1, 0),
    
    # Région
    region = as.character(hh7)
  ) %>%
  
  # Filtrer les enfants vivants
  filter(enfant_vivant == 1) %>%
  
  # Convertir les catégories en facteurs
  mutate(
    age_mere_cat = factor(age_mere_cat, 
                          levels = c("< 20 ans", "20-34 ans", "≥ 35 ans")),
    ordre_cat = factor(ordre_cat, 
                      levels = c("Premier", "Deuxième", "Troisième", "Quatrième ou plus")),
    intervalle_cat = factor(intervalle_cat, 
                           levels = c("Premier né", "< 24 mois", "24-35 mois", "≥ 36 mois")),
    educ_mere_cat = factor(educ_mere_cat, 
                          levels = c("Aucune", "Primaire", "Secondaire", "Supérieur")),
    richesse_cat = factor(richesse_cat, 
                         levels = c("Q1 (Plus pauvre)", "Q2", "Q3", "Q4", "Q5 (Plus riche)"))
  )

cat("  ✅ bh.sav nettoyé:", nrow(bh_clean), "enfants vivants\n")
cat("  📊 Âge min:", round(min(bh_clean$age_annees, na.rm = TRUE), 1), "ans\n")
cat("  📊 Âge max:", round(max(bh_clean$age_annees, na.rm = TRUE), 1), "ans\n")
cat("  📊 Âge moyen:", round(mean(bh_clean$age_annees, na.rm = TRUE), 1), "ans\n")
cat("  📊 Enfants avec âge:", sum(!is.na(bh_clean$age_annees)), "\n\n")

# 3. NETTOYAGE DU FICHIER HL (MEMBRES DU MÉNAGE) ============================

cat("📋 Nettoyage de hl.sav (membres du ménage)...\n")

hl_clean <- mics_data$hl %>%
  rename_with(tolower) %>%
  select(
    hh1, hh2, hl1, hl3, hl4, hl6,
    any_of(c("ed4", "psu", "stratum"))
  ) %>%
  mutate(
    person_id = paste(hh1, hh2, hl1, sep = "_"),
    
    # Sexe (1=Masculin, 2=Féminin)
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

cat("  ✅ hl.sav nettoyé:", nrow(hl_clean), "individus\n\n")

# 4. CALCUL DE LA TAILLE DU MÉNAGE ===========================================

cat("📋 Calcul des caractéristiques du ménage...\n")

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
      taille_menage > 7 ~ "Très grand (>7)"
    ),
    taille_menage_risque = if_else(taille_menage > 7, 1, 0)
  )

cat("  ✅ Caractéristiques calculées pour", nrow(taille_menage), "ménages\n\n")

# 5. FUSION DES DONNÉES =======================================================

cat("🔗 Fusion des fichiers...\n")

dataset_analytique <- bh_clean %>%
  # Joindre taille du ménage
  left_join(taille_menage, by = c("hh1", "hh2")) %>%
  
  # Joindre sexe de l'enfant depuis HL
  left_join(
    hl_clean %>% 
      filter(hl3 == 3) %>%  # Fils/Fille uniquement
      select(hh1, hh2, hl1, sexe, age, groupe_age),
    by = c("hh1", "hh2", "ln" = "hl1")
  ) %>%
  
  # Ajouter variable sexe_risque
  mutate(
    sexe_risque = if_else(sexe == "Masculin", 1, 0)
  )

cat("  ✅ Dataset analytique:", nrow(dataset_analytique), "observations\n\n")

# 6. CRITÈRES D'ÉLIGIBILITÉ ===================================================

cat("🔍 Application des critères d'éligibilité...\n")

# Statistiques avant filtrage
cat("\n📊 Avant filtrage:\n")
cat("  - Total:", nrow(dataset_analytique), "enfants\n")
cat("  - Avec âge valide:", sum(!is.na(dataset_analytique$age_annees)), "\n")
cat("  - Avec info mère:", sum(!is.na(dataset_analytique$magebrt)), "\n")
cat("  - Avec info richesse:", sum(!is.na(dataset_analytique$windex5)), "\n")

# Filtrage progressif
dataset_final <- dataset_analytique %>%
  filter(
    !is.na(magebrt),      # Âge maternel disponible
    !is.na(brthord),      # Ordre de naissance disponible
    !is.na(windex5),      # Richesse disponible
    !is.na(age_annees),   # Âge disponible
    age_annees >= 0,      # Âge valide
    age_annees <= 17      # Moins de 18 ans
  )

cat("\n📊 Après filtrage:\n")
cat("  - Échantillon final:", nrow(dataset_final), "enfants\n")
cat("  - Exclus:", nrow(dataset_analytique) - nrow(dataset_final), "\n\n")

# 7. STATISTIQUES DESCRIPTIVES ================================================

if (nrow(dataset_final) > 0) {
  cat("📈 Statistiques descriptives:\n\n")
  
  cat("Sexe:\n")
  print(table(dataset_final$sexe, useNA = "ifany"))
  
  cat("\nÂge:\n")
  cat("  Moyenne:", round(mean(dataset_final$age_annees, na.rm = TRUE), 1), "ans\n")
  cat("  Médiane:", round(median(dataset_final$age_annees, na.rm = TRUE), 1), "ans\n")
  
  cat("\nÂge maternel:\n")
  print(table(dataset_final$age_mere_cat, useNA = "ifany"))
  
  cat("\nOrdre de naissance:\n")
  print(table(dataset_final$ordre_cat, useNA = "ifany"))
  
  cat("\nRichesse:\n")
  print(table(dataset_final$richesse_cat, useNA = "ifany"))
  
  cat("\nMilieu:\n")
  print(table(dataset_final$milieu, useNA = "ifany"))
  
} else {
  cat("⚠️  Échantillon final vide!\n")
}

# 8. SAUVEGARDE ===============================================================

cat("\n💾 Sauvegarde...\n")

saveRDS(dataset_final, file.path(project_root, "data", "processed", "dataset_final_spss.rds"))
saveRDS(bh_clean, file.path(project_root, "data", "processed", "bh_clean_spss.rds"))
saveRDS(hl_clean, file.path(project_root, "data", "processed", "hl_clean_spss.rds"))

save(
  dataset_final,
  dataset_analytique,
  bh_clean,
  hl_clean,
  taille_menage,
  file = file.path(project_root, "data", "processed", "02_cleaned_data_SPSS.RData")
)

cat("  ✅ Données sauvegardées\n\n")
cat("✨ Nettoyage SPSS terminé!\n")
cat("🚀 Prochaine étape: 03_risk_score.R\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================