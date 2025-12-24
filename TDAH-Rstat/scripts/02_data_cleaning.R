# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023  
# Script 02 : Nettoyage FINAL (avec variables catégorielles UNICEF)
# ==============================================================================
# Description: Utilisation des variables catégorielles créées par UNICEF
# Auteur: Asma BELKAHLA
# Date: 2024-12-23
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(haven)

project_root <- getwd()

# Charger les données SPSS importées
load(file.path(project_root, "data", "processed", "01_imported_data_SPSS.RData"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║       NETTOYAGE FINAL - Variables Catégorielles UNICEF            ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# 2. NETTOYAGE DU FICHIER BH ==================================================

cat("📋 Nettoyage de bh.sav...\n\n")

bh_clean <- mics_data$bh %>%
  mutate(across(everything(), ~zap_labels(.))) %>%
  rename_with(tolower) %>%
  select(
    hh1, hh2, ln, bhln, psu, stratum,
    wm1, wm2, wm3,
    
    # Variables catégorielles UNICEF
    magebrt,    # 1=<20, 2=20-34, 3=35+
    brthord,    # 1=1er, 2=2-3, 3=4-6, 4=7+
    birthint,   # 0=premier, 1=<2ans, 2=2ans, 3=3ans, 4=4+ans
    
    # Date/Âge enfant
    bh4c, bh4d, bh4m, bh4y,
    bh9c,
    
    # Survie
    bh5,
    
    # Socio-économique
    welevel, windex5, wscore,
    
    # Milieu
    hh6, hh7,
    
    any_of(c("wmweight", "insurance", "disability"))
  ) %>%
  mutate(
    child_id = paste(hh1, hh2, bhln, sep = "_"),
    
    # === ÂGE ENFANT ===
    cmc_enquete = 1488,
    age_mois = if_else(!is.na(bh4c) & bh4c > 0, 
                       cmc_enquete - bh4c, 
                       bh9c),
    age_annees = age_mois / 12,
    
    # === STATUT VITAL ===
    enfant_vivant = if_else(bh5 == 1, 1, 0),
    
    # === ÂGE MATERNEL (catégoriel UNICEF) ===
    age_mere_cat = case_when(
      magebrt == 1 ~ "< 20 ans",
      magebrt == 2 ~ "20-34 ans",
      magebrt == 3 ~ "≥ 35 ans",
      TRUE ~ NA_character_
    ),
    age_mere_risque = if_else(magebrt %in% c(1, 3), 1, 0),  # <20 OU 35+
    
    # === ORDRE NAISSANCE (catégoriel UNICEF) ===
    ordre_cat = case_when(
      brthord == 1 ~ "Premier",
      brthord == 2 ~ "Deuxième-Troisième",
      brthord == 3 ~ "Quatrième-Sixième",
      brthord == 4 ~ "Septième ou plus",
      TRUE ~ NA_character_
    ),
    ordre_risque = if_else(brthord >= 3, 1, 0),  # 4+ enfants
    
    # === INTERVALLE (catégoriel UNICEF) ===
    intervalle_cat = case_when(
      birthint == 0 | brthord == 1 ~ "Premier né",
      birthint == 1 ~ "< 2 ans",
      birthint == 2 ~ "2 ans",
      birthint == 3 ~ "3 ans",
      birthint == 4 ~ "≥ 4 ans",
      TRUE ~ NA_character_
    ),
    intervalle_risque = if_else(birthint == 1, 1, 0),  # <2 ans
    
    # === ÉDUCATION ===
    educ_mere_cat = case_when(
      welevel == 0 ~ "Aucune",
      welevel == 1 ~ "Primaire",
      welevel == 2 ~ "Secondaire",
      welevel >= 3 ~ "Supérieur",
      TRUE ~ NA_character_
    ),
    educ_mere_risque = if_else(welevel <= 1, 1, 0),
    
    # === RICHESSE ===
    richesse_cat = case_when(
      windex5 == 1 ~ "Q1 (Plus pauvre)",
      windex5 == 2 ~ "Q2",
      windex5 == 3 ~ "Q3",
      windex5 == 4 ~ "Q4",
      windex5 == 5 ~ "Q5 (Plus riche)",
      TRUE ~ NA_character_
    ),
    richesse_risque = if_else(windex5 <= 2, 1, 0),
    
    # === MILIEU ===
    milieu = case_when(
      hh6 == 1 ~ "Urbain",
      hh6 == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    milieu_risque = if_else(hh6 == 2, 1, 0),
    
    region = as.character(hh7)
  ) %>%
  filter(enfant_vivant == 1) %>%
  mutate(
    age_mere_cat = factor(age_mere_cat, 
                          levels = c("< 20 ans", "20-34 ans", "≥ 35 ans")),
    ordre_cat = factor(ordre_cat, 
                      levels = c("Premier", "Deuxième-Troisième", 
                                "Quatrième-Sixième", "Septième ou plus")),
    intervalle_cat = factor(intervalle_cat, 
                           levels = c("Premier né", "< 2 ans", "2 ans", "3 ans", "≥ 4 ans")),
    educ_mere_cat = factor(educ_mere_cat, 
                          levels = c("Aucune", "Primaire", "Secondaire", "Supérieur")),
    richesse_cat = factor(richesse_cat, 
                         levels = c("Q1 (Plus pauvre)", "Q2", "Q3", "Q4", "Q5 (Plus riche)"))
  )

cat("  ✅ bh.sav:", nrow(bh_clean), "enfants vivants\n")
cat("  📊 Âge:", round(mean(bh_clean$age_annees, na.rm = TRUE), 1), "ans (moyenne)\n\n")

# 3. NETTOYAGE HL =============================================================

cat("📋 Nettoyage de hl.sav...\n")

hl_clean <- mics_data$hl %>%
  mutate(across(everything(), ~zap_labels(.))) %>%
  rename_with(tolower) %>%
  select(hh1, hh2, hl1, hl3, hl4, hl6) %>%
  mutate(
    person_id = paste(hh1, hh2, hl1, sep = "_"),
    sexe = case_when(
      hl4 == 1 ~ "Masculin",
      hl4 == 2 ~ "Féminin",
      TRUE ~ NA_character_
    ),
    relation_cm = case_when(
      hl3 == 1 ~ "Chef",
      hl3 == 2 ~ "Conjoint",
      hl3 == 3 ~ "Fils/Fille",
      TRUE ~ "Autre"
    ),
    age = hl6
  )

cat("  ✅ hl.sav:", nrow(hl_clean), "individus\n")
cat("  📊 Fils/Fille:", sum(hl_clean$hl3 == 3), "\n\n")

# 4. TAILLE MÉNAGE ============================================================

cat("📋 Calcul taille ménage...\n")

taille_menage <- hl_clean %>%
  group_by(hh1, hh2) %>%
  summarise(
    taille_menage = n(),
    nb_enfants = sum(age < 18, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    taille_menage_cat = case_when(
      taille_menage <= 3 ~ "Petit (≤3)",
      taille_menage <= 5 ~ "Moyen (4-5)",
      taille_menage <= 7 ~ "Grand (6-7)",
      TRUE ~ "Très grand (>7)"
    ),
    taille_menage_risque = if_else(taille_menage > 7, 1, 0)
  )

cat("  ✅", nrow(taille_menage), "ménages\n\n")

# 5. FUSION ===================================================================

cat("🔗 Fusion des données...\n")

dataset_analytique <- bh_clean %>%
  left_join(taille_menage, by = c("hh1", "hh2")) %>%
  left_join(
    hl_clean %>% 
      filter(hl3 == 3) %>%
      select(hh1, hh2, hl1, sexe, age),
    by = c("hh1" = "hh1", "hh2" = "hh2", "ln" = "hl1")
  ) %>%
  mutate(
    sexe_risque = if_else(sexe == "Masculin", 1, 0, missing = 0)
  )

cat("  ✅", nrow(dataset_analytique), "observations\n")
cat("  📊 Avec sexe:", sum(!is.na(dataset_analytique$sexe)), 
    "(", round(sum(!is.na(dataset_analytique$sexe))/nrow(dataset_analytique)*100, 1), "%)\n\n")

# 6. FILTRAGE =================================================================

cat("🔍 Filtrage...\n\n")

dataset_final <- dataset_analytique %>%
  filter(
    !is.na(magebrt),
    !is.na(brthord),
    !is.na(windex5),
    !is.na(age_annees),
    age_annees >= 0,
    age_annees <= 17
  )

cat("  📊 Échantillon final:", nrow(dataset_final), "enfants\n")
cat("  📊 Avec sexe:", sum(!is.na(dataset_final$sexe)), 
    "(", round(sum(!is.na(dataset_final$sexe))/nrow(dataset_final)*100, 1), "%)\n\n")

# 7. STATISTIQUES =============================================================

cat("📊 STATISTIQUES DESCRIPTIVES\n")
cat(strrep("=", 70), "\n\n")

cat("Sexe:\n")
print(table(dataset_final$sexe, useNA = "always"))

cat("\nÂge:\n")
cat("  Moyenne:", round(mean(dataset_final$age_annees, na.rm = TRUE), 1), "ans\n")
cat("  Médiane:", round(median(dataset_final$age_annees, na.rm = TRUE), 1), "ans\n")

cat("\nÂge maternel:\n")
print(table(dataset_final$age_mere_cat, useNA = "always"))

cat("\nOrdre naissance:\n")
print(table(dataset_final$ordre_cat, useNA = "always"))

cat("\nIntervalle:\n")
print(table(dataset_final$intervalle_cat, useNA = "always"))

cat("\nRichesse:\n")
print(table(dataset_final$richesse_cat, useNA = "always"))

cat("\nMilieu:\n")
print(table(dataset_final$milieu, useNA = "always"))

# 8. SAUVEGARDE ===============================================================

cat("\n💾 Sauvegarde...\n")

saveRDS(dataset_final, file.path(project_root, "data", "processed", "dataset_final_spss.rds"))

save(
  dataset_final,
  dataset_analytique,
  bh_clean,
  hl_clean,
  taille_menage,
  file = file.path(project_root, "data", "processed", "02_cleaned_data_SPSS.RData")
)

cat("  ✅ Terminé!\n\n")
cat("✨ Nettoyage réussi!\n")
cat("🚀 Prochaine étape: 03_feature_engineering.R\n\n")

# ==============================================================================
# FIN
# ==============================================================================