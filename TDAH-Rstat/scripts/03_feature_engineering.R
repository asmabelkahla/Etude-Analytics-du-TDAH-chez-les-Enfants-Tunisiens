# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 03 : Feature Engineering - Création de variables dérivées
# ==============================================================================
# Description: Création de variables composites et d'interaction
# Auteur: Asma BELKAHLA
# Date: 2025-12-23
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(labelled)

project_root <- getwd()

# Charger les données nettoyées
load(file.path(project_root, "data", "processed", "02_cleaned_data_SPSS.RData"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║              FEATURE ENGINEERING - VARIABLES DÉRIVÉES              ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 Dataset de départ:", nrow(dataset_final), "enfants\n\n")

# 2. VARIABLES D'INTERACTION ==================================================

cat("🔧 Création des variables d'interaction...\n\n")

dataset_features <- dataset_final %>%
  mutate(
    # === INTERACTIONS SOCIO-DÉMOGRAPHIQUES ===
    
    # Pauvreté urbaine vs rurale
    pauvrete_milieu = case_when(
      richesse_risque == 1 & milieu == "Urbain" ~ "Pauvre urbain",
      richesse_risque == 1 & milieu == "Rural" ~ "Pauvre rural",
      richesse_risque == 0 & milieu == "Urbain" ~ "Non-pauvre urbain",
      richesse_risque == 0 & milieu == "Rural" ~ "Non-pauvre rural",
      TRUE ~ NA_character_
    ),
    
    # Éducation et richesse combinées
    educ_richesse = case_when(
      educ_mere_risque == 1 & richesse_risque == 1 ~ "Faible éduc + Pauvre",
      educ_mere_risque == 1 & richesse_risque == 0 ~ "Faible éduc + Non-pauvre",
      educ_mere_risque == 0 & richesse_risque == 1 ~ "Bonne éduc + Pauvre",
      educ_mere_risque == 0 & richesse_risque == 0 ~ "Bonne éduc + Non-pauvre",
      TRUE ~ NA_character_
    ),
    
    # === CUMUL DE FACTEURS PÉRINATAUX ===
    
    # Nombre de facteurs périnataux à risque
    nb_risques_perinataux = (age_mere_risque + ordre_risque + intervalle_risque),
    
    # Catégorie de risque périnatal
    risque_perinatal_cat = case_when(
      nb_risques_perinataux == 0 ~ "Aucun risque",
      nb_risques_perinataux == 1 ~ "1 risque",
      nb_risques_perinataux >= 2 ~ "2+ risques",
      TRUE ~ NA_character_
    ),
    
    # === CUMUL DE FACTEURS SOCIO-ÉCONOMIQUES ===
    
    # Nombre de facteurs socio-économiques à risque
    nb_risques_socioeco = (richesse_risque + educ_mere_risque + milieu_risque),
    
    # Catégorie de risque socio-économique
    risque_socioeco_cat = case_when(
      nb_risques_socioeco == 0 ~ "Aucun risque",
      nb_risques_socioeco == 1 ~ "1 risque",
      nb_risques_socioeco >= 2 ~ "2+ risques",
      TRUE ~ NA_character_
    ),
    
    # === VARIABLES D'ÂGE GROUPÉ ===
    
    # Groupes d'âge larges
    age_groupe = case_when(
      age_annees < 6 ~ "0-5 ans (Préscolaire)",
      age_annees >= 6 & age_annees < 12 ~ "6-11 ans (Primaire)",
      age_annees >= 12 & age_annees < 15 ~ "12-14 ans (Collège)",
      age_annees >= 15 ~ "15-17 ans (Lycée)",
      TRUE ~ NA_character_
    ),
    
    # Âge maternel détaillé
    age_mere_detail = case_when(
      magebrt < 18 ~ "< 18 ans",
      magebrt >= 18 & magebrt < 20 ~ "18-19 ans",
      magebrt >= 20 & magebrt < 25 ~ "20-24 ans",
      magebrt >= 25 & magebrt < 30 ~ "25-29 ans",
      magebrt >= 30 & magebrt < 35 ~ "30-34 ans",
      magebrt >= 35 & magebrt < 40 ~ "35-39 ans",
      magebrt >= 40 ~ "≥ 40 ans",
      TRUE ~ NA_character_
    ),
    
    # === PROFILS FAMILIAUX ===
    
    # Famille nombreuse avec espacement court
    famille_vulnerable = case_when(
      taille_menage_risque == 1 & intervalle_risque == 1 ~ "Très vulnérable",
      taille_menage_risque == 1 | intervalle_risque == 1 ~ "Vulnérable",
      taille_menage_risque == 0 & intervalle_risque == 0 ~ "Favorable",
      TRUE ~ NA_character_
    ),
    
    # Position dans la fratrie
    position_fratrie = case_when(
      brthord == 1 ~ "Aîné",
      brthord == 2 ~ "Cadet",
      brthord >= 3 & brthord <= 4 ~ "Milieu (3-4)",
      brthord > 4 ~ "Dernier/Grand rang (5+)",
      TRUE ~ NA_character_
    ),
    
    # === INDICES COMPOSITES ===
    
    # Indice de vulnérabilité familiale (0-3)
    vulnerabilite_familiale = (
      if_else(taille_menage > median(taille_menage, na.rm = TRUE), 1, 0) +
      if_else(brthord >= 3, 1, 0) +
      if_else(!is.na(birthint) & birthint < 36, 1, 0)
    ),
    
    # Indice d'adversité socio-économique (0-4)
    adversite_socioeco = (
      richesse_risque +
      educ_mere_risque +
      milieu_risque +
      if_else(taille_menage_risque == 1, 1, 0)
    ),
    
    # === VARIABLES BINAIRES COMBINÉES ===
    
    # Au moins 1 risque périnatal
    risque_perinatal_present = if_else(nb_risques_perinataux >= 1, 1, 0),
    
    # Au moins 1 risque socio-économique
    risque_socioeco_present = if_else(nb_risques_socioeco >= 1, 1, 0),
    
    # Cumul risque périnatal + socio-économique
    double_risque = if_else(
      risque_perinatal_present == 1 & risque_socioeco_present == 1, 1, 0
    )
  ) %>%
  
  # Convertir en facteurs
  mutate(
    pauvrete_milieu = factor(pauvrete_milieu,
      levels = c("Non-pauvre urbain", "Non-pauvre rural", 
                 "Pauvre urbain", "Pauvre rural")),
    
    educ_richesse = factor(educ_richesse,
      levels = c("Bonne éduc + Non-pauvre", "Bonne éduc + Pauvre",
                 "Faible éduc + Non-pauvre", "Faible éduc + Pauvre")),
    
    risque_perinatal_cat = factor(risque_perinatal_cat,
      levels = c("Aucun risque", "1 risque", "2+ risques")),
    
    risque_socioeco_cat = factor(risque_socioeco_cat,
      levels = c("Aucun risque", "1 risque", "2+ risques")),
    
    age_groupe = factor(age_groupe,
      levels = c("0-5 ans (Préscolaire)", "6-11 ans (Primaire)",
                 "12-14 ans (Collège)", "15-17 ans (Lycée)")),
    
    famille_vulnerable = factor(famille_vulnerable,
      levels = c("Favorable", "Vulnérable", "Très vulnérable")),
    
    position_fratrie = factor(position_fratrie,
      levels = c("Aîné", "Cadet", "Milieu (3-4)", "Dernier/Grand rang (5+)"))
  )

cat("✅ Variables d'interaction créées\n\n")

# 3. STATISTIQUES DESCRIPTIVES DES NOUVELLES VARIABLES =======================

cat("📊 Distribution des nouvelles variables:\n\n")

# Cumul de risques périnataux
cat("Risques périnataux cumulés:\n")
print(table(dataset_features$risque_perinatal_cat, useNA = "ifany"))

# Cumul de risques socio-économiques
cat("\nRisques socio-économiques cumulés:\n")
print(table(dataset_features$risque_socioeco_cat, useNA = "ifany"))

# Double risque
cat("\nDouble risque (périnatal + socio-éco):\n")
print(table(dataset_features$double_risque, useNA = "ifany"))
cat("Proportion:", round(mean(dataset_features$double_risque, na.rm = TRUE) * 100, 1), "%\n")

# Pauvreté selon le milieu
cat("\nPauvreté selon le milieu:\n")
print(table(dataset_features$pauvrete_milieu, useNA = "ifany"))

# Groupes d'âge
cat("\nDistribution par groupe d'âge:\n")
print(table(dataset_features$age_groupe, useNA = "ifany"))

# 4. MATRICE DE CORRÉLATIONS ENTRE FACTEURS ==================================

cat("\n📈 Corrélations entre facteurs de risque:\n\n")

risk_vars <- dataset_features %>%
  select(
    age_mere_risque, ordre_risque, intervalle_risque,
    richesse_risque, educ_mere_risque, milieu_risque,
    taille_menage_risque, sexe_risque
  )

cor_matrix <- cor(risk_vars, use = "pairwise.complete.obs")
print(round(cor_matrix, 2))

# 5. ANALYSES CROISÉES ========================================================

cat("\n📊 Analyses croisées:\n\n")

# Risque périnatal selon le milieu
cat("Risque périnatal selon le milieu de résidence:\n")
risk_milieu <- dataset_features %>%
  count(milieu, risque_perinatal_cat) %>%
  group_by(milieu) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()
print(risk_milieu)

# Risque socio-économique selon la richesse
cat("\nRisque socio-économique selon le quintile de richesse:\n")
risk_richesse <- dataset_features %>%
  count(richesse_cat, risque_socioeco_cat) %>%
  group_by(richesse_cat) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()
print(risk_richesse)

# 6. CRÉATION DE SOUS-GROUPES POUR ANALYSES STRATIFIÉES ======================

cat("\n🎯 Identification des sous-groupes clés:\n\n")

dataset_features <- dataset_features %>%
  mutate(
    # Groupe 1: Enfants à haut risque multiple (3+ facteurs)
    haut_risque_multiple = if_else(
      (nb_risques_perinataux + nb_risques_socioeco) >= 3, 1, 0
    ),
    
    # Groupe 2: Garçons avec facteurs cumulés
    garcon_risque = if_else(
      sexe == "Masculin" & (nb_risques_perinataux + nb_risques_socioeco) >= 2, 1, 0
    ),
    
    # Groupe 3: Milieu rural défavorisé
    rural_defavorise = if_else(
      milieu == "Rural" & richesse_risque == 1 & educ_mere_risque == 1, 1, 0
    )
  )

cat("Sous-groupes identifiés:\n")
cat("  - Haut risque multiple (3+ facteurs):", 
    sum(dataset_features$haut_risque_multiple), "enfants (",
    round(mean(dataset_features$haut_risque_multiple) * 100, 1), "%)\n")
cat("  - Garçons avec risques cumulés:", 
    sum(dataset_features$garcon_risque), "enfants (",
    round(mean(dataset_features$garcon_risque) * 100, 1), "%)\n")
cat("  - Rural défavorisé:", 
    sum(dataset_features$rural_defavorise), "enfants (",
    round(mean(dataset_features$rural_defavorise) * 100, 1), "%)\n")

# 7. SAUVEGARDE ===============================================================

cat("\n💾 Sauvegarde des données enrichies...\n")

saveRDS(
  dataset_features,
  file.path(project_root, "data", "processed", "dataset_features.rds")
)

# Créer un dictionnaire des nouvelles variables
nouvelles_vars <- tibble(
  variable = setdiff(names(dataset_features), names(dataset_final)),
  description = c(
    "Pauvreté selon milieu urbain/rural",
    "Éducation maternelle et richesse combinées",
    "Nombre de risques périnataux (0-3)",
    "Catégorie de risque périnatal",
    "Nombre de risques socio-économiques (0-3)",
    "Catégorie de risque socio-économique",
    "Groupe d'âge de l'enfant",
    "Âge maternel détaillé",
    "Vulnérabilité familiale",
    "Position dans la fratrie",
    "Indice de vulnérabilité familiale (0-3)",
    "Indice d'adversité socio-économique (0-4)",
    "Présence d'au moins 1 risque périnatal",
    "Présence d'au moins 1 risque socio-économique",
    "Double risque (périnatal + socio-éco)",
    "Haut risque multiple (3+ facteurs)",
    "Garçons avec risques cumulés",
    "Rural défavorisé (pauvre + faible éduc)"
  )[1:length(setdiff(names(dataset_features), names(dataset_final)))]
)

write_csv(
  nouvelles_vars,
  file.path(project_root, "data", "metadata", "nouvelles_variables.csv")
)

save(
  dataset_features,
  cor_matrix,
  risk_milieu,
  risk_richesse,
  file = file.path(project_root, "data", "processed", "03_features.RData")
)

cat("  ✅ Données sauvegardées:", nrow(dataset_features), "enfants\n")
cat("  ✅ Nouvelles variables:", 
    length(setdiff(names(dataset_features), names(dataset_final))), "\n\n")

cat("✨ Feature engineering terminé!\n")
cat("🚀 Prochaine étape: 04_descriptive_analysis.R\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================