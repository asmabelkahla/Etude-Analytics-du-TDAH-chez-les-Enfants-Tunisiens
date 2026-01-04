# PROJET TDAH TUNISIE - MICS6 2023
# Script 02 : Nettoyage des données
# Auteur: Asma BELKAHLA

rm(list = ls())
gc()

library(tidyverse)
library(haven)

project_root <- getwd()
load(file.path(project_root, "data", "processed", "01_imported_data_SPSS.RData"))

cat("Nettoyage des données MICS6\n")

bh_clean <- mics_data$bh %>%
  mutate(across(everything(), ~zap_labels(.))) %>%
  rename_with(tolower) %>%
  select(
    hh1, hh2, ln, bhln, psu, stratum,
    wm1, wm2, wm3,
    magebrt, brthord, birthint,
    bh4c, bh4d, bh4m, bh4y, bh9c, bh5,
    welevel, windex5, wscore, hh6, hh7,
    any_of(c("wmweight", "insurance", "disability"))
  ) %>%
  mutate(
    child_id = paste(hh1, hh2, bhln, sep = "_"),
    cmc_enquete = 1488,
    age_mois = if_else(!is.na(bh4c) & bh4c > 0,
                       cmc_enquete - bh4c,
                       bh9c),
    age_annees = age_mois / 12,
    enfant_vivant = if_else(bh5 == 1, 1, 0),
    age_mere_cat = case_when(
      magebrt == 1 ~ "< 20 ans",
      magebrt == 2 ~ "20-34 ans",
      magebrt == 3 ~ "≥ 35 ans",
      TRUE ~ NA_character_
    ),
    age_mere_risque = if_else(magebrt %in% c(1, 3), 1, 0),
    ordre_cat = case_when(
      brthord == 1 ~ "Premier",
      brthord == 2 ~ "Deuxième-Troisième",
      brthord == 3 ~ "Quatrième-Sixième",
      brthord == 4 ~ "Septième ou plus",
      TRUE ~ NA_character_
    ),
    ordre_risque = if_else(brthord >= 3, 1, 0),
    intervalle_cat = case_when(
      birthint == 0 | brthord == 1 ~ "Premier né",
      birthint == 1 ~ "< 2 ans",
      birthint == 2 ~ "2 ans",
      birthint == 3 ~ "3 ans",
      birthint == 4 ~ "≥ 4 ans",
      TRUE ~ NA_character_
    ),
    intervalle_risque = if_else(birthint == 1, 1, 0),
    educ_mere_cat = case_when(
      welevel == 0 ~ "Aucune",
      welevel == 1 ~ "Primaire",
      welevel == 2 ~ "Secondaire",
      welevel >= 3 ~ "Supérieur",
      TRUE ~ NA_character_
    ),
    educ_mere_risque = if_else(welevel <= 1, 1, 0),
    richesse_cat = case_when(
      windex5 == 1 ~ "Q1 (Plus pauvre)",
      windex5 == 2 ~ "Q2",
      windex5 == 3 ~ "Q3",
      windex5 == 4 ~ "Q4",
      windex5 == 5 ~ "Q5 (Plus riche)",
      TRUE ~ NA_character_
    ),
    richesse_risque = if_else(windex5 <= 2, 1, 0),
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

dataset_final <- dataset_analytique %>%
  filter(
    !is.na(magebrt),
    !is.na(brthord),
    !is.na(windex5),
    !is.na(age_annees),
    age_annees >= 0,
    age_annees <= 17
  )

saveRDS(dataset_final, file.path(project_root, "data", "processed", "dataset_final_spss.rds"))

save(
  dataset_final,
  dataset_analytique,
  bh_clean,
  hl_clean,
  taille_menage,
  file = file.path(project_root, "data", "processed", "02_cleaned_data_SPSS.RData")
)

cat("Nettoyage terminé:", nrow(dataset_final), "enfants\n")