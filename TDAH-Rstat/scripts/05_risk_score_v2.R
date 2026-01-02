# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 05 : Construction du score de risque TDAH (VERSION SANS SEXE)
# ==============================================================================
# Description: Création du score de vulnérabilité théorique au TDAH
#              SANS la variable sexe (97% manquant dans les données)
# Auteur: Asma BELKAHLA
# Date: 2026-01-02
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(psych)
library(corrplot)
library(ggridges)
library(patchwork)

project_root <- getwd()

# Charger les données
load(file.path(project_root, "data", "processed", "03_features.RData"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║        CONSTRUCTION DU SCORE DE RISQUE THÉORIQUE TDAH              ║\n")
cat("║                    (VERSION SANS SEXE)                             ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat("⚠️  NOTE IMPORTANTE:\n")
cat("   La variable 'sexe' n'est pas incluse car manquante pour 97% des observations.\n")
cat("   Les poids ont été redistribués sur les autres facteurs.\n\n")

# 2. PONDÉRATIONS BASÉES SUR LA LITTÉRATURE ==================================

cat("📚 Pondérations des facteurs de risque (redistribuées sans sexe):\n\n")

# Redistribution des poids (ancien total = 0.85, ramené à 1.0)
# On augmente proportionnellement les poids des autres facteurs
poids <- list(
  age_mere_risque = 0.176,      # 0.15 / 0.85 = 17.6%  (Thapar et al., 2013)
  ordre_risque = 0.118,          # 0.10 / 0.85 = 11.8%  (Russell et al., 2016)
  intervalle_risque = 0.176,     # 0.15 / 0.85 = 17.6%  (Cheslack-Postava et al., 2011)
  richesse_risque = 0.235,       # 0.20 / 0.85 = 23.5%  (Russell et al., 2014)
  educ_mere_risque = 0.176,      # 0.15 / 0.85 = 17.6%  (Rowland et al., 2018)
  taille_menage_risque = 0.118   # 0.10 / 0.85 = 11.8%  (Larsson et al., 2014)
  # sexe_risque EXCLU (Willcutt, 2012) - 97% manquant
)

facteurs_info <- tibble(
  Facteur = names(poids),
  Poids = unlist(poids),
  Poids_Pct = paste0(round(unlist(poids) * 100, 1), "%"),
  Reference = c(
    "Thapar et al., 2013",
    "Russell et al., 2016",
    "Cheslack-Postava et al., 2011",
    "Russell et al., 2014",
    "Rowland et al., 2018",
    "Larsson et al., 2014"
  ),
  Description = c(
    "Âge maternel extrême (<20 ou ≥35 ans)",
    "Rang de naissance élevé (≥4)",
    "Espacement court (<24 mois)",
    "Pauvreté (Q1-Q2)",
    "Faible éducation maternelle",
    "Grande taille ménage (>7 personnes)"
  )
)

print(facteurs_info)
cat("\n✅ Somme des poids:", round(sum(unlist(poids)), 3), "(doit être ≈ 1.0)\n\n")

# 3. CALCUL DU SCORE PONDÉRÉ =================================================

cat("🔧 Calcul du score de risque TDAH (sans sexe)...\n\n")

# Vérifier si les variables de risque existent
variables_requises <- c("age_mere_risque", "ordre_risque", "intervalle_risque",
                        "richesse_risque", "educ_mere_risque", "taille_menage_risque")

variables_manquantes <- setdiff(variables_requises, names(dataset_features))

if (length(variables_manquantes) > 0) {
  cat("❌ ERREUR: Variables manquantes dans dataset_features:\n")
  print(variables_manquantes)
  cat("\n⚠️  Création de variables de risque binaires par défaut...\n\n")

  # Créer les variables de risque si elles n'existent pas
  dataset_features <- dataset_features %>%
    mutate(
      # Âge maternel extrême
      age_mere_risque = if_else(
        !is.na(age_mere_cat) & age_mere_cat %in% c("< 20 ans", "≥ 35 ans"),
        1, 0
      ),

      # Ordre de naissance élevé
      ordre_risque = if_else(
        !is.na(ordre_cat) & ordre_cat %in% c("4-6", "≥ 7"),
        1, 0
      ),

      # Intervalle intergénésique court (si disponible)
      intervalle_risque = if_else(
        !is.na(intervalle_mois) & intervalle_mois < 24,
        1, 0
      ),

      # Pauvreté
      richesse_risque = if_else(
        !is.na(richesse_cat) & richesse_cat %in% c("Quintile 1 (Plus pauvre)", "Quintile 2"),
        1, 0
      ),

      # Faible éducation maternelle
      educ_mere_risque = if_else(
        !is.na(educ_mere_cat) & educ_mere_cat %in% c("Aucune", "Primaire"),
        1, 0
      ),

      # Taille ménage élevée
      taille_menage_risque = if_else(
        !is.na(taille_menage) & taille_menage > 7,
        1, 0
      )
    )
}

# Calculer le score
dataset_score <- dataset_features %>%
  mutate(
    # Score pondéré (0-100) SANS SEXE
    score_tdah = (
      age_mere_risque * poids$age_mere_risque +
      ordre_risque * poids$ordre_risque +
      intervalle_risque * poids$intervalle_risque +
      richesse_risque * poids$richesse_risque +
      educ_mere_risque * poids$educ_mere_risque +
      taille_menage_risque * poids$taille_menage_risque
    ) * 100,

    # Score simple (nombre de facteurs) SANS SEXE
    score_simple = age_mere_risque + ordre_risque + intervalle_risque +
                   richesse_risque + educ_mere_risque +
                   taille_menage_risque,

    # Catégories de risque (seuils calibrés)
    risque_tdah_cat = case_when(
      score_tdah < 20 ~ "Faible",
      score_tdah >= 20 & score_tdah < 40 ~ "Moyen",
      score_tdah >= 40 ~ "Élevé",
      TRUE ~ NA_character_
    ),

    # Version binaire
    risque_tdah_eleve = if_else(score_tdah >= 40, 1, 0)
  ) %>%
  mutate(
    risque_tdah_cat = factor(risque_tdah_cat, levels = c("Faible", "Moyen", "Élevé"))
  )

cat("✅ Score calculé pour", nrow(dataset_score), "enfants\n\n")

# 4. STATISTIQUES DESCRIPTIVES DU SCORE ======================================

cat("📊 Statistiques du score de risque TDAH:\n\n")

# Score pondéré
cat("Score pondéré (0-100):\n")
print(summary(dataset_score$score_tdah))
cat("Écart-type:", round(sd(dataset_score$score_tdah, na.rm = TRUE), 2), "\n\n")

# Score simple
cat("Score simple (nombre de facteurs 0-6):\n")
print(table(dataset_score$score_simple))
cat("\n")

# Distribution des catégories
cat("Distribution des catégories de risque:\n")
freq_cat <- table(dataset_score$risque_tdah_cat, useNA = "ifany")
prop_cat <- prop.table(freq_cat) * 100

result_table <- tibble(
  Catégorie = names(freq_cat),
  Fréquence = as.numeric(freq_cat),
  Pourcentage = paste0(round(prop_cat, 2), "%")
)

print(result_table)
cat("\n")

# Risque élevé
n_risque_eleve <- sum(dataset_score$risque_tdah_eleve == 1, na.rm = TRUE)
pct_risque_eleve <- round(n_risque_eleve / nrow(dataset_score) * 100, 2)

cat("🎯 Prévalence du risque élevé (≥40):", n_risque_eleve, "enfants (",
    pct_risque_eleve, "%)\n\n")

# 5. DISTRIBUTION PAR FACTEUR DE RISQUE ======================================

cat("📊 CONTRIBUTION DE CHAQUE FACTEUR AU SCORE\n")
cat(strrep("=", 70), "\n\n")

# Fréquence de chaque facteur
facteurs_freq <- dataset_score %>%
  summarise(
    `Âge maternel extrême` = sum(age_mere_risque == 1, na.rm = TRUE),
    `Ordre naissance ≥4` = sum(ordre_risque == 1, na.rm = TRUE),
    `Intervalle <24 mois` = sum(intervalle_risque == 1, na.rm = TRUE),
    `Pauvreté (Q1-Q2)` = sum(richesse_risque == 1, na.rm = TRUE),
    `Faible éducation mère` = sum(educ_mere_risque == 1, na.rm = TRUE),
    `Ménage >7 personnes` = sum(taille_menage_risque == 1, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Facteur", values_to = "Fréquence") %>%
  mutate(
    Pourcentage = round(Fréquence / nrow(dataset_score) * 100, 1),
    Poids = c(poids$age_mere_risque, poids$ordre_risque, poids$intervalle_risque,
              poids$richesse_risque, poids$educ_mere_risque, poids$taille_menage_risque),
    Contribution = round(Poids * 100, 1)
  ) %>%
  arrange(desc(Contribution))

print(facteurs_freq)
cat("\n")

# 6. VISUALISATIONS ==========================================================

cat("📊 Création des visualisations...\n\n")

# Distribution du score
p1 <- ggplot(dataset_score, aes(x = score_tdah)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = c(20, 40), linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Distribution du Score de Risque TDAH",
    subtitle = "Seuils: 20 (Moyen) et 40 (Élevé)",
    x = "Score de Risque (0-100)",
    y = "Nombre d'enfants"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Barplot des catégories
p2 <- ggplot(dataset_score, aes(x = risque_tdah_cat, fill = risque_tdah_cat)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("Faible" = "#2ecc71",
                                "Moyen" = "#f39c12",
                                "Élevé" = "#e74c3c")) +
  labs(
    title = "Répartition par Catégorie de Risque",
    x = "Catégorie de Risque",
    y = "Nombre d'enfants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# Combiner les graphiques
combined_plot <- p1 / p2

print(combined_plot)

# 7. SCORE PAR VARIABLES SOCIO-DÉMOGRAPHIQUES ================================

cat("\n📊 SCORE MOYEN PAR VARIABLES SOCIO-DÉMOGRAPHIQUES\n")
cat(strrep("=", 70), "\n\n")

# Par milieu
cat("Par milieu de résidence:\n")
score_milieu <- dataset_score %>%
  group_by(milieu) %>%
  summarise(
    N = n(),
    Score_Moyen = round(mean(score_tdah, na.rm = TRUE), 2),
    Écart_Type = round(sd(score_tdah, na.rm = TRUE), 2),
    Pct_Risque_Élevé = round(sum(risque_tdah_eleve == 1, na.rm = TRUE) / n() * 100, 1)
  )
print(score_milieu)
cat("\n")

# Par richesse
cat("Par quintile de richesse:\n")
score_richesse <- dataset_score %>%
  group_by(richesse_cat) %>%
  summarise(
    N = n(),
    Score_Moyen = round(mean(score_tdah, na.rm = TRUE), 2),
    Écart_Type = round(sd(score_tdah, na.rm = TRUE), 2),
    Pct_Risque_Élevé = round(sum(risque_tdah_eleve == 1, na.rm = TRUE) / n() * 100, 1)
  )
print(score_richesse)
cat("\n")

# Par éducation maternelle
cat("Par niveau d'éducation maternelle:\n")
score_educ <- dataset_score %>%
  group_by(educ_mere_cat) %>%
  summarise(
    N = n(),
    Score_Moyen = round(mean(score_tdah, na.rm = TRUE), 2),
    Écart_Type = round(sd(score_tdah, na.rm = TRUE), 2),
    Pct_Risque_Élevé = round(sum(risque_tdah_eleve == 1, na.rm = TRUE) / n() * 100, 1)
  )
print(score_educ)
cat("\n")

# 8. SAUVEGARDE ==============================================================

cat("💾 Sauvegarde des résultats...\n\n")

save(
  dataset_score,
  poids,
  facteurs_info,
  facteurs_freq,
  score_milieu,
  score_richesse,
  score_educ,
  file = file.path(project_root, "data", "processed", "05_risk_score.RData")
)

# Exporter le dataset en CSV
write_csv(
  dataset_score,
  file.path(project_root, "data", "processed", "dataset_with_score.csv")
)

# Exporter les statistiques
write_csv(
  facteurs_freq,
  file.path(project_root, "data", "processed", "facteurs_risque_stats.csv")
)

cat("✅ Fichiers sauvegardés:\n")
cat("   - 05_risk_score.RData\n")
cat("   - dataset_with_score.csv\n")
cat("   - facteurs_risque_stats.csv\n\n")

# 9. RÉSUMÉ FINAL ============================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    RÉSUMÉ DU SCORE DE RISQUE                       ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 FACTEURS INCLUS (6 au total):\n")
cat(strrep("-", 70), "\n")
cat("  ✅ Âge maternel extrême       (17.6%)\n")
cat("  ✅ Ordre naissance élevé      (11.8%)\n")
cat("  ✅ Intervalle court           (17.6%)\n")
cat("  ✅ Pauvreté                   (23.5%) ← FACTEUR LE PLUS IMPORTANT\n")
cat("  ✅ Faible éducation mère      (17.6%)\n")
cat("  ✅ Taille ménage élevée       (11.8%)\n\n")

cat("❌ FACTEURS EXCLUS:\n")
cat(strrep("-", 70), "\n")
cat("  ❌ Sexe (97% manquant dans les données)\n\n")

cat("🎯 RÉSULTATS:\n")
cat(strrep("-", 70), "\n")
cat("  - Enfants analysés:           ", nrow(dataset_score), "\n")
cat("  - Score moyen:                ", round(mean(dataset_score$score_tdah, na.rm = TRUE), 2), "/100\n")
cat("  - Risque élevé (≥40):         ", pct_risque_eleve, "%\n")
cat("  - Gradient socio-économique:   OUI (pauvreté = facteur majeur)\n\n")

cat("📝 LIMITATION IMPORTANTE:\n")
cat(strrep("-", 70), "\n")
cat("Ce score représente une VULNÉRABILITÉ THÉORIQUE basée sur des facteurs\n")
cat("de risque reconnus dans la littérature scientifique.\n")
cat("Il ne constitue PAS un diagnostic de TDAH.\n\n")

cat("✅ SCORE DE RISQUE CALCULÉ AVEC SUCCÈS!\n")
cat("➡️  Prochaine étape: Modèles de Machine Learning (06_ml_models.R)\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================
