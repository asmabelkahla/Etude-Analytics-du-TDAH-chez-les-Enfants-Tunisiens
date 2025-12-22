# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 03 : Construction du score de risque théorique TDAH
# ==============================================================================
# Description: Création d'un indicateur synthétique de vulnérabilité au TDAH
# Auteur: Asma Belkahla
# Date: 22/12/2025
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(here)
library(psych)      # Pour l'analyse factorielle si nécessaire
library(corrplot)   # Pour visualiser les corrélations
library(ggridges)   # Pour des graphiques de distribution

# Charger les données nettoyées
load(here("data", "processed", "02_cleaned_data.RData"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║        CONSTRUCTION DU SCORE DE RISQUE THÉORIQUE TDAH              ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# 2. FACTEURS DE RISQUE TDAH (BASÉS SUR LA LITTÉRATURE) ====================

cat("📚 Facteurs de risque du TDAH reconnus dans la littérature:\n\n")

facteurs_info <- tribble(
  ~Catégorie, ~Facteur, ~Poids, ~Reference,
  "Périnataux", "Âge maternel extrême (<20 ou ≥35)", 0.15, "Thapar et al., 2013",
  "Périnataux", "Rang de naissance élevé (≥4)", 0.10, "Russell et al., 2016",
  "Périnataux", "Espacement court (<24 mois)", 0.15, "Cheslack-Postava et al., 2011",
  "Socio-économiques", "Pauvreté (Q1-Q2)", 0.20, "Russell et al., 2014",
  "Socio-économiques", "Faible éducation maternelle", 0.15, "Rowland et al., 2018",
  "Familiaux", "Grande taille du ménage (>7)", 0.10, "Larsson et al., 2014",
  "Démographiques", "Sexe masculin", 0.15, "Willcutt, 2012"
)

print(facteurs_info)

# 3. CRÉATION DES VARIABLES DE RISQUE =======================================

cat("\n🔧 Création des variables de risque binaires...\n")

dataset_score <- dataset_final %>%
  mutate(
    # 1. FACTEURS PÉRINATAUX
    
    # Âge maternel à risque (déjà créé dans 02_)
    # age_mere_risque = 1 si <20 ou ≥35
    
    # Ordre de naissance à risque (déjà créé)
    # ordre_risque = 1 si ≥4
    
    # Intervalle intergénésique à risque (déjà créé)
    # intervalle_risque = 1 si <24 mois
    
    # 2. FACTEURS SOCIO-ÉCONOMIQUES
    
    # Pauvreté (Q1-Q2) (déjà créé)
    # richesse_risque = 1 si Q1 ou Q2
    
    # Faible éducation maternelle (déjà créé)
    # educ_mere_risque = 1 si aucune ou primaire
    
    # 3. FACTEURS FAMILIAUX
    
    # Grande taille du ménage (déjà créé)
    # taille_menage_risque = 1 si >7
    
    # 4. FACTEURS DÉMOGRAPHIQUES
    
    # Sexe masculin
    sexe_risque = ifelse(sexe == "Masculin", 1, 0),
    
    # Milieu rural (facteur de risque additionnel - accès aux soins)
    milieu_risque = ifelse(milieu == "Rural", 1, 0)
  )

# Vérifier la création des variables
cat("\n✅ Variables de risque créées:\n")
risk_vars <- c("age_mere_risque", "ordre_risque", "intervalle_risque",
               "richesse_risque", "educ_mere_risque", "taille_menage_risque",
               "sexe_risque", "milieu_risque")

for (var in risk_vars) {
  n_risk <- sum(dataset_score[[var]], na.rm = TRUE)
  pct_risk <- round(n_risk / nrow(dataset_score) * 100, 1)
  cat(sprintf("  - %-25s: %5d enfants (%5.1f%%)\n", var, n_risk, pct_risk))
}

# 4. CONSTRUCTION DU SCORE PONDÉRÉ ==========================================

cat("\n📊 Construction du score de risque pondéré...\n")

# Pondérations basées sur la littérature
poids <- list(
  age_mere_risque = 0.15,
  ordre_risque = 0.10,
  intervalle_risque = 0.15,
  richesse_risque = 0.20,
  educ_mere_risque = 0.15,
  taille_menage_risque = 0.10,
  sexe_risque = 0.15
  # milieu_risque = 0.00  # Pas inclus dans le score principal
)

# Vérifier que les poids somment à 1
cat("Somme des poids:", sum(unlist(poids)), "\n")

# Calculer le score pondéré (0-100)
dataset_score <- dataset_score %>%
  mutate(
    # Score pondéré (0-100)
    score_tdah = (
      age_mere_risque * poids$age_mere_risque +
      ordre_risque * poids$ordre_risque +
      intervalle_risque * poids$intervalle_risque +
      richesse_risque * poids$richesse_risque +
      educ_mere_risque * poids$educ_mere_risque +
      taille_menage_risque * poids$taille_menage_risque +
      sexe_risque * poids$sexe_risque
    ) * 100,
    
    # Score simple (nombre de facteurs de risque présents)
    score_simple = age_mere_risque + ordre_risque + intervalle_risque +
                   richesse_risque + educ_mere_risque + taille_menage_risque +
                   sexe_risque,
    
    # Catégories de risque basées sur le score pondéré
    risque_cat = case_when(
      score_tdah < 20 ~ "Faible",
      score_tdah >= 20 & score_tdah < 40 ~ "Moyen",
      score_tdah >= 40 ~ "Élevé",
      TRUE ~ NA_character_
    ),
    
    # Version binaire (risque élevé vs non)
    risque_eleve = ifelse(score_tdah >= 40, 1, 0)
  ) %>%
  mutate(
    risque_cat = factor(risque_cat, levels = c("Faible", "Moyen", "Élevé"))
  )

# 5. STATISTIQUES DESCRIPTIVES DU SCORE =====================================

cat("\n📈 Statistiques descriptives du score TDAH:\n\n")

# Score pondéré
cat("Score pondéré (0-100):\n")
summary_score <- summary(dataset_score$score_tdah)
print(summary_score)

cat("\nÉcart-type:", round(sd(dataset_score$score_tdah, na.rm = TRUE), 2), "\n")

# Score simple
cat("\nScore simple (nombre de facteurs):\n")
table_simple <- table(dataset_score$score_simple)
print(table_simple)

# Distribution des catégories de risque
cat("\nDistribution des catégories de risque:\n")
table_risque <- dataset_score %>%
  count(risque_cat) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    pct_cum = cumsum(pct)
  )
print(table_risque)

cat("\nPrévalence du risque élevé:", 
    sum(dataset_score$risque_eleve, na.rm = TRUE), "enfants (",
    round(mean(dataset_score$risque_eleve, na.rm = TRUE) * 100, 1), "%)\n")

# 6. VISUALISATIONS DU SCORE =================================================

cat("\n📊 Création des visualisations...\n")

# Créer le dossier figures si nécessaire
dir.create(here("reports", "figures"), showWarnings = FALSE, recursive = TRUE)

# 6.1 Distribution du score pondéré
p1 <- ggplot(dataset_score, aes(x = score_tdah)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "red", size = 1) +
  geom_vline(xintercept = c(20, 40), linetype = "dashed", color = "darkred") +
  annotate("text", x = 10, y = 0.03, label = "Faible", color = "darkgreen", size = 4) +
  annotate("text", x = 30, y = 0.03, label = "Moyen", color = "orange", size = 4) +
  annotate("text", x = 50, y = 0.03, label = "Élevé", color = "red", size = 4) +
  labs(
    title = "Distribution du Score de Risque Théorique TDAH",
    subtitle = paste0("N = ", nrow(dataset_score), " enfants"),
    x = "Score de risque (0-100)",
    y = "Densité"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(here("reports", "figures", "01_distribution_score.png"), 
       p1, width = 10, height = 6, dpi = 300)

# 6.2 Distribution par catégorie de risque
p2 <- dataset_score %>%
  count(risque_cat) %>%
  mutate(
    pct = n / sum(n) * 100,
    label = paste0(n, "\n(", round(pct, 1), "%)")
  ) %>%
  ggplot(aes(x = risque_cat, y = n, fill = risque_cat)) +
  geom_col(alpha = 0.8, color = "white", size = 1) +
  geom_text(aes(label = label), vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Faible" = "#2ecc71", 
                                "Moyen" = "#f39c12", 
                                "Élevé" = "#e74c3c")) +
  labs(
    title = "Répartition des Enfants par Catégorie de Risque TDAH",
    x = "Catégorie de risque",
    y = "Nombre d'enfants"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(here("reports", "figures", "02_categories_risque.png"), 
       p2, width = 10, height = 6, dpi = 300)

# 6.3 Score par sexe
p3 <- ggplot(dataset_score, aes(x = score_tdah, y = sexe, fill = sexe)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  geom_vline(xintercept = c(20, 40), linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(values = c("Masculin" = "#3498db", "Féminin" = "#e91e63")) +
  labs(
    title = "Distribution du Score TDAH selon le Sexe",
    x = "Score de risque (0-100)",
    y = "Sexe"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave(here("reports", "figures", "03_score_par_sexe.png"), 
       p3, width = 10, height = 6, dpi = 300)

# 6.4 Score par quintile de richesse
p4 <- ggplot(dataset_score, aes(x = richesse_cat, y = score_tdah, fill = richesse_cat)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(
    title = "Score TDAH selon le Quintile de Richesse",
    x = "Quintile de richesse",
    y = "Score de risque (0-100)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 20, hjust = 1)
  ) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1)

ggsave(here("reports", "figures", "04_score_par_richesse.png"), 
       p4, width = 10, height = 6, dpi = 300)

cat("  ✅ Graphiques sauvegardés dans reports/figures/\n")

# 7. ANALYSE DES CORRÉLATIONS ENTRE FACTEURS ================================

cat("\n🔍 Analyse des corrélations entre facteurs de risque...\n")

# Matrice de corrélation
cor_matrix <- dataset_score %>%
  select(all_of(risk_vars)) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

# Visualisation
png(here("reports", "figures", "05_correlation_facteurs.png"), 
    width = 800, height = 800, res = 120)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Corrélations entre Facteurs de Risque TDAH",
         mar = c(0,0,2,0))
dev.off()

cat("  ✅ Matrice de corrélation sauvegardée\n")

# 8. VALIDATION DU SCORE ====================================================

cat("\n✅ Validation du score de risque:\n\n")

# Cohérence interne (alpha de Cronbach)
alpha_result <- psych::alpha(dataset_score[, risk_vars], check.keys = TRUE)
cat("Alpha de Cronbach:", round(alpha_result$total$raw_alpha, 3), "\n")

# Distribution par sous-groupes
cat("\nScore moyen par sous-groupes:\n")
scores_groupes <- dataset_score %>%
  group_by(sexe, milieu) %>%
  summarise(
    n = n(),
    score_moyen = round(mean(score_tdah, na.rm = TRUE), 1),
    score_sd = round(sd(score_tdah, na.rm = TRUE), 1),
    pct_risque_eleve = round(mean(risque_eleve, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(scores_groupes)

# 9. SAUVEGARDE DES RÉSULTATS ================================================

cat("\n💾 Sauvegarde des résultats...\n")

# Sauvegarder le dataset avec scores
saveRDS(dataset_score, here("data", "processed", "dataset_with_score.rds"))
cat("  ✅ Dataset avec scores sauvegardé\n")

# Sauvegarder un résumé des scores
resume_scores <- tibble(
  statistique = c("N", "Moyenne", "Médiane", "Écart-type", "Min", "Max",
                  "% Risque faible", "% Risque moyen", "% Risque élevé"),
  valeur = c(
    nrow(dataset_score),
    round(mean(dataset_score$score_tdah, na.rm = TRUE), 2),
    round(median(dataset_score$score_tdah, na.rm = TRUE), 2),
    round(sd(dataset_score$score_tdah, na.rm = TRUE), 2),
    round(min(dataset_score$score_tdah, na.rm = TRUE), 2),
    round(max(dataset_score$score_tdah, na.rm = TRUE), 2),
    round(sum(dataset_score$risque_cat == "Faible", na.rm = TRUE) / 
            nrow(dataset_score) * 100, 1),
    round(sum(dataset_score$risque_cat == "Moyen", na.rm = TRUE) / 
            nrow(dataset_score) * 100, 1),
    round(sum(dataset_score$risque_cat == "Élevé", na.rm = TRUE) / 
            nrow(dataset_score) * 100, 1)
  )
)

write_csv(resume_scores, here("data", "processed", "resume_scores_tdah.csv"))
cat("  ✅ Résumé des scores sauvegardé\n")

# Sauvegarder l'environnement
save(
  dataset_score,
  facteurs_info,
  poids,
  cor_matrix,
  resume_scores,
  scores_groupes,
  file = here("data", "processed", "03_risk_score.RData")
)

cat("\n✨ Construction du score terminée avec succès!\n")
cat("📊 Graphiques disponibles dans: reports/figures/\n")
cat("\n🚀 Prochaine étape: Exécuter 04_descriptive_analysis.R\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================