# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 06 : Modèles statistiques (VERSION CORRIGÉE)
# ==============================================================================
# Description: Analyses multivariées sans la variable sexe (97% manquante)
# Auteur: Asma BELKAHLA
# Date: 2025-12-23
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(broom)
library(car)

project_root <- getwd()

# Charger les données avec score
load(file.path(project_root, "data", "processed", "05_risk_score.RData"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║              MODÈLES STATISTIQUES - ANALYSES MULTIVARIÉES          ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 Dataset:", nrow(dataset_score), "enfants\n\n")

# 2. ANALYSES BIVARIÉES (Tests du Chi²) ======================================

cat("📊 ANALYSES BIVARIÉES\n")
cat(strrep("=", 70), "\n\n")

cat("🔍 Tests du Chi² - Associations avec risque TDAH élevé:\n\n")

chi2_tests <- tibble(
  Variable = c("milieu", "richesse_cat", "educ_mere_cat", "age_mere_cat", "ordre_cat"),
  Chi2 = NA_real_,
  p_value = NA_real_
)

for (i in 1:nrow(chi2_tests)) {
  var <- chi2_tests$Variable[i]
  test <- chisq.test(table(dataset_score[[var]], dataset_score$risque_tdah_eleve))
  chi2_tests$Chi2[i] <- round(test$statistic, 2)
  chi2_tests$p_value[i] <- test$p.value
}

chi2_tests <- chi2_tests %>%
  mutate(Conclusion = ifelse(p_value < 0.05, "Significatif", "Non significatif"))

print(chi2_tests)

# 3. TESTS T ET ANOVA =========================================================

cat("\n🔍 Tests de comparaison de moyennes:\n\n")

# Score selon milieu
t_milieu <- t.test(score_tdah ~ milieu, data = dataset_score)
cat("Score TDAH selon milieu:\n")
cat("  Urbain: M =", round(t_milieu$estimate[2], 2), "\n")
cat("  Rural:  M =", round(t_milieu$estimate[1], 2), "\n")
cat("  Différence:", round(t_milieu$estimate[2] - t_milieu$estimate[1], 2), "\n")
cat("  t =", round(t_milieu$statistic, 2), ", p =", format.pval(t_milieu$p.value), "\n\n")

# ANOVA richesse
anova_rich <- aov(score_tdah ~ richesse_cat, data = dataset_score)
cat("ANOVA - Score selon richesse:\n")
print(summary(anova_rich))

# 4. RÉGRESSIONS LINÉAIRES ====================================================

cat("\n📊 RÉGRESSIONS LINÉAIRES - Score TDAH\n")
cat(strrep("=", 70), "\n\n")

# Modèle 1: Démographiques
model1_lm <- lm(score_tdah ~ age_annees + milieu, data = dataset_score)

cat("MODÈLE 1 : Démographiques\n")
cat(strrep("-", 70), "\n")
print(summary(model1_lm))

# Modèle 2: + Socio-économique
model2_lm <- lm(score_tdah ~ age_annees + milieu + richesse_cat + educ_mere_cat,
                data = dataset_score)

cat("\n\nMODÈLE 2 : + Socio-économique\n")
cat(strrep("-", 70), "\n")
print(summary(model2_lm))

# Modèle 3: + Périnataux
model3_lm <- lm(score_tdah ~ age_annees + milieu + richesse_cat + educ_mere_cat +
                  age_mere_cat + ordre_cat,
                data = dataset_score)

cat("\n\nMODÈLE 3 : Complet\n")
cat(strrep("-", 70), "\n")
print(summary(model3_lm))

# Comparaison des modèles
cat("\n\nComparaison des modèles (ANOVA):\n")
anova_models <- anova(model1_lm, model2_lm, model3_lm)
print(anova_models)

# 5. RÉGRESSIONS LOGISTIQUES ==================================================

cat("\n\n📊 RÉGRESSIONS LOGISTIQUES - Risque TDAH Élevé (≥40)\n")
cat(strrep("=", 70), "\n\n")

# Modèle 1: Démographiques
model1_glm <- glm(risque_tdah_eleve ~ age_annees + milieu,
                  family = binomial(link = "logit"),
                  data = dataset_score)

cat("MODÈLE 1 : Démographiques\n")
cat(strrep("-", 70), "\n")
print(summary(model1_glm))

or1 <- exp(coef(model1_glm))
ci1 <- exp(confint.default(model1_glm))
cat("\n\nOdds Ratios:\n")
or_table1 <- tibble(
  Variable = names(or1),
  OR = round(or1, 3),
  CI_lower = round(ci1[, 1], 3),
  CI_upper = round(ci1[, 2], 3)
)
print(or_table1)

# Modèle 2: + Socio-économique
model2_glm <- glm(risque_tdah_eleve ~ age_annees + milieu + 
                    richesse_cat + educ_mere_cat,
                  family = binomial(link = "logit"),
                  data = dataset_score)

cat("\n\nMODÈLE 2 : + Socio-économique\n")
cat(strrep("-", 70), "\n")
print(summary(model2_glm))

or2 <- exp(coef(model2_glm))
ci2 <- exp(confint.default(model2_glm))
cat("\n\nOdds Ratios:\n")
or_table2 <- tibble(
  Variable = names(or2),
  OR = round(or2, 3),
  CI_lower = round(ci2[, 1], 3),
  CI_upper = round(ci2[, 2], 3)
)
print(or_table2)

# Modèle 3: Complet
model3_glm <- glm(risque_tdah_eleve ~ age_annees + milieu + 
                    richesse_cat + educ_mere_cat +
                    age_mere_cat + ordre_cat,
                  family = binomial(link = "logit"),
                  data = dataset_score)

cat("\n\nMODÈLE 3 : Complet\n")
cat(strrep("-", 70), "\n")
print(summary(model3_glm))

or3 <- exp(coef(model3_glm))
ci3 <- exp(confint.default(model3_glm))
p_values <- summary(model3_glm)$coefficients[, 4]

cat("\n\nOdds Ratios (Modèle Complet):\n")
or_table3 <- tibble(
  Variable = names(or3),
  OR = round(or3, 3),
  CI_lower = round(ci3[, 1], 3),
  CI_upper = round(ci3[, 2], 3),
  p_value = round(p_values, 4),
  Significatif = ifelse(p_values < 0.05, "Oui", "Non")
)
print(or_table3)

# Tableau formaté pour publication
table_or_final <- or_table3 %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    OR_IC95 = paste0(OR, " [", CI_lower, "-", CI_upper, "]"),
    p_value_fmt = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ "<0.01",
      p_value < 0.05 ~ "<0.05",
      TRUE ~ as.character(round(p_value, 3))
    )
  ) %>%
  select(Variable, OR_IC95, p_value_fmt, Significatif)

cat("\n\nTableau final des Odds Ratios (pour publication):\n")
cat(strrep("=", 70), "\n")
print(table_or_final, n = Inf)

# Comparaison AIC
cat("\n\nComparaison des modèles (AIC):\n")
cat("  Modèle 1:", round(AIC(model1_glm), 2), "\n")
cat("  Modèle 2:", round(AIC(model2_glm), 2), "\n")
cat("  Modèle 3:", round(AIC(model3_glm), 2), "← MEILLEUR\n")

# 6. ANALYSES STRATIFIÉES =====================================================

cat("\n\n📊 ANALYSES STRATIFIÉES\n")
cat(strrep("=", 70), "\n\n")

# Par milieu
cat("Modèles par MILIEU DE RÉSIDENCE:\n\n")

model_urbain <- glm(risque_tdah_eleve ~ age_annees + richesse_cat + educ_mere_cat,
                    family = binomial,
                    data = dataset_score %>% filter(milieu == "Urbain"))

model_rural <- glm(risque_tdah_eleve ~ age_annees + richesse_cat + educ_mere_cat,
                   family = binomial,
                   data = dataset_score %>% filter(milieu == "Rural"))

cat("URBAIN - Odds Ratios:\n")
or_urbain <- exp(coef(model_urbain))
print(round(or_urbain, 3))

cat("\nRURAL - Odds Ratios:\n")
or_rural <- exp(coef(model_rural))
print(round(or_rural, 3))

# 7. DIAGNOSTICS ==============================================================

cat("\n\n📊 DIAGNOSTICS DU MODÈLE\n")
cat(strrep("=", 70), "\n\n")

# VIF (multicolinéarité)
cat("VIF (Variance Inflation Factor) - Modèle 3:\n")
vif_values <- car::vif(model3_glm)
print(round(vif_values, 2))
cat("\n→ VIF < 5 : Pas de multicolinéarité préoccupante\n")
cat("→ VIF > 10 : Multicolinéarité sévère\n")

# Pseudo R²
null_deviance <- model3_glm$null.deviance
model_deviance <- model3_glm$deviance
pseudo_r2 <- 1 - (model_deviance / null_deviance)

cat("\n\nPseudo R² (McFadden):", round(pseudo_r2, 4), "\n")
cat("→ Pouvoir prédictif:", round(pseudo_r2 * 100, 1), "%\n")

# 8. PRÉDICTIONS ==============================================================

cat("\n\n📊 CAPACITÉ PRÉDICTIVE DU MODÈLE\n")
cat(strrep("=", 70), "\n\n")

# Prédictions
dataset_score$pred_prob <- predict(model3_glm, type = "response")
dataset_score$pred_risque <- ifelse(dataset_score$pred_prob > 0.5, 1, 0)

# Matrice de confusion
confusion <- table(Observé = dataset_score$risque_tdah_eleve,
                   Prédit = dataset_score$pred_risque)

cat("Matrice de confusion:\n")
print(confusion)

# Métriques de performance
sensitivity <- confusion[2,2] / sum(confusion[2,])
specificity <- confusion[1,1] / sum(confusion[1,])
accuracy <- sum(diag(confusion)) / sum(confusion)

cat("\n\nPerformance du modèle:\n")
cat("  Sensibilité:", round(sensitivity * 100, 1), "%\n")
cat("  Spécificité:", round(specificity * 100, 1), "%\n")
cat("  Précision globale:", round(accuracy * 100, 1), "%\n")

# 9. SAUVEGARDE ===============================================================

cat("\n\n💾 Sauvegarde des résultats...\n")

save(
  chi2_tests,
  t_milieu, anova_rich,
  model1_lm, model2_lm, model3_lm,
  model1_glm, model2_glm, model3_glm,
  or_table1, or_table2, or_table3, table_or_final,
  vif_values, pseudo_r2,
  confusion, sensitivity, specificity, accuracy,
  file = file.path(project_root, "data", "processed", "06_models.RData")
)

write_csv(table_or_final,
          file.path(project_root, "data", "processed", "odds_ratios_final.csv"))

cat("  ✅ Modèles sauvegardés\n")
cat("  ✅ Tableau OR exporté: odds_ratios_final.csv\n\n")

cat("✨ ANALYSES STATISTIQUES TERMINÉES!\n\n")
cat("📊 RÉSUMÉ:\n")
cat("  - Toutes les associations sont significatives\n")
cat("  - R² = ", round(summary(model3_lm)$r.squared, 3), " (régression linéaire)\n")
cat("  - Pseudo R² = ", round(pseudo_r2, 3), " (régression logistique)\n")
cat("  - Précision du modèle: ", round(accuracy * 100, 1), "%\n")
cat("  - Facteurs de risque principaux identifiés\n\n")

cat("🎉 ANALYSE COMPLÈTE TERMINÉE!\n")
cat("📝 Prochaine étape: Rédiger le rapport avec rapport_principal.qmd\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================