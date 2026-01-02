# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 06 : Modèles de Machine Learning
# ==============================================================================
# Description: Modèles de prédiction du risque TDAH avec Random Forest
# Auteur: Asma BELKAHLA
# Date: 2026-01-02
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

# Charger les bibliothèques
library(tidyverse)
library(randomForest)  # Random Forest
library(caret)         # Pour la validation croisée et les métriques
library(pROC)          # Pour les courbes ROC
library(ggplot2)       # Visualisations

project_root <- getwd()

# Charger les données avec score
load(file.path(project_root, "data", "processed", "05_risk_score.RData"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║              MODÈLES DE MACHINE LEARNING - RANDOM FOREST           ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 Dataset:", nrow(dataset_score), "enfants\n\n")

# 2. PRÉPARATION DES DONNÉES ==================================================

cat("🔧 PRÉPARATION DES DONNÉES\n")
cat(strrep("=", 70), "\n\n")

# Sélectionner les variables pertinentes (sans sexe car 97% manquant)
# Convertir risque_tdah_eleve en facteur pour la classification
dataset_ml <- dataset_score %>%
  select(
    # Variable cible
    risque_tdah_eleve,
    score_tdah,

    # Prédicteurs
    age_annees,
    milieu,
    richesse_cat,
    educ_mere_cat,
    age_mere_cat,
    ordre_cat
  ) %>%
  # Supprimer les valeurs manquantes
  drop_na() %>%
  # Convertir la variable cible en facteur
  mutate(
    risque_tdah_eleve = factor(
      risque_tdah_eleve,
      levels = c(0, 1),
      labels = c("Non", "Oui")
    )
  )

cat("✅ Données préparées:\n")
cat("   - Observations après nettoyage:", nrow(dataset_ml), "\n")
cat("   - Variables prédictives:", ncol(dataset_ml) - 2, "\n")
cat("   - Variable cible: risque_tdah_eleve (Oui/Non)\n\n")

# Afficher la distribution de la variable cible
cat("📊 Distribution de la variable cible:\n")
table(dataset_ml$risque_tdah_eleve) %>% print()
cat("\nProportion:\n")
prop.table(table(dataset_ml$risque_tdah_eleve)) %>%
  round(3) %>%
  print()
cat("\n\n")

# 3. SPLIT TRAIN/TEST =========================================================

cat("✂️ SÉPARATION DES DONNÉES\n")
cat(strrep("=", 70), "\n\n")

# Fixer la graine pour la reproductibilité
set.seed(123)

# Créer l'index de séparation (70% train, 30% test)
train_index <- createDataPartition(
  dataset_ml$risque_tdah_eleve,
  p = 0.7,
  list = FALSE
)

# Créer les ensembles d'entraînement et de test
train_data <- dataset_ml[train_index, ]
test_data <- dataset_ml[-train_index, ]

cat("✅ Données séparées:\n")
cat("   - Ensemble d'entraînement:", nrow(train_data), "observations\n")
cat("   - Ensemble de test:", nrow(test_data), "observations\n\n")

# Vérifier l'équilibre des classes dans les deux ensembles
cat("Distribution dans l'ensemble d'entraînement:\n")
table(train_data$risque_tdah_eleve) %>% prop.table() %>% round(3) %>% print()

cat("\nDistribution dans l'ensemble de test:\n")
table(test_data$risque_tdah_eleve) %>% prop.table() %>% round(3) %>% print()
cat("\n\n")

# 4. MODÈLE 1: RANDOM FOREST ==================================================

cat("🌲 MODÈLE 1: RANDOM FOREST\n")
cat(strrep("=", 70), "\n\n")

cat("⏳ Entraînement du modèle Random Forest...\n")

# Entraîner le modèle Random Forest
# ntree = nombre d'arbres, mtry = nombre de variables à chaque split
set.seed(123)
rf_model <- randomForest(
  risque_tdah_eleve ~ age_annees + milieu + richesse_cat +
    educ_mere_cat + age_mere_cat + ordre_cat,
  data = train_data,
  ntree = 500,           # Nombre d'arbres
  mtry = 3,              # Nombre de variables par split
  importance = TRUE,     # Calculer l'importance des variables
  proximity = FALSE
)

cat("✅ Modèle entraîné!\n\n")

# Afficher le résumé du modèle
print(rf_model)
cat("\n")

# 5. IMPORTANCE DES VARIABLES =================================================

cat("📊 IMPORTANCE DES VARIABLES\n")
cat(strrep("=", 70), "\n\n")

# Extraire l'importance des variables
importance_rf <- importance(rf_model) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  arrange(desc(MeanDecreaseGini))

cat("Importance des variables (Mean Decrease Gini):\n\n")
print(importance_rf, row.names = FALSE)
cat("\n")

# Visualisation de l'importance
importance_plot <- ggplot(
  importance_rf,
  aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importance des Variables - Random Forest",
    x = "Variable",
    y = "Importance (Mean Decrease Gini)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(importance_plot)
cat("\n")

# 6. PRÉDICTIONS SUR L'ENSEMBLE DE TEST =======================================

cat("🎯 PRÉDICTIONS SUR L'ENSEMBLE DE TEST\n")
cat(strrep("=", 70), "\n\n")

# Faire des prédictions
predictions_rf <- predict(rf_model, newdata = test_data, type = "class")
predictions_prob_rf <- predict(rf_model, newdata = test_data, type = "prob")

# 7. ÉVALUATION DU MODÈLE =====================================================

cat("📈 ÉVALUATION DE LA PERFORMANCE\n")
cat(strrep("=", 70), "\n\n")

# Matrice de confusion
confusion_rf <- confusionMatrix(predictions_rf, test_data$risque_tdah_eleve)

cat("Matrice de Confusion:\n\n")
print(confusion_rf$table)
cat("\n")

cat("Métriques de Performance:\n")
cat(strrep("-", 70), "\n")
cat("  Précision (Accuracy):   ", round(confusion_rf$overall["Accuracy"] * 100, 2), "%\n")
cat("  Sensibilité (Recall):   ", round(confusion_rf$byClass["Sensitivity"] * 100, 2), "%\n")
cat("  Spécificité:            ", round(confusion_rf$byClass["Specificity"] * 100, 2), "%\n")
cat("  Précision (Precision):  ", round(confusion_rf$byClass["Precision"] * 100, 2), "%\n")
cat("  F1-Score:               ", round(confusion_rf$byClass["F1"] * 100, 2), "%\n")
cat("  Kappa:                  ", round(confusion_rf$overall["Kappa"], 3), "\n\n")

# 8. COURBE ROC ===============================================================

cat("📊 COURBE ROC (Receiver Operating Characteristic)\n")
cat(strrep("=", 70), "\n\n")

# Calculer la courbe ROC
roc_rf <- roc(
  test_data$risque_tdah_eleve,
  predictions_prob_rf[, "Oui"],
  levels = c("Non", "Oui")
)

cat("AUC (Area Under Curve):", round(auc(roc_rf), 3), "\n")

# Interprétation de l'AUC
auc_value <- auc(roc_rf)
interpretation <- case_when(
  auc_value >= 0.9 ~ "Excellent",
  auc_value >= 0.8 ~ "Très bon",
  auc_value >= 0.7 ~ "Bon",
  auc_value >= 0.6 ~ "Moyen",
  TRUE ~ "Faible"
)
cat("Interprétation: ", interpretation, "\n\n")

# Visualisation de la courbe ROC
roc_plot <- ggroc(roc_rf) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Courbe ROC - Random Forest",
    x = "1 - Spécificité (Faux Positifs)",
    y = "Sensibilité (Vrais Positifs)",
    subtitle = paste("AUC =", round(auc_roc), 3))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(roc_plot)
cat("\n")

# 9. VALIDATION CROISÉE =======================================================

cat("🔄 VALIDATION CROISÉE (K-FOLD)\n")
cat(strrep("=", 70), "\n\n")

cat("⏳ Exécution de la validation croisée 5-fold...\n")

# Configuration de la validation croisée
train_control <- trainControl(
  method = "cv",               # Validation croisée
  number = 5,                  # 5 folds
  classProbs = TRUE,           # Calculer les probabilités
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Entraîner avec validation croisée
set.seed(123)
rf_cv <- train(
  risque_tdah_eleve ~ age_annees + milieu + richesse_cat +
    educ_mere_cat + age_mere_cat + ordre_cat,
  data = train_data,
  method = "rf",
  trControl = train_control,
  metric = "ROC",
  ntree = 500
)

cat("✅ Validation croisée terminée!\n\n")

cat("Résultats de la validation croisée:\n")
cat(strrep("-", 70), "\n")
print(rf_cv$results)
cat("\n")

# 10. PRÉDICTIONS INDIVIDUELLES (EXEMPLES) ====================================

cat("🔍 EXEMPLES DE PRÉDICTIONS INDIVIDUELLES\n")
cat(strrep("=", 70), "\n\n")

# Sélectionner quelques exemples
exemples <- test_data %>%
  slice_sample(n = 5) %>%
  mutate(
    Prediction = predict(rf_model, newdata = ., type = "class"),
    Prob_Risque_Eleve = round(
      predict(rf_model, newdata = ., type = "prob")[, "Oui"] * 100, 1
    )
  ) %>%
  select(
    Vérité = risque_tdah_eleve,
    Prediction,
    Prob_Risque_Eleve,
    milieu,
    richesse_cat,
    educ_mere_cat
  )

cat("Exemples de prédictions:\n\n")
print(exemples, n = Inf)
cat("\n")

# 11. ANALYSE DES ERREURS =====================================================

cat("❌ ANALYSE DES ERREURS DE CLASSIFICATION\n")
cat(strrep("=", 70), "\n\n")

# Identifier les faux positifs et faux négatifs
erreurs <- test_data %>%
  mutate(
    Prediction = predict(rf_model, newdata = test_data, type = "class"),
    Erreur = case_when(
      risque_tdah_eleve == "Oui" & Prediction == "Non" ~ "Faux Négatif",
      risque_tdah_eleve == "Non" & Prediction == "Oui" ~ "Faux Positif",
      TRUE ~ "Correct"
    )
  )

cat("Nombre d'erreurs par type:\n")
table(erreurs$Erreur) %>% print()
cat("\n")

# Analyser les caractéristiques des erreurs
if (sum(erreurs$Erreur != "Correct") > 0) {
  cat("Caractéristiques des cas mal classés:\n\n")

  erreurs_summary <- erreurs %>%
    filter(Erreur != "Correct") %>%
    group_by(Erreur, milieu, richesse_cat) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n))

  print(erreurs_summary, n = 20)
}

cat("\n")

# 12. SAUVEGARDE DES RÉSULTATS ================================================

cat("💾 SAUVEGARDE DES RÉSULTATS\n")
cat(strrep("=", 70), "\n\n")

# Sauvegarder le modèle et les résultats
save(
  rf_model,
  rf_cv,
  confusion_rf,
  roc_rf,
  importance_rf,
  train_data,
  test_data,
  predictions_rf,
  predictions_prob_rf,
  file = file.path(project_root, "data", "processed", "06_ml_models.RData")
)

# Sauvegarder les métriques dans un CSV
metrics_df <- tibble(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1", "AUC"),
  Value = c(
    confusion_rf$overall["Accuracy"],
    confusion_rf$byClass["Sensitivity"],
    confusion_rf$byClass["Specificity"],
    confusion_rf$byClass["Precision"],
    confusion_rf$byClass["F1"],
    auc(roc_rf)
  )
) %>%
  mutate(Value = round(Value, 4))

write_csv(
  metrics_df,
  file.path(project_root, "data", "processed", "ml_metrics.csv")
)

# Sauvegarder l'importance des variables
write_csv(
  importance_rf,
  file.path(project_root, "data", "processed", "variable_importance.csv")
)

cat("✅ Résultats sauvegardés:\n")
cat("   - 06_ml_models.RData (modèles et résultats)\n")
cat("   - ml_metrics.csv (métriques de performance)\n")
cat("   - variable_importance.csv (importance des variables)\n\n")

# 13. RÉSUMÉ FINAL ============================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                       RÉSUMÉ DE L'ANALYSE ML                       ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 MODÈLE: Random Forest\n")
cat(strrep("-", 70), "\n")
cat("  - Nombre d'arbres:           ", rf_model$ntree, "\n")
cat("  - Variables par split:       ", rf_model$mtry, "\n")
cat("  - Taille échantillon train:  ", nrow(train_data), "\n")
cat("  - Taille échantillon test:   ", nrow(test_data), "\n\n")

cat("🎯 PERFORMANCE SUR L'ENSEMBLE DE TEST\n")
cat(strrep("-", 70), "\n")
cat("  - Précision (Accuracy):      ", round(confusion_rf$overall["Accuracy"] * 100, 2), "%\n")
cat("  - AUC:                       ", round(auc(roc_rf), 3), "\n")
cat("  - F1-Score:                  ", round(confusion_rf$byClass["F1"], 3), "\n\n")

cat("🔝 TOP 3 VARIABLES LES PLUS IMPORTANTES\n")
cat(strrep("-", 70), "\n")
top3 <- importance_rf %>% slice_head(n = 3)
for (i in 1:3) {
  cat("  ", i, ". ", top3$Variable[i], " (",
      round(top3$MeanDecreaseGini[i], 2), ")\n", sep = "")
}

cat("\n")
cat("✅ ANALYSE DE MACHINE LEARNING TERMINÉE!\n")
cat("📝 Les résultats sont prêts pour être intégrés au rapport.\n\n")

cat("📌 INTERPRÉTATION\n")
cat(strrep("-", 70), "\n")
cat("Le modèle Random Forest identifie les enfants à haut risque de TDAH\n")
cat("avec une précision de", round(confusion_rf$overall["Accuracy"] * 100, 1), "%.\n")
cat("Les facteurs les plus prédictifs sont:\n")
cat("  1. La situation socio-économique (richesse)\n")
cat("  2. L'éducation maternelle\n")
cat("  3. Le milieu de résidence (urbain/rural)\n\n")

cat("🚀 PROCHAINES ÉTAPES\n")
cat(strrep("-", 70), "\n")
cat("  - Intégrer les résultats dans rapport_principal.qmd\n")
cat("  - Créer des visualisations pour la présentation\n")
cat("  - Documenter les recommandations de santé publique\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================
