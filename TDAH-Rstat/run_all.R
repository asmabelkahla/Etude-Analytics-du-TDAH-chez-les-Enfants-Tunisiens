# PROJET TDAH TUNISIE - MICS6 2023
# Script Maitre : Exécution complète du Pipeline
# Auteur: Asma BELKAHLA
# ==============================================================================

cat("🚀 Lancement du pipeline complet de traitement des données TDAH...\n\n")

# Chemins des scripts
scripts <- c(
    "scripts/01_data_import.R",
    "scripts/02_data_cleaning_SPSS.R",
    "scripts/03_feature_engineering_SPSS.R",
    "scripts/04_descriptive_analysis.R",
    "scripts/05_risk_score_calculation.R",
    "scripts/06_ml_models.R"
)

# Exécution séquentielle
for (script in scripts) {
    if (file.exists(script)) {
        cat("\n------------------------------------------------------------\n")
        cat("▶️ Exécution de :", script, "\n")
        cat("------------------------------------------------------------\n")
        source(script, encoding = "UTF-8")
        cat("\n✅ Terminé :", script, "\n")
    } else {
        warning("⚠️ Le script ", script, " est introuvable.")
    }
}

cat("\n============================================================\n")
cat("✨ PIPELINE TERMINÉ AVEC SUCCÈS !\n")
cat("📊 Toutes les données sont à jour dans data/processed/\n")
cat("🚀 Vous pouvez maintenant lancer le rendu du site Quarto.\n")
cat("============================================================\n")
