# ==============================================================================
# Script de réparation des packages
# ==============================================================================

# Étape 1 : Supprimer les dossiers de verrouillage
cat("🔧 Nettoyage des verrous de packages...\n")
lock_dir <- "C:/Users/GIGABYTE/AppData/Local/R/win-library/4.5/00LOCK"
if (dir.exists(lock_dir)) {
  unlink(lock_dir, recursive = TRUE)
  cat("  ✅ Dossier 00LOCK supprimé\n")
} else {
  cat("  ✅ Pas de dossier 00LOCK\n")
}

# Étape 2 : Redémarrer la session R
cat("\n⚠️  FERMEZ complètement RStudio maintenant!\n")
cat("Puis rouvrez-le et exécutez ces commandes:\n\n")
cat("install.packages(c('xfun', 'litedown'), type = 'binary')\n")
cat("library(xfun)\n")
cat("library(litedown)\n")
cat("\n")
