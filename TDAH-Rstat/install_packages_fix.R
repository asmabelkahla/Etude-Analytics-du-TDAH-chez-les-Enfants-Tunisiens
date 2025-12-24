# ==============================================================================
# Installation forcée des packages
# ==============================================================================

cat("🔧 Étape 1 : Nettoyage complet...\n")

# Supprimer tous les verrous
lock_dirs <- c(
  "C:/Users/GIGABYTE/AppData/Local/R/win-library/4.5/00LOCK",
  "C:/Users/GIGABYTE/AppData/Local/R/win-library/4.5/00LOCK-xfun",
  "C:/Users/GIGABYTE/AppData/Local/R/win-library/4.5/00LOCK-litedown"
)

for (lock_dir in lock_dirs) {
  if (dir.exists(lock_dir)) {
    unlink(lock_dir, recursive = TRUE, force = TRUE)
    cat("  ✅ Supprimé:", lock_dir, "\n")
  }
}

# Supprimer les anciennes versions
cat("\n🗑️  Étape 2 : Suppression des anciennes versions...\n")
try(remove.packages("xfun"), silent = TRUE)
try(remove.packages("litedown"), silent = TRUE)

cat("\n📥 Étape 3 : Installation des packages...\n")

# Installer xfun en premier
install.packages("xfun",
                 repos = "https://cloud.r-project.org/",
                 type = "binary",
                 dependencies = TRUE,
                 INSTALL_opts = '--no-lock')

cat("\n📥 Installation de litedown...\n")
install.packages("litedown",
                 repos = "https://cloud.r-project.org/",
                 type = "binary",
                 dependencies = TRUE,
                 INSTALL_opts = '--no-lock')

cat("\n✅ Vérification...\n")
if (requireNamespace("xfun", quietly = TRUE)) {
  cat("  ✅ xfun version:", as.character(packageVersion("xfun")), "\n")
} else {
  cat("  ❌ xfun non installé\n")
}

if (requireNamespace("litedown", quietly = TRUE)) {
  cat("  ✅ litedown version:", as.character(packageVersion("litedown")), "\n")
} else {
  cat("  ❌ litedown non installé\n")
}

cat("\n✨ Terminé!\n")
