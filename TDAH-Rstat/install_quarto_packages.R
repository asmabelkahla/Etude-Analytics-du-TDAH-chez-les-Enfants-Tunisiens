# Installation des packages pour Quarto
# Fermer toutes les sessions R ouvertes avant d'exécuter ce script

cat("📦 Installation des packages nécessaires pour Quarto...\n\n")

# Liste complète des packages nécessaires
packages_needed <- c(
  "knitr",
  "rmarkdown",
  "tidyverse",
  "gtsummary",
  "kableExtra",
  "scales",
  "patchwork",
  "here"
)

# Installer les packages manquants
for (pkg in packages_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("📥 Installation de", pkg, "...\n")
    install.packages(pkg,
                     repos = "https://cloud.r-project.org/",
                     dependencies = TRUE,
                     type = "binary")
  } else {
    cat("✅", pkg, "déjà installé\n")
  }
}

cat("\n✅ Vérification finale...\n")
for (pkg in packages_needed) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✅", pkg, "version:", as.character(packageVersion(pkg)), "\n")
  } else {
    cat("  ❌", pkg, "NON installé\n")
  }
}

cat("\n✨ Installation terminée!\n")
