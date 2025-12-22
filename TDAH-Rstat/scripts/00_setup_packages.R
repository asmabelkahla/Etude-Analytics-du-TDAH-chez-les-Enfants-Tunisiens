# Packages à installer
packages <- c(
  "tidyverse", "here", "haven", "readr", "janitor",
  "gtsummary", "survey", "broom", "patchwork",
  "scales", "kableExtra", "rmarkdown", "quarto"
)

install.packages(packages)

# Initialiser renv pour la reproductibilité
install.packages("renv")
renv::init()