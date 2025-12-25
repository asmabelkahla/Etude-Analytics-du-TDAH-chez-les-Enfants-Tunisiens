# ==============================================================================
# PROJET TDAH TUNISIE - MICS6 2023
# Script 04 : Analyses descriptives complètes (VERSION CORRIGÉE)
# ==============================================================================
# Description: Statistiques descriptives avec correction de l'erreur pivot_longer
# Auteur: Asma BELKAHLA
# Date: 2025-12-24
# ==============================================================================

# 1. CONFIGURATION ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(scales)
library(patchwork)

project_root <- getwd()

# Charger les données
load(file.path(project_root, "data", "processed", "03_features.RData"))

# Créer dossier figures
dir.create(file.path(project_root, "reports", "figures"), 
           showWarnings = FALSE, recursive = TRUE)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║              ANALYSES DESCRIPTIVES COMPLÈTES                       ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# 2. TABLEAU 1 : CARACTÉRISTIQUES GÉNÉRALES ===================================

cat("📊 Tableau 1 : Caractéristiques générales de l'échantillon\n")
cat(strrep("=", 70), "\n\n")

# Statistiques démographiques
cat("CARACTÉRISTIQUES DÉMOGRAPHIQUES\n\n")

cat("Sexe:\n")
print(table(dataset_features$sexe))
print(prop.table(table(dataset_features$sexe)) * 100)

cat("\nÂge de l'enfant:\n")
cat("  Moyenne:", round(mean(dataset_features$age_annees, na.rm = TRUE), 1), "ans\n")
cat("  Médiane:", round(median(dataset_features$age_annees, na.rm = TRUE), 1), "ans\n")
cat("  Min-Max:", round(min(dataset_features$age_annees, na.rm = TRUE), 1), "-",
    round(max(dataset_features$age_annees, na.rm = TRUE), 1), "ans\n")

cat("\nGroupe d'âge:\n")
print(table(dataset_features$age_groupe))

cat("\n\nCARACTÉRISTIQUES MATERNELLES\n\n")

cat("Âge maternel à la naissance:\n")
print(table(dataset_features$age_mere_cat))
print(prop.table(table(dataset_features$age_mere_cat)) * 100)

cat("\nÉducation maternelle:\n")
print(table(dataset_features$educ_mere_cat))
print(prop.table(table(dataset_features$educ_mere_cat)) * 100)

cat("\nOrdre de naissance:\n")
print(table(dataset_features$ordre_cat))

cat("\n\nCARACTÉRISTIQUES SOCIO-ÉCONOMIQUES\n\n")

cat("Quintile de richesse:\n")
print(table(dataset_features$richesse_cat))
print(prop.table(table(dataset_features$richesse_cat)) * 100)

cat("\nMilieu de résidence:\n")
print(table(dataset_features$milieu))
print(prop.table(table(dataset_features$milieu)) * 100)

cat("\nTaille du ménage:\n")
cat("  Moyenne:", round(mean(dataset_features$taille_menage, na.rm = TRUE), 1), "personnes\n")
cat("  Médiane:", median(dataset_features$taille_menage, na.rm = TRUE), "personnes\n")

# Sauvegarder en CSV
table1_data <- dataset_features %>%
  summarise(
    N = n(),
    Age_moyen = round(mean(age_annees, na.rm = TRUE), 1),
    Pct_urbain = round(mean(milieu == "Urbain", na.rm = TRUE) * 100, 1),
    Pct_pauvres = round(mean(richesse_risque, na.rm = TRUE) * 100, 1),
    Taille_menage_moy = round(mean(taille_menage, na.rm = TRUE), 1)
  )

write_csv(table1_data, 
          file.path(project_root, "reports", "figures", "table1_resume.csv"))

cat("\n✅ Tableau 1 résumé sauvegardé en CSV\n\n")

# 3. TABLEAU 2 : FACTEURS DE RISQUE PAR SEXE =================================

cat("📊 Tableau 2 : Facteurs de risque selon le sexe\n")
cat(strrep("=", 70), "\n\n")

facteurs_risque <- c("age_mere_risque", "ordre_risque", "intervalle_risque",
                     "richesse_risque", "educ_mere_risque", "milieu_risque",
                     "taille_menage_risque")

table2_data <- dataset_features %>%
  group_by(sexe) %>%
  summarise(
    N = n(),
    Age_mere_risque = round(mean(age_mere_risque, na.rm = TRUE) * 100, 1),
    Ordre_risque = round(mean(ordre_risque, na.rm = TRUE) * 100, 1),
    Intervalle_risque = round(mean(intervalle_risque, na.rm = TRUE) * 100, 1),
    Pauvrete = round(mean(richesse_risque, na.rm = TRUE) * 100, 1),
    Faible_educ = round(mean(educ_mere_risque, na.rm = TRUE) * 100, 1),
    Milieu_rural = round(mean(milieu_risque, na.rm = TRUE) * 100, 1),
    Grande_taille = round(mean(taille_menage_risque, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

print(table2_data)

write_csv(table2_data, 
          file.path(project_root, "reports", "figures", "table2_risques_sexe.csv"))

# Tests statistiques
cat("\n📊 Tests du Chi² pour association avec le sexe:\n\n")
for (var in facteurs_risque) {
  test <- chisq.test(table(dataset_features$sexe, dataset_features[[var]]))
  cat(sprintf("%-25s: Chi² = %6.2f, p = %.4f %s\n", 
              var, test$statistic, test$p.value,
              ifelse(test$p.value < 0.05, "***", "")))
}

cat("\n✅ Tableau 2 sauvegardé en CSV\n\n")

# 4. TABLEAU 3 : FACTEURS DE RISQUE PAR MILIEU ===============================

cat("📊 Tableau 3 : Facteurs de risque selon le milieu\n")
cat(strrep("=", 70), "\n\n")

table3_data <- dataset_features %>%
  group_by(milieu) %>%
  summarise(
    N = n(),
    Age_mere_risque = round(mean(age_mere_risque, na.rm = TRUE) * 100, 1),
    Ordre_risque = round(mean(ordre_risque, na.rm = TRUE) * 100, 1),
    Intervalle_risque = round(mean(intervalle_risque, na.rm = TRUE) * 100, 1),
    Pauvrete = round(mean(richesse_risque, na.rm = TRUE) * 100, 1),
    Faible_educ = round(mean(educ_mere_risque, na.rm = TRUE) * 100, 1),
    Grande_taille = round(mean(taille_menage_risque, na.rm = TRUE) * 100, 1),
    Sexe_masculin = round(mean(sexe_risque, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

print(table3_data)

write_csv(table3_data, 
          file.path(project_root, "reports", "figures", "table3_risques_milieu.csv"))

cat("\n✅ Tableau 3 sauvegardé en CSV\n\n")

# 5. GRAPHIQUES : DISTRIBUTION DES FACTEURS DE RISQUE ========================

cat("📊 Création des graphiques...\n")

# 5.1 Distribution des facteurs de risque
p1 <- dataset_features %>%
  select(age_mere_risque, ordre_risque, intervalle_risque,
         richesse_risque, educ_mere_risque, milieu_risque,
         taille_menage_risque, sexe_risque) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Facteur", values_to = "N") %>%
  mutate(
    Pct = N / nrow(dataset_features) * 100,
    Facteur = factor(Facteur, 
                     levels = c("sexe_risque", "richesse_risque", "educ_mere_risque",
                               "milieu_risque", "age_mere_risque", "ordre_risque",
                               "intervalle_risque", "taille_menage_risque"),
                     labels = c("Sexe masculin", "Pauvreté", "Faible éduc. mère",
                               "Milieu rural", "Âge mère à risque", "Rang ≥4",
                               "Espacement <24m", "Ménage >7"))
  ) %>%
  ggplot(aes(x = reorder(Facteur, Pct), y = Pct)) +
  geom_col(aes(fill = Pct), alpha = 0.8) +
  geom_text(aes(label = paste0(round(Pct, 1), "%\n(n=", N, ")")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "#ffa07a", high = "#dc143c") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), 
                     labels = percent_format(scale = 1)) +
  labs(
    title = "Prévalence des Facteurs de Risque TDAH",
    subtitle = paste0("N = ", nrow(dataset_features), " enfants"),
    x = NULL,
    y = "Pourcentage d'enfants exposés"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(project_root, "reports", "figures", "01_prevalence_facteurs.png"),
       p1, width = 12, height = 8, dpi = 300)

# 5.2 Cumul de facteurs de risque
p2 <- dataset_features %>%
  mutate(
    nb_risques_total = nb_risques_perinataux + nb_risques_socioeco + sexe_risque
  ) %>%
  count(nb_risques_total) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = nb_risques_total, y = n)) +
  geom_col(aes(fill = nb_risques_total), alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(n, "\n(", round(pct, 1), "%)")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#90ee90", high = "#ff4500") +
  scale_x_continuous(breaks = 0:8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribution du Nombre de Facteurs de Risque Cumulés",
    subtitle = "Risques périnataux + socio-économiques + sexe masculin",
    x = "Nombre de facteurs de risque",
    y = "Nombre d'enfants"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave(file.path(project_root, "reports", "figures", "02_cumul_facteurs.png"),
       p2, width = 12, height = 8, dpi = 300)

# 5.3 Facteurs de risque par sexe - CORRIGÉ
p3 <- dataset_features %>%
  # Sélectionner UNIQUEMENT les variables de risque binaires
  select(sexe, age_mere_risque, ordre_risque, intervalle_risque,
         richesse_risque, educ_mere_risque, milieu_risque,
         taille_menage_risque) %>%
  pivot_longer(cols = -sexe, names_to = "Facteur", values_to = "Risque") %>%
  filter(!is.na(sexe)) %>%
  group_by(sexe, Facteur) %>%
  summarise(pct = mean(Risque, na.rm = TRUE) * 100, .groups = "drop") %>%
  mutate(
    Facteur = factor(Facteur,
                     levels = c("age_mere_risque", "ordre_risque", "intervalle_risque",
                               "richesse_risque", "educ_mere_risque", "milieu_risque",
                               "taille_menage_risque"),
                     labels = c("Âge mère", "Rang ≥4", "Espacement <24m",
                               "Pauvreté", "Faible éduc.", "Rural", "Ménage >7"))
  ) %>%
  ggplot(aes(x = Facteur, y = pct, fill = sexe)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Masculin" = "#4169e1", "Féminin" = "#ff69b4")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = percent_format(scale = 1)) +
  labs(
    title = "Facteurs de Risque selon le Sexe de l'Enfant",
    x = NULL,
    y = "Pourcentage",
    fill = "Sexe"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top"
  )

ggsave(file.path(project_root, "reports", "figures", "03_risques_par_sexe.png"),
       p3, width = 12, height = 8, dpi = 300)

# 5.4 Facteurs par quintile de richesse
p4 <- dataset_features %>%
  filter(!is.na(richesse_cat)) %>%
  select(richesse_cat, age_mere_risque, ordre_risque, intervalle_risque,
         educ_mere_risque, taille_menage_risque) %>%
  pivot_longer(cols = -richesse_cat, names_to = "Facteur", values_to = "Risque") %>%
  group_by(richesse_cat, Facteur) %>%
  summarise(pct = mean(Risque, na.rm = TRUE) * 100, .groups = "drop") %>%
  mutate(
    Facteur = factor(Facteur,
                     labels = c("Âge mère", "Faible éduc.", "Espacement",
                               "Rang ≥4", "Ménage >7"))
  ) %>%
  ggplot(aes(x = richesse_cat, y = pct, color = Facteur, group = Facteur)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Gradient des Facteurs de Risque selon la Richesse",
    x = "Quintile de richesse",
    y = "Pourcentage",
    color = "Facteur"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "right"
  )

ggsave(file.path(project_root, "reports", "figures", "04_gradient_richesse.png"),
       p4, width = 12, height = 8, dpi = 300)

cat("  ✅ 4 graphiques sauvegardés\n\n")

# 6. STATISTIQUES RÉCAPITULATIVES ============================================

cat("📈 Statistiques récapitulatives:\n")
cat(strrep("=", 70), "\n\n")

resume <- list(
  n_total = nrow(dataset_features),
  n_garcons = sum(dataset_features$sexe == "Masculin", na.rm = TRUE),
  n_filles = sum(dataset_features$sexe == "Féminin", na.rm = TRUE),
  age_moyen = mean(dataset_features$age_annees, na.rm = TRUE),
  pct_urbain = mean(dataset_features$milieu == "Urbain", na.rm = TRUE) * 100,
  pct_pauvres = mean(dataset_features$richesse_risque, na.rm = TRUE) * 100,
  nb_risques_moyen = mean(dataset_features$nb_risques_perinataux + 
                          dataset_features$nb_risques_socioeco, na.rm = TRUE)
)

cat("Échantillon total:", resume$n_total, "enfants\n")
cat("  - Garçons:", resume$n_garcons, "(", 
    round(resume$n_garcons/resume$n_total*100, 1), "%)\n")
cat("  - Filles:", resume$n_filles, "(", 
    round(resume$n_filles/resume$n_total*100, 1), "%)\n")
cat("Âge moyen:", round(resume$age_moyen, 1), "ans\n")
cat("Milieu urbain:", round(resume$pct_urbain, 1), "%\n")
cat("Pauvreté (Q1-Q2):", round(resume$pct_pauvres, 1), "%\n")
cat("Nombre moyen de facteurs de risque:", round(resume$nb_risques_moyen, 2), "\n")

# 7. SAUVEGARDE ===============================================================

cat("\n💾 Sauvegarde des résultats...\n")

save(
  table1_data, table2_data, table3_data,
  resume,
  file = file.path(project_root, "data", "processed", "04_descriptive.RData")
)

cat("  ✅ Tableaux CSV et analyses sauvegardés\n\n")

cat("✨ Analyses descriptives terminées!\n")
cat("📊 Fichiers générés:\n")
cat("  - 3 tableaux CSV (dans reports/figures/)\n")
cat("  - 4 graphiques PNG\n")
cat("  - 04_descriptive.RData créé avec succès ✅\n")
cat("🚀 Prochaine étape: 05_risk_score.R\n\n")

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================
