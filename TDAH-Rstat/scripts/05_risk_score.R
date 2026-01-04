# PROJET TDAH TUNISIE - MICS6 2023
# Script 05 : Construction du score de risque TDAH
# Auteur: Asma BELKAHLA

rm(list = ls())
gc()

library(tidyverse)
library(psych)
library(corrplot)
library(ggridges)
library(patchwork)

project_root <- getwd()
load(file.path(project_root, "data", "processed", "03_features.RData"))

cat("Construction du score de risque TDAH\n")

poids <- list(
  age_mere_risque = 0.15,      # Thapar et al., 2013
  ordre_risque = 0.10,          # Russell et al., 2016
  intervalle_risque = 0.15,     # Cheslack-Postava et al., 2011
  richesse_risque = 0.20,       # Russell et al., 2014
  educ_mere_risque = 0.15,      # Rowland et al., 2018
  taille_menage_risque = 0.10,  # Larsson et al., 2014
  sexe_risque = 0.15            # Willcutt, 2012
)

facteurs_info <- tibble(
  Facteur = names(poids),
  Poids = unlist(poids),
  Reference = c(
    "Thapar et al., 2013",
    "Russell et al., 2016",
    "Cheslack-Postava et al., 2011",
    "Russell et al., 2014",
    "Rowland et al., 2018",
    "Larsson et al., 2014",
    "Willcutt, 2012"
  ),
  Description = c(
    "Âge maternel extrême (<20 ou ≥35 ans)",
    "Rang de naissance élevé (≥4)",
    "Espacement court (<24 mois)",
    "Pauvreté (Q1-Q2)",
    "Faible éducation maternelle",
    "Grande taille ménage (>7 personnes)",
    "Sexe masculin"
  )
)


dataset_score <- dataset_features %>%
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
    
    # Score simple (nombre de facteurs)
    score_simple = age_mere_risque + ordre_risque + intervalle_risque +
                   richesse_risque + educ_mere_risque + 
                   taille_menage_risque + sexe_risque,
    
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
distrib_cat <- dataset_score %>%
  count(risque_tdah_cat) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    pct_cum = cumsum(pct)
  )
p1 <- ggplot(dataset_score, aes(x = score_tdah)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "darkred", size = 1.5) +
  geom_vline(xintercept = c(20, 40), linetype = "dashed", 
             color = "darkred", size = 1) +
  annotate("text", x = 10, y = 0.035, label = "Faible", 
           color = "darkgreen", size = 5, fontface = "bold") +
  annotate("text", x = 30, y = 0.035, label = "Moyen", 
           color = "orange", size = 5, fontface = "bold") +
  annotate("text", x = 55, y = 0.035, label = "Élevé", 
           color = "red", size = 5, fontface = "bold") +
  labs(
    title = "Distribution du Score de Risque Théorique TDAH",
    subtitle = paste0("N = ", nrow(dataset_score), " enfants | Moyenne = ",
                     round(mean(dataset_score$score_tdah), 1), " | SD = ",
                     round(sd(dataset_score$score_tdah), 1)),
    x = "Score de risque TDAH (0-100)",
    y = "Densité"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(project_root, "reports", "figures", "05_distribution_score.png"),
       p1, width = 12, height = 8, dpi = 300)

# 5.2 Répartition par catégories
p2 <- dataset_score %>%
  count(risque_tdah_cat) %>%
  mutate(
    pct = n / sum(n) * 100,
    label = paste0(n, "\n(", round(pct, 1), "%)")
  ) %>%
  ggplot(aes(x = risque_tdah_cat, y = n, fill = risque_tdah_cat)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_text(aes(label = label), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Faible" = "#2ecc71", 
                                "Moyen" = "#f39c12", 
                                "Élevé" = "#e74c3c")) +
  labs(
    title = "Répartition des Enfants par Catégorie de Risque TDAH",
    x = "Catégorie de risque",
    y = "Nombre d'enfants"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(file.path(project_root, "reports", "figures", "06_categories_risque.png"),
       p2, width = 12, height = 8, dpi = 300)

# 5.3 Score par sexe (seulement si sexe non NA)
if(sum(!is.na(dataset_score$sexe)) > 0) {
  p3 <- ggplot(dataset_score %>% filter(!is.na(sexe)), 
               aes(x = score_tdah, y = sexe, fill = sexe)) +
    geom_density_ridges(alpha = 0.75, scale = 1.5, rel_min_height = 0.01) +
    geom_vline(xintercept = c(20, 40), linetype = "dashed", alpha = 0.6) +
    scale_fill_manual(values = c("Masculin" = "#3498db", "Féminin" = "#e91e63")) +
    labs(
      title = "Distribution du Score TDAH selon le Sexe",
      subtitle = paste0("Garçons: M=", 
                       round(mean(dataset_score$score_tdah[dataset_score$sexe=="Masculin"], na.rm=TRUE), 1),
                       " | Filles: M=",
                       round(mean(dataset_score$score_tdah[dataset_score$sexe=="Féminin"], na.rm=TRUE), 1)),
      x = "Score de risque TDAH (0-100)",
      y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 15)
    )
  
  ggsave(file.path(project_root, "reports", "figures", "07_score_par_sexe.png"),
         p3, width = 12, height = 8, dpi = 300)
}

# 5.4 Score par quintile de richesse
p4 <- ggplot(dataset_score, aes(x = richesse_cat, y = score_tdah, fill = richesse_cat)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "red", color = "darkred") +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "Score TDAH selon le Quintile de Richesse",
    subtitle = "Diamant rouge = moyenne",
    x = "Quintile de richesse",
    y = "Score de risque TDAH (0-100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

ggsave(file.path(project_root, "reports", "figures", "08_score_par_richesse.png"),
       p4, width = 12, height = 8, dpi = 300)

# 5.5 Score par milieu et richesse (heatmap)
p5 <- dataset_score %>%
  group_by(milieu, richesse_cat) %>%
  summarise(score_moyen = mean(score_tdah, na.rm = TRUE), 
            n = n(), .groups = "drop") %>%
  ggplot(aes(x = richesse_cat, y = milieu, fill = score_moyen)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = paste0(round(score_moyen, 1), "\n(n=", n, ")")), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient2(low = "#2ecc71", mid = "#f39c12", high = "#e74c3c",
                       midpoint = 30, name = "Score\nmoyen") +
  labs(
    title = "Score TDAH Moyen selon Milieu et Richesse",
    x = "Quintile de richesse",
    y = "Milieu de résidence"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 25, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(file.path(project_root, "reports", "figures", "09_heatmap_score.png"),
       p5, width = 12, height = 8, dpi = 300)

cor_matrix <- dataset_score %>%
  select(age_mere_risque, ordre_risque, intervalle_risque,
         richesse_risque, educ_mere_risque, milieu_risque,
         taille_menage_risque, sexe_risque) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

png(file.path(project_root, "reports", "figures", "10_correlation_facteurs.png"),
    width = 1000, height = 1000, res = 120)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 1.2,
         number.cex = 1,
         col = colorRampPalette(c("#2ecc71", "white", "#e74c3c"))(200),
         title = "Corrélations entre Facteurs de Risque TDAH",
         mar = c(0,0,2,0))
dev.off()
alpha_result <- NULL
tryCatch({
  alpha_result <- psych::alpha(dataset_score %>%
                                 select(age_mere_risque:sexe_risque) %>%
                                 select_if(~sd(., na.rm = TRUE) > 0),
                               check.keys = TRUE)
}, error = function(e) {})
scores_groupes <- dataset_score %>%
  group_by(milieu) %>%
  summarise(
    n = n(),
    score_moyen = round(mean(score_tdah, na.rm = TRUE), 1),
    score_sd = round(sd(score_tdah, na.rm = TRUE), 1),
    pct_risque_eleve = round(mean(risque_tdah_eleve, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

haut_risque <- dataset_score %>%
  filter(risque_tdah_eleve == 1)
resume_score <- tibble(
  Statistique = c("Minimum", "1er quartile", "Médiane", "Moyenne", 
                  "3e quartile", "Maximum", "Écart-type"),
  Valeur = c(
    min(dataset_score$score_tdah, na.rm = TRUE),
    quantile(dataset_score$score_tdah, 0.25, na.rm = TRUE),
    median(dataset_score$score_tdah, na.rm = TRUE),
    mean(dataset_score$score_tdah, na.rm = TRUE),
    quantile(dataset_score$score_tdah, 0.75, na.rm = TRUE),
    max(dataset_score$score_tdah, na.rm = TRUE),
    sd(dataset_score$score_tdah, na.rm = TRUE)
  )
) %>%
  mutate(Valeur = round(Valeur, 2))
saveRDS(dataset_score,
        file.path(project_root, "data", "processed", "dataset_with_score.rds"))

write_csv(resume_score,
          file.path(project_root, "data", "processed", "resume_score_tdah.csv"))
save(
  dataset_score,
  facteurs_info,
  poids,
  cor_matrix,
  alpha_result,
  resume_score,
  scores_groupes,
  haut_risque,
  distrib_cat,
  file = file.path(project_root, "data", "processed", "05_risk_score.RData")
)

cat("Score de risque calculé:", nrow(dataset_score), "enfants\n")