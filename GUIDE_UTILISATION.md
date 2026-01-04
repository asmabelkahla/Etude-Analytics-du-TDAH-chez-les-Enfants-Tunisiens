# 📘 Guide d'Utilisation - Projet TDAH Tunisie

## 🎯 Vue d'Ensemble du Projet

Ce projet analyse les facteurs de risque du TDAH chez les enfants tunisiens en utilisant les données MICS6 (2023). L'analyse combine des **méthodes statistiques classiques** et du **Machine Learning**.

---

## 📁 Structure Simplifiée du Projet

```
TDAH-Rstat/
│
├── data/
│   ├── raw/                      # Données brutes MICS6 (.sav)
│   └── processed/                # Données nettoyées et résultats
│
├── scripts/                      # Scripts R à exécuter dans l'ordre
│   ├── 01_data_import.R         # Import des données SPSS
│   ├── 02_data_cleaning.R       # Nettoyage des données
│   ├── 03_feature_engineering.R # Création de variables dérivées
│   ├── 04_descriptive_analysis.R # Analyses descriptives et bivariées
│   ├── 05_risk_score.R          # Calcul du score de risque TDAH
│   ├── 05_risk_score_v2.R       # Version SANS sexe (recommandée)
│   └── 06_ml_models.R           # Modèles Machine Learning (Random Forest)
│
├── notebook_preprocessing.qmd    # Notebook interactif (pour apprendre)
├── rapport_principal.qmd         # Rapport complet
├── presentation.qmd              # Diapositives
└── index.qmd                     # Page d'accueil du projet
```

---

## 🚀 Comment Utiliser le Projet?

### Option 1: Pour Débutants (Recommandé)

**Utilisez le Notebook Interactif** qui explique chaque étape:

```bash
# Ouvrir le notebook dans RStudio ou VSCode
quarto render TDAH-Rstat/notebook_preprocessing.qmd
```

### Option 2: Pour Utilisateurs Avancés

**Exécutez les scripts R dans l'ordre:**

```r
# Dans R ou RStudio
setwd("TDAH-Rstat")

# 1. Import des données
source("scripts/01_data_import.R")

# 2. Nettoyage
source("scripts/02_data_cleaning.R")

# 3. Feature Engineering
source("scripts/03_feature_engineering.R")

# 4. Analyses descriptives
source("scripts/04_descriptive_analysis.R")

# 5. Calcul du score de risque (VERSION SANS SEXE)
source("scripts/05_risk_score_v2.R")

# 6. Modèles Machine Learning
source("scripts/06_ml_models.R")
```

---

## 📊 Analyses Incluses

### 1. Analyses Descriptives (Script 04)

**Ce que ça fait:**
- Calcule les moyennes, écarts-types
- Crée des tableaux de fréquences
- Génère des graphiques

**Exemple de résultat:**
```
Score TDAH moyen: 25.3 ± 12.5
10% des enfants ont un risque élevé
```

### 2. Analyses Bivariées (Script 04)

**Ce que ça fait:**
- Compare 2 groupes (urbain vs rural)
- Teste si les différences sont significatives

**Exemple de résultat:**
```
Test Chi²: Risque élevé vs Milieu
- Rural: 15% risque élevé
- Urbain: 8% risque élevé
- p < 0.001 → Différence significative!
```

### 3. Score de Risque TDAH (Script 05)

**Ce que ça fait:**
- Calcule un score de 0 à 100 pour chaque enfant
- Base le calcul sur 6 facteurs de risque (SANS sexe)

**Facteurs utilisés:**
1. Pauvreté (23.5% du score)
2. Âge maternel extrême (17.6%)
3. Intervalle intergénésique court (17.6%)
4. Faible éducation maternelle (17.6%)
5. Ordre de naissance élevé (11.8%)
6. Grande taille de ménage (11.8%)

**Catégories:**
- Faible: Score < 20
- Moyen: Score 20-39
- Élevé: Score ≥ 40

### 4. Machine Learning (Script 06)

**Ce que ça fait:**
- Entraîne un modèle Random Forest
- Prédit le risque élevé (Oui/Non)
- Évalue la performance du modèle

**Métriques calculées:**
- **Accuracy**: Taux de bonnes prédictions
- **Precision**: Parmi les prédictions "risque élevé", combien sont correctes?
- **Recall**: Parmi les vrais "risque élevé", combien sont détectés?
- **F1-Score**: Équilibre entre Precision et Recall
- **AUC**: Performance globale du modèle (0-1, plus proche de 1 = mieux)

**Exemple de résultat:**
```
Performance du modèle:
- Accuracy: 85%
- AUC: 0.82
- Variables les plus importantes:
  1. Richesse (pauvreté)
  2. Éducation maternelle
  3. Milieu de résidence
```

---

## ⚠️ Points Importants

### 1. Variable Sexe NON Utilisée

**Pourquoi?**
- Seulement 3% des données disponibles
- 97% de valeurs manquantes
- Impossible d'analyser de manière fiable

**Solution:**
- On utilise les 6 autres facteurs de risque
- Les poids ont été redistribués pour totaliser 100%
- C'est documenté comme limitation dans le rapport

### 2. Deux Versions du Script 05

| Fichier | Description | À Utiliser? |
|---------|-------------|-------------|
| `05_risk_score.R` | Version AVEC sexe | ❌ Non (données manquantes) |
| `05_risk_score_v2.R` | Version SANS sexe | ✅ **OUI** (recommandée) |

### 3. Interprétation des Résultats

**⚠️ IMPORTANT:**
- Le score de risque est **théorique**
- Il ne remplace **PAS** un diagnostic médical de TDAH
- C'est un **outil de dépistage** pour identifier les populations vulnérables

---

## 📈 Résultats Attendus

Après avoir exécuté tous les scripts, vous aurez:

### Fichiers de Données
- `dataset_with_score.csv`: Données avec score de risque calculé
- `05_risk_score.RData`: Workspace R du scoring
- `06_ml_models.RData`: Modèles ML entraînés
- `ml_metrics.csv`: Métriques de performance
- `variable_importance.csv`: Importance des variables

### Graphiques
- Distribution du score de risque
- Comparaisons par milieu/richesse/éducation
- Courbe ROC du modèle ML
- Importance des variables (barplot)

### Tableaux
- Statistiques descriptives
- Tableaux de contingence
- Matrice de confusion
- Métriques de performance

---

## 📚 Ressources

### Documentation R
- tidyverse: https://www.tidyverse.org/
- randomForest: https://cran.r-project.org/web/packages/randomForest/
- caret: http://topepo.github.io/caret/

### Statistiques
- Comprendre les Odds Ratios: https://www.statology.org/odds-ratio/
- Régression logistique: https://www.statology.org/logistic-regression-in-r/
- Random Forest: https://www.stat.berkeley.edu/~breiman/RandomForests/


---

**Dernière mise à jour:** 2 janvier 2026
**Auteur:** Asma BELKAHLA
**Contact:** asma.belkahla@polytechnicien.tn

