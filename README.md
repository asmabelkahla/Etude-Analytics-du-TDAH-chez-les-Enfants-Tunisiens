# Facteurs de Risque du TDAH chez l'Enfant en Tunisie

## 📊 Analyse Secondaire des Données MICS6 (2023)

### Contexte du Projet

Ce projet analyse les facteurs périnataux, familiaux et socio-économiques associés au risque théorique de TDAH (Trouble du Déficit de l'Attention avec ou sans Hyperactivité) chez les enfants en Tunisie, en utilisant les données de l'enquête MICS6 (Multiple Indicator Cluster Survey) réalisée en 2023.

**Motivation** : Face à la prévalence croissante du TDAH dans les écoles tunisiennes (plus de 20 enfants identifiés dans une seule école), cette étude vise à identifier les populations à risque et à orienter les politiques de prévention.

### ⚠️ Avertissement Important

Cette étude ne permet **PAS** de :
- Diagnostiquer le TDAH (pas de données cliniques dans MICS6)
- Établir des relations causales (étude transversale)

Elle permet de :
- Identifier des facteurs de risque reconnus dans la littérature
- Construire un profil de vulnérabilité théorique
- Décrire les inégalités dans la distribution du risque

## 🎯 Objectifs

### Objectif Général
Étudier l'association entre facteurs périnataux, familiaux et socio-économiques et un profil de vulnérabilité théorique au TDAH chez les enfants tunisiens.

### Objectifs Spécifiques
1. Caractériser les facteurs périnataux (âge maternel, ordre de naissance, espacement des naissances)
2. Décrire le profil socio-économique des ménages
3. Construire un score synthétique de vulnérabilité au TDAH
4. Analyser les associations avec les caractéristiques de l'enfant et du ménage
5. Explorer les inégalités (sexe, richesse, milieu urbain/rural, région)

## 📁 Structure des Données

### Fichiers MICS6 Utilisés
- `hh.csv` : Données sur les ménages
- `hl.csv` : Liste des membres du ménage
- `wm.csv` : Femmes âgées de 15-49 ans
- `mn.csv` : Hommes âgés de 15-49 ans
- `bh.csv` : Historique des naissances
- `ch.csv` : Données sur les enfants
- `fs.csv` : Sécurité alimentaire

## 🛠️ Technologies

- **Langage** : R (version ≥ 4.3.0)
- **IDE** : VSCode avec extensions R
- **Rapport** : Quarto
- **Contrôle de version** : Git/GitHub
- **Gestion des packages** : renv

## 📦 Packages R Principaux

```r
# Manipulation de données
- tidyverse (dplyr, ggplot2, tidyr, readr)
- data.table

# Analyses statistiques
- survey (plan de sondage complexe)
- gtsummary (tableaux descriptifs)
- broom (résultats de modèles)

# Modélisation
- lme4 (modèles mixtes si nécessaire)
- car (tests ANOVA)

# Visualisation
- ggplot2
- patchwork
- scales
```

## 🚀 Installation et Démarrage

### 1. Cloner le Repository
```bash
git clone https://github.com/votre-username/tdah-tunisie-mics6.git
cd tdah-tunisie-mics6
```

### 2. Installer R et RStudio/VSCode
- R : https://cran.r-project.org/
- VSCode : https://code.visualstudio.com/
- Extensions VSCode : R, R Debugger, Quarto

### 3. Restaurer l'Environnement R
```r
# Dans R
install.packages("renv")
renv::restore()
```

### 4. Placer les Données
Télécharger les fichiers MICS6 depuis UNICEF et les placer dans `data/raw/`

### 5. Exécuter l'Analyse
```r
# Scripts à exécuter dans l'ordre
source("scripts/01_data_import.R")
source("scripts/02_data_cleaning.R")
source("scripts/03_feature_engineering.R")
source("scripts/04_descriptive_analysis.R")
source("scripts/05_risk_score.R")
source("scripts/06_statistical_models.R")
```

### 6. Générer le Rapport
```bash
quarto render reports/rapport_principal.qmd
```

## 📊 Variables d'Intérêt

### Variables Dépendantes (à construire)
- Score de vulnérabilité théorique au TDAH
- Profil de risque catégoriel (faible/moyen/élevé)

### Variables Indépendantes

**Facteurs Périnataux** :
- Âge maternel à la naissance
- Ordre de naissance
- Intervalle intergénésique
- Poids de naissance (si disponible)
- Suivi prénatal

**Facteurs Familiaux** :
- Éducation des parents
- Structure familiale
- Taille du ménage
- Nombre d'enfants

**Facteurs Socio-économiques** :
- Quintile de richesse
- Milieu de résidence (urbain/rural)
- Région/gouvernorat
- Accès aux services de santé

## 📈 Méthodes Statistiques

1. **Analyses Descriptives**
   - Fréquences, moyennes, écarts-types
   - Tableaux croisés

2. **Analyses Bivariées**
   - Tests du Chi²
   - Tests de Student / ANOVA
   - Corrélations

3. **Analyses Multivariées**
   - Régression logistique (risque élevé vs faible)
   - Régression linéaire (score continu)
   - Analyses de sous-groupes

4. **Prise en compte du Plan de Sondage**
   - Pondérations
   - Stratification
   - Clusters

## 👥 Contribution

Ce projet est développé dans un cadre académique. Pour toute question ou suggestion, merci d'ouvrir une issue.

## 📄 Licence

Ce projet utilise des données UNICEF MICS6. Veuillez respecter les conditions d'utilisation des données MICS.

## 📚 Références

- UNICEF. (2023). Multiple Indicator Cluster Survey (MICS6) - Tunisia.


## 🙏 Remerciements

- UNICEF Tunisie pour la mise à disposition des données MICS6
- Mr.Abdallah Khemais pour son encadrement et ses conseils précieux tout au long de ce projet.

---

**Dernière mise à jour** : Décembre 2025
**Contact** : asma.belkahla@polytechnicien.tn
