## DEVELOPPEMENT DE PACKAGE AVEC R PAR PROSPER LAWA FOUMSOU ET SAMBA DIENG
# 📦 ehcvmpoverty

**ehcvmpoverty** est un projet de développement de package visant à calculer des indicateurs de pauvreté et d'inégalité et d'évaluer des impacts de politique contre la pauvreté. Il permet de générer des tableaux et des graphiques automatiquement, exportés dans un fichier Excel structuré par feuille, afin de faciliter l'analyse socio-économique.

## 🚀 Objectifs du package

- Automatiser le calcul d’indicateurs de pauvreté et d’inégalités.
- Générer des tableaux statistiques et graphiques exploitables directement.
- Offrir des fonctions généralisées pour l’analyse par sous-groupes (sexe, région, milieu, etc.).
- Simplifier l’évaluation de l’impact de politiques telles que les transferts monétaires.

## 📚 Fonctions principales (en cours de développement)

- `population_distribution(data, separateur)` : Décrit la répartition de la population selon une variable.
- `poverty_summary(data, separateur)` : Calcule les indicateurs de pauvreté (headcount, gap, squared gap, Gini).
- `inequality_indices(data, separateur)` : Produit des indices d’inégalités (Lorenz, Gini, Theil).
- `monetary_transfers_impact(data, separateur, transfer_amount)` : Évalue l’impact de transferts monétaires sur la pauvreté et les inégalités.
- `generate_full_report(data)` : Génère un rapport Excel avec toutes les analyses.

Chaque fonction produit un tableau R structuré et l’exporte dans un fichier Excel avec une feuille dédiée.

## ⚙️ Caractéristiques techniques

- Données d'entrée : bases de données EHCVM propres et harmonisées ou d'autres bases qui ont des variables socio-économiques et de seuil de pauvreté.
- Exports : fichiers Excel multi-feuilles (via `openxlsx`).
- Analyse par groupe possible via l’argument `separateur` (ex. sexe, région).

## 📌 Statut

🔧 Fonctions en cours de développement et de test  
📦 Packaging prévu une fois les fonctions stabilisées



---

