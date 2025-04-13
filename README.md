## DEVELOPPEMENT DE PACKAGE AVEC R PAR PROSPER LAWA FOUMSOU ET SAMBA DIENG
voici le lien de l'appli 👉 [Dashboard sécurité UEMOA](https://ahmedniass.shinyapps.io/Dashboard_UEMOA_securite/)
# 📦 poverty

**poverty** est un projet de développement de package visant à calculer des indicateurs de pauvreté et d'inégalité et d'évaluer des impacts de politique de transferts monétaires contre la pauvreté. Il permet de générer des tableaux et des graphiques automatiquement, exportés dans un fichier Excel structuré par feuille, afin de faciliter l'analyse socio-économique.

## 🚀 Objectifs du package

- Automatiser le calcul d’indicateurs de pauvreté et d’inégalités.
- Evaluer les impacts de transferts monétaires sur la réduction de la pauvreté et des inégalités.
- Générer des tableaux statistiques et graphiques exploitables directement.
- Offrir des fonctions généralisées pour l’analyse par sous-groupes (sexe, région, milieu, etc.).


## 📚 Fonctions principales (en cours de développement)

- `population_distribution` : Décrit la répartition de la population selon une variable.
- `poverty_summary` : Calcule les indicateurs de pauvreté (headcount, gap, squared gap) ainsi que la répartition de la pauvreté.
- `inequality_indices` : Produit des indices d’inégalités (Lorenz, Gini, Theil).
- `monetary_transfers_impact` : Évalue l’impact de transferts monétaires sur la pauvreté et les inégalités.
- `generate_full_report` : Génère un rapport Excel avec toutes les analyses.

Chaque fonction produit un tableau R structuré et l’exporte dans un fichier Excel avec une feuille dédiée.

## ⚙️ Caractéristiques techniques

- Données d'entrée : bases de données EHCVM propres et harmonisées ou d'autres bases qui ont des variables socio-économiques( au minimum les variables de consommation ou de dépenses, la variable poids et la variable seuil de pauvreté).
- Exports : fichiers Excel multi-feuilles (via `openxlsx`).
- Analyse par groupe possible via l’argument `separateur` (ex. sexe, région, milieu de résidence).

## 📌 Statut

🔧 Fonctions en cours de développement et de test  
📦 Packaging prévu une fois les fonctions stabilisées



---

