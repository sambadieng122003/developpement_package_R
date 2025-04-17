#' Données sur le bien-être socio-économique (EHCVM, Sénégal 2018)
#'
#' Ce jeu de données provient de l'Enquête Harmonisée sur les Conditions de Vie des Ménages (EHCVM) réalisée au Sénégal en 2018.
#' Il contient des informations détaillées sur les conditions socio-économiques des ménages, notamment leurs revenus, dépenses et caractéristiques démographiques.
#'
#' @format Un data frame avec 35 variables :
#' \describe{
#'   \item{country}{Pays (Sénégal)}
#'   \item{year}{Année de l'enquête (2018)}
#'   \item{hhid}{Identifiant unique du ménage}
#'   \item{grappe}{Numéro de grappe (zone de sondage)}
#'   \item{menage}{Identifiant du ménage dans la grappe}
#'   \item{vague}{Vague de l'enquête}
#'   \item{zae}{Zone agro-écologique}
#'   \item{region}{Région géographique du ménage}
#'   \item{milieu}{Type de milieu de résidence (Urbain/Rural)}
#'   \item{hhweight}{Pondération du ménage}
#'   \item{hhsize}{Taille du ménage (nombre de personnes)}
#'   \item{eqadu1}{Échelle d'équivalence adulte 1}
#'   \item{eqadu2}{Échelle d'équivalence adulte 2}
#'   \item{hgender}{Sexe du chef de ménage}
#'   \item{hage}{Âge du chef de ménage}
#'   \item{hmstat}{Statut matrimonial du chef de ménage}
#'   \item{hreligion}{Religion du chef de ménage}
#'   \item{hnation}{Nationalité du chef de ménage}
#'   \item{halfab}{Capacité de lecture et d'écriture du chef de ménage}
#'   \item{heduc}{Niveau d'éducation du chef de ménage}
#'   \item{hdiploma}{Diplôme obtenu par le chef de ménage}
#'   \item{hhandig}{Présence d'un handicap chez le chef de ménage}
#'   \item{hactiv7j}{Activité principale des 7 derniers jours}
#'   \item{hactiv12m}{Activité principale des 12 derniers mois}
#'   \item{hbranch}{Branche d'activité économique}
#'   \item{hsectins}{Secteur d'activité (Privé/Public)}
#'   \item{hcsp}{Catégorie socioprofessionnelle}
#'   \item{dali}{Dépenses alimentaires (en monnaie locale)}
#'   \item{dnal}{Dépenses non alimentaires (en monnaie locale)}
#'   \item{dtot}{Dépenses totales du ménage}
#'   \item{pcexp}{Dépenses par tête du ménage}
#'   \item{zzae}{Zone agro-écologique ajustée}
#'   \item{zref}{Seuil de pauvreté de référence}
#'   \item{def_spa}{Déflateur spatial}
#'   \item{def_temp}{Déflateur temporel}
#' }
#' @source Agence nationale de la Statistique et de la Démographie (2018)
#' @examples
#' data(welfare)
#' head(welfare)
"welfare"

