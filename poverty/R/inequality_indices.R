#' Calcul des indices de Gini et de Theil (en pourcentage)
#'
#' Cette fonction calcule deux indicateurs d'inegalite economique :
#' l'indice de Gini et l'indice de Theil, globalement ou par sous-groupes.
#'
#' @param data Un data frame contenant les donnees.
#' @param variable Nom de la variable numerique (revenu ou consommation).
#' @param separateur Nom de la colonne de regroupement pour une analyse par sous-groupes (facultatif).
#'
#' @return Une data.frame contenant les indices de Gini et de Theil (en pourcentage).
#' @import dplyr ineq
#' @export
#' @examples
#' df <- data.frame(
#'   revenu = c(100, 200, 300, 400, 1000, 2000),
#'   sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme")
#' )
#' inequality_indices(df, variable = "revenu")
#' inequality_indices(df, variable = "revenu", separateur = "sexe")
inequality_indices <- function(data, variable, separateur = NULL) {

  # Verification des colonnes
  if (!variable %in% names(data)) {
    stop("La variable specifiee n'existe pas dans le dataset.")
  }

  if (!is.null(separateur) && !separateur %in% names(data)) {
    stop("Le separateur specifie n'existe pas dans le dataset.")
  }

  # Gerer les variables haven_labelled (separateur)

  if (!is.null(separateur) && "haven_labelled" %in% class(data[[separateur]])) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Le package 'haven' est requis pour traiter les variables labellisees.")
    }
    data[[separateur]] <- haven::as_factor(data[[separateur]])
  }

  # Fonction de calcul des indices (en pourcentage)
  calcul <- function(df) {
    revenus <- df[[variable]]
    revenus <- revenus[!is.na(revenus) & revenus > 0]
    if (length(revenus) < 2) return(data.frame(gini = NA, theil = NA))
    data.frame(
      gini = ineq::Gini(revenus) * 100,
      theil = ineq::Theil(revenus) * 100
    )
  }

  # Application globale ou par groupe
  if (is.null(separateur)) {
    resultats <- calcul(data)
  } else {
    resultats <- data %>%
      dplyr::group_by(.data[[separateur]]) %>%
      dplyr::group_modify(~ calcul(.x)) %>%
      dplyr::ungroup()
  }

  return(resultats)
}
