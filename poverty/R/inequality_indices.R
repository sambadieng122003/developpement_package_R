#' Calcul des indices de Gini et de Theil
#'
#' Cette fonction calcule deux indicateurs d'inegalite economique :
#' l'indice de Gini et l'indice de Theil, globalement ou par sous-groupes.
#'
#' @param data Un data frame contenant les donnees.
#' @param variable Nom de la variable numerique (revenu ou consommation).
#' @param separateur Nom de la colonne de regroupement pour une analyse par sous-groupes (facultatif).
#'
#' @return Une data.frame contenant les indices de Gini et de Theil.
#' @import dplyr ineq
#' @export
#' @examples
#' # Exemple simple avec des donnees fictives
#' df <- data.frame(
#'   revenu = c(100, 200, 300, 400, 1000, 2000),
#'   sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme")
#' )
#'
#' # Calcul global des indices
#' inequality_indices(df, variable = "revenu")
#'
#' # Calcul par sexe
#' inequality_indices(df, variable = "revenu", separateur = "sexe")
inequality_indices <- function(data, variable, separateur = NULL) {

  # Verifications
  if (!variable %in% names(data)) {
    stop("La variable specifiee n'existe pas dans le dataset.")
  }

  if (!is.null(separateur) && !separateur %in% names(data)) {
    stop("Le separateur specifie n'existe pas dans le dataset.")
  }

  # Fonction interne
  calcul <- function(df) {
    revenus <- df[[variable]]
    revenus <- revenus[!is.na(revenus) & revenus > 0]
    if (length(revenus) < 2) return(data.frame(gini = NA, theil = NA))
    data.frame(
      gini = ineq::Gini(revenus),
      theil = ineq::Theil(revenus)
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
