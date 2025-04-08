#' Variables globales internes
#' @name internal_globals
#' @noRd
utils::globalVariables(c("Nombre", "Percentage"))

#' Analyse la repartition de la population par categories
#'
#' Cette fonction permet d'analyser la repartition d'une population selon une variable categorielle donnee.
#'
#' @param data Un data.frame contenant les donnees.
#' @param separateur Une chaîne de caractères indiquant la variable par laquelle effectuer la repartition.
#'
#' @return Un graphique et un tableau recapitulatif de la repartition.
#' @export
#' @examples
#'
#' # Exemple avec des données fictives
#' df <- data.frame(
#'   sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme"),
#'   age = c(30, 25, 35, 40, 45, 50)
#' )
#' # Analyser la répartition par sexe
#' population_distribution(df, separateur = "sexe")

population_distribution <- function(data, separateur) {
  # Verification si la variable separateur existe dans les donnees
  if (!separateur %in% names(data)) {
    stop("Le separateur specifie n'existe pas dans les donnees.")
  }

  # Verification de la disponibilite du package forcats
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop("Le package 'forcats' n'est pas installe.")
  }

  # Conversion en facteur si necessaire
  data[[separateur]] <- forcats::as_factor(data[[separateur]])

  # Calcul de la repartition
  distribution <- data %>%
    dplyr::group_by(.data[[separateur]]) %>%
    dplyr::summarise(Nombre = dplyr::n(), .groups = 'drop') %>%
    dplyr::mutate(Percentage = (Nombre / sum(Nombre)) * 100)

  # Affichage du tableau
  print(distribution)

  # Creation du graphique
  ggplot2::ggplot(distribution, ggplot2::aes(x = .data[[separateur]], y = Percentage, fill = .data[[separateur]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = paste("Repartition de la population par", separateur),
                  x = separateur, y = "Pourcentage") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
