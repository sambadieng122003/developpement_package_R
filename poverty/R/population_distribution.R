#' Analyse la repartition de la population par categories
#'
#' @param data Un data.frame contenant les donnees.
#' @param separateur Une chaîne de caractères indiquant la variable par laquelle effectuer la repartition.
#'
#' @return Un graphique et un tableau recapitulatif de la repartition.
#' @export
population_distribution <- function(data, separateur) {
  # Verification si la variable separateur existe dans les donnees
  if (!separateur %in% names(data)) {
    stop("Le separateur specifie n'existe pas dans les donnees.")
  }dto

  # Charger forcats pour as_factor() si necessaire
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop("Le package 'forcats' n'est pas installe.")
  }

  # Convertir la variable separateur en facteur, si elle est de type 'haven_labelled'
  data[[separateur]] <- forcats::as_factor(data[[separateur]])

  # Calcul de la repartition
  distribution <- data %>%
    dplyr::group_by(.data[[separateur]]) %>%
    dplyr::summarise(Nombre = n(), .groups = 'drop') %>%
    dplyr::mutate(Percentage = (Nombre / sum(Nombre)) * 100)

  # Afficher la table recapitulative
  print(distribution)

  # Creer un graphique avec ggplot2
  ggplot2::ggplot(distribution, ggplot2::aes(x = .data[[separateur]], y = Percentage, fill = .data[[separateur]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = paste("Repartition de la population par", separateur),
                  x = separateur, y = "Pourcentage") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))  # Rotation des etiquettes pour les categories longues
}
