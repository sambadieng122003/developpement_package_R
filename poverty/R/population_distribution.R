#' Variables globales internes
#' @name internal_globals
#' @noRd
utils::globalVariables(c("Nombre", "Percentage"))
#' Analyse la repartition de la population par variable
#'
#' Cette fonction permet d'analyser la repartition d'une population selon une variable donnee.
#' - Si la variable est qualitative, elle affiche un tableau de repartition (ponderee ou non) et un graphique en barres.
#' - Si elle est quantitative, elle affiche des statistiques univariees avec DescTools et un histogramme.
#'
#' @param data Un data.frame contenant les donnees.
#' @param separateur Une chaîne de caractères indiquant la variable à analyser.
#' @param poids (optionnel) Une chaîne de caractères indiquant le nom de la variable de ponderation.
#'
#' @return Un graphique (barplot ou histogramme) et l'impression en console d'un tableau de statistiques ou de repartition.
#' @importFrom DescTools Desc
#' @export
#'
#' @examples
#' df <- data.frame(
#'   sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme"),
#'   age = c(30, 25, 35, 40, 45, 50),
#'   poids = c(1, 2, 1, 1, 3, 2)
#' )
#' population_distribution(df, separateur = "sexe", poids = "poids")
#' population_distribution(df, separateur = "age")
population_distribution <- function(data, separateur, poids = NULL) {
  if (!separateur %in% names(data)) {
    stop("Le separateur specifie n'existe pas dans les donnees.")
  }

  if (!is.null(poids) && !poids %in% names(data)) {
    stop("La variable de ponderation specifiee n'existe pas dans les donnees.")
  }

  variable <- data[[separateur]]

  # Gerer les variables haven_labelled
  if ("haven_labelled" %in% class(variable)) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Le package 'haven' est requis pour traiter les variables labellisees.")
    }
    variable <- haven::as_factor(variable)
  }

  # Cas quantitatif
  if (is.numeric(variable)) {
    if (!requireNamespace("DescTools", quietly = TRUE)) {
      stop("Le package 'DescTools' n'est pas installe.")
    }

    poids_vec <- if (!is.null(poids)) data[[poids]] else NULL
    stats <- DescTools::Desc(variable, weights = poids_vec, plotit = TRUE)
    return(stats)
  }

  # Cas qualitatif
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop("Le package 'forcats' n'est pas installe.")
  }

  variable <- forcats::as_factor(variable)
  data[[separateur]] <- variable

  if (is.null(poids)) {
    distribution <- data %>%
      dplyr::group_by(.data[[separateur]]) %>%
      dplyr::summarise(Nombre = dplyr::n(), .groups = 'drop') %>%
      dplyr::mutate(Percentage = (Nombre / sum(Nombre)) * 100)
  } else {
    distribution <- data %>%
      dplyr::group_by(.data[[separateur]]) %>%
      dplyr::summarise(Nombre = sum(.data[[poids]], na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(Percentage = (Nombre / sum(Nombre)) * 100)
  }

  print(distribution)

  return(
    ggplot2::ggplot(distribution, ggplot2::aes(x = .data[[separateur]], y = Percentage, fill = .data[[separateur]])) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(title = paste("Repartition de la population par", separateur),
                    x = separateur, y = "Pourcentage") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )
}
