#' Variables globales internes pour eviter les notes R CMD check
#' @name internal_globals_inequality
#' @noRd
utils::globalVariables(c("Part_Population", "Part_Revenu"))

#' Visualisation de la courbe de Lorenz par sous-groupe
#'
#' Cette fonction genère une courbe de Lorenz globalement ou par sous-groupe
#' (par sexe, region, etc.).
#'
#' @param data Un data.frame contenant les donnees.
#' @param variable Nom de la variable representant le revenu ou la consommation (chaîne de caractères).
#' @param separateur (Optionnel) Nom de la colonne de regroupement pour creer les courbes par sous-groupe.
#'
#' @return Un graphique ggplot2 de la courbe de Lorenz.
#' @import ggplot2 ineq dplyr
#' @export
#' @examples
#' # Exemple avec des donnees fictives
#' df <- data.frame(
#'   revenu = c(100, 200, 300, 400, 1000, 2000),
#'   sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme")
#' )
#'
#' # Courbes globales
#' lorenz_curve(df, variable = "revenu")
#'
#' # Courbes par sexe
#' lorenz_curve(df, variable = "revenu", separateur = "sexe")

lorenz_curve <- function(data, variable, separateur = NULL) {

  if (!variable %in% names(data)) {
    stop("La variable specifiee n'existe pas dans les donnees.")
  }

  if (!is.null(separateur) && !separateur %in% names(data)) {
    stop("Le separateur specifie n'existe pas dans les donnees.")
  }

  # Fonction interne pour creer la courbe de Lorenz pour un sous-groupe
  create_lorenz_plot <- function(sub_data, group_name = "Global") {
    revenus <- sub_data[[variable]]
    revenus <- revenus[!is.na(revenus) & revenus > 0]

    # Verifier si les donnees sont suffisantes pour creer la courbe de Lorenz
    if (length(revenus) < 2) {
      message(paste("Pas suffisamment de donnees valides pour generer la courbe de Lorenz dans le groupe", group_name))
      return(NULL)
    }

    # Courbe de Lorenz
    lorenz_data <- ineq::Lc(revenus)
    lorenz_data <- data.frame(Part_Population = lorenz_data$p, Part_Revenu = lorenz_data$L)

    lorenz_plot <- ggplot2::ggplot(lorenz_data, ggplot2::aes(x = Part_Population, y = Part_Revenu)) +
      ggplot2::geom_line(color = "#d7301f", size = 1.5) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(title = paste("Courbe de Lorenz -", group_name),
                    x = "Part cumulee de la population",
                    y = "Part cumulee du revenu") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    return(lorenz_plot)
  }

  # Si aucun separateur, creer la courbe de Lorenz pour l'ensemble des donnees
  if (is.null(separateur)) {
    return(create_lorenz_plot(data))
  }

  # Sinon, creer les courbes de Lorenz par sous-groupe
  groupes <- unique(data[[separateur]])
  resultats <- list()

  for (groupe in groupes) {
    sous_donnees <- dplyr::filter(data, .data[[separateur]] == groupe)
    lorenz_plot <- create_lorenz_plot(sous_donnees, group_name = paste0(separateur, " = ", groupe))

    # Ajouter la courbe generee à la liste des resultats si elle n'est pas NULL
    if (!is.null(lorenz_plot)) {
      resultats[[as.character(groupe)]] <- lorenz_plot
    } else {
      message(paste("Pas de courbe generee pour le groupe", groupe))
    }
  }

  return(resultats)
}
