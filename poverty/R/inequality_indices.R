#' Calcul des indices d'inegalite
#'
#' Cette fonction permet de calculer plusieurs indices d'inegalite economique,
#' notamment la courbe de Lorenz, l'indice de Gini et l'indice de Theil.
#'
#' @param data Un data frame contenant les donnees.
#' @param variable Nom de la variable representant le revenu ou la consommation.
#' @param separateur Nom de la colonne permettant une analyse par sous-groupes (ex. sexe, region, milieu).
#' @return Une liste contenant :
#'   - La courbe de Lorenz
#'   - L'indice de Gini
#'   - L'indice de Theil
#' @import dplyr ggplot2 ineq DescTools
#' @export
inequality_indices <- function(data, variable, separateur = NULL) {

  # Verification de l'existence de la variable dans le dataset
  if (!variable %in% colnames(data)) {
    stop("La variable specifiee n'existe pas dans le dataset.")
  }

  # Verification de l'existence du separateur (si specifie)
  if (!is.null(separateur) && !separateur %in% colnames(data)) {
    stop("Le separateur specifie n'existe pas dans le dataset.")
  }

  # Chargement conditionnel des packages necessaires avec requireNamespace()

  # Utilisation de DescTools pour une description de la variable
  if (requireNamespace("DescTools", quietly = TRUE)) {
    desc_data <- DescTools::Desc(data[[variable]])  # Utilisation de la fonction Desc de DescTools
    print(desc_data)  # Affichage des statistiques descriptives
  } else {
    stop("Le package 'DescTools' n'est pas installe.")
  }

  # Calcul de l'indice de Gini avec le package ineq
  if (requireNamespace("ineq", quietly = TRUE)) {
    gini_index <- ineq::Gini(data[[variable]])  # Calcul de l'indice de Gini
  } else {
    stop("Le package 'ineq' n'est pas installe.")
  }

  # Visualisation avec ggplot2 (exemple : graphique de la distribution des revenus)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot(data, aes_string(x = variable)) +
      ggplot2::geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
      ggplot2::labs(title = paste("Distribution de la variable", variable),
                    x = variable, y = "Frequence")
  } else {
    stop("Le package 'ggplot2' n'est pas installe.")
  }

  # Fonction pour calculer les indices d'inegalite pour un sous-ensemble de donnees
  calcul_indicateurs <- function(sub_data) {
    revenus <- sub_data[[variable]]
    revenus <- revenus[!is.na(revenus) & revenus > 0]

    if (length(revenus) < 2) return(NULL)  # Si trop peu de donnees, retourner NULL

    # Calcul de la courbe de Lorenz
    lorenz_curve <- ineq::Lc(revenus)

    # Affichage de la courbe de Lorenz
    plot(lorenz_curve, main = "Courbe de Lorenz", xlab = "Part de la population", ylab = "Part du revenu cumule")

    # Calcul des indices
    gini_index <- ineq::Gini(revenus)
    theil_index <- ineq::Theil(revenus)

    # Retourner les resultats sous forme de liste
    list(
      lorenz = lorenz_curve,
      gini = gini_index,
      theil = theil_index
    )
  }

  # Si un separateur est specifie, calculer les indices pour chaque sous-groupe
  if (is.null(separateur)) {
    resultats <- calcul_indicateurs(data)
  } else {
    resultats <- data %>%
      dplyr::group_by(.data[[separateur]]) %>%
      dplyr::group_map(~ calcul_indicateurs(.x), .keep = TRUE)
  }

  # Retourner les resultats sous forme de liste
  return(resultats)
}
