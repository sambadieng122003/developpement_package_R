#' Distribution of Poverty Across Groups
#'
#' Computes the distribution of poverty across groups (region, sex, etc.),
#' including headcount rate, share of poor, and share of population.
#'
#' @param data A data.frame with welfare, weights, and poverty line
#' @param separateur Character vector of grouping variables
#' @param params A list with:
#'   - var_cons: name of consumption variable
#'   - var_poids: name of weight variable
#'   - var_seuil: fixed value or column name for poverty line
#'
#' @return A data.frame with 3 indicators + total row
#' @examples
#' # Création d'une base fictive
#' df <- data.frame(
#'   pcexp = c(100000, 200000, 150000, 300000, 120000, 50000),
#'   hhweight = c(1.5, 1.2, 1.8, 2.0, 1.3, 0.9),
#'   zref = c(150000, 150000, 150000, 150000, 150000, 150000),
#'   milieu = c("Urbain", "Rural", "Urbain", "Urbain", "Rural", "Rural")
#' )
#'
#' # Calcul global (sans regroupement)
#' poverty_distribution(
#'   data = df,
#'   separateur = NULL,
#'   params = list(
#'     var_cons = "pcexp",
#'     var_poids = "hhweight",
#'     var_seuil = "zref"
#'   )
#' )
#'
#' # Calcul par milieu de résidence
#' poverty_distribution(
#'   data = df,
#'   separateur = "milieu",
#'   params = list(
#'     var_cons = "pcexp",
#'     var_poids = "hhweight",
#'     var_seuil = "zref"
#'   )
#' )
#'
#' @import dplyr
#' @import ggplot2
#' @import haven
#' @import  rlang
#' @export

poverty_distribution <- function(data, separateur = NULL, params = list()) {


  if (!all(c("var_cons", "var_poids", "var_seuil") %in% names(params))) {
    stop("params must include 'var_cons', 'var_poids', and 'var_seuil'")
  }

  # 1. Préparer les données
  data <- data |>
    dplyr::mutate(
      cons  = .data[[params$var_cons]],
      poids = .data[[params$var_poids]],
      seuil = if (is.numeric(params$var_seuil)) params$var_seuil else .data[[params$var_seuil]],
      poor  = ifelse(cons < seuil, 1, 0)
    )

  # 2. Gérer les séparateurs avec labels
  if (!is.null(separateur)) {
    data <- data |>
      dplyr::mutate(across(all_of(separateur), ~ if (haven::is.labelled(.x)) {
        haven::as_factor(.x)
      } else {
        as.character(.x)
      }))
  }

  # 3. Totaux globaux
  total_weight <- sum(data$poids, na.rm = TRUE)
  total_poor   <- sum(data$poids * data$poor, na.rm = TRUE)

  # 4. Fonction de résumé pour un groupe
  dist_summary <- function(df) {
    group_weight <- sum(df$poids, na.rm = TRUE)
    group_poor   <- sum(df$poids * df$poor, na.rm = TRUE)

    data.frame(
      poverty_headcount   = round(100 * group_poor / group_weight, 1),
      share_of_poor       = round(100 * group_poor / total_poor, 1),
      share_of_population = round(100 * group_weight / total_weight, 1)
    )
  }

  # 5. Calcul principal
  if (is.null(separateur)) {
    result <- dist_summary(data)
  } else {
    result <- data |>
      dplyr::group_by(across(all_of(separateur))) |>
      dplyr::group_modify(~ dist_summary(.x)) |>
      dplyr::ungroup()
  }

  # 6. Ligne Total
  total_row <- dist_summary(data)
  if (!is.null(separateur)) {
    total_values <- as.list(setNames(rep("Total", length(separateur)), separateur))
    total_row <- cbind(as.data.frame(total_values), total_row)
    result <- dplyr::bind_rows(result, total_row)
  }

  return(result)
}


