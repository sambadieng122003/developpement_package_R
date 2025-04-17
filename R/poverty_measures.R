#' Calculate FGT Poverty Indicators
#'
#' Calculates the FGT (0, 1, 2) poverty indicators and returns them by group,
#' using sampling weights. Handles variable labels from labelled data (e.g. Stata).
#' @param data A data.frame with consumption, weights, and possibly poverty line variable
#' @param separateur Character vector of grouping variable names (e.g. "region", "sex"), or NULL
#' @param params A list with:
#'   - var_cons: consumption variable name
#'   - var_poids: weight variable name
#'   - var_seuil: fixed numeric poverty line or name of a column
#'
#' @return A data.frame with FGT indicators (in percent), including a Total row
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
#' poverty_measures(
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
#' poverty_measures(
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

poverty_measures <- function(data, separateur = NULL, params = list()) {
  # 1. Vérification des paramètres
  if (!all(c("var_cons", "var_poids", "var_seuil") %in% names(params))) {
    stop("params must include 'var_cons', 'var_poids' and 'var_seuil'.")
  }

  # 2. Préparer les variables nécessaires
  data <- data |>
    dplyr::mutate(
      cons  = .data[[params$var_cons]],
      poids = .data[[params$var_poids]],
      seuil = if (is.numeric(params$var_seuil)) params$var_seuil else .data[[params$var_seuil]],
      poor  = ifelse(cons < seuil, 1, 0),
      gap   = ifelse(cons < seuil, (seuil - cons) / seuil, 0),
      gap2  = gap^2
    )

  # 3. Traiter les séparateurs et labels
  if (!is.null(separateur)) {
    data <- data |>
      dplyr::mutate(across(all_of(separateur), ~ if (haven::is.labelled(.x)) {
        haven::as_factor(.x)
      } else {
        as.character(.x)
      }))
  }

  # 4. Fonction de calcul FGT pondéré
  fgt_summary <- function(df) {
    total_weight <- sum(df$poids, na.rm = TRUE)
    if (total_weight == 0) {
      return(data.frame(
        poverty_headcount = NA,
        poverty_gap = NA,
        poverty_severity = NA
      ))
    }

    data.frame(
      poverty_headcount = round(100 * sum(df$poids * df$poor, na.rm = TRUE) / total_weight, 1),
      poverty_gap       = round(100 * sum(df$poids * df$gap,  na.rm = TRUE) / total_weight, 1),
      poverty_severity  = round(100 * sum(df$poids * df$gap2, na.rm = TRUE) / total_weight, 1)
    )
  }

  # 5. Calcul principal
  if (is.null(separateur)) {
    result <- fgt_summary(data)
  } else {
    result <- data |>
      dplyr::group_by(across(all_of(separateur))) |>
      dplyr::group_modify(~ fgt_summary(.x)) |>
      dplyr::ungroup()
  }

  # 6. Ajout de la ligne Total
  total_row <- fgt_summary(data)
  if (!is.null(separateur)) {
    total_values <- as.list(setNames(rep("Total", length(separateur)), separateur))
    total_row <- cbind(as.data.frame(total_values), total_row)
    result <- dplyr::bind_rows(result, total_row)

  }

  return(result)
}






