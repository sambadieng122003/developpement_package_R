#' Composition of poverty
#'
#' Computes the composition of poverty: share of each group's contribution
#' to national FGT indicators (headcount, gap, severity).
#'
#' @param data A data.frame with welfare, weights, and poverty line
#' @param separateur Character vector of grouping variables
#' @param params A list with:
#'   - var_cons: consumption variable
#'   - var_poids: weight variable
#'   - var_seuil: poverty line (numeric or variable name)
#'
#' @return A data.frame with:
#'   - headcount (FGT0)
#'   - contribution_to_headcount (%)
#'   - contribution_to_gap (%)
#'   - contribution_to_severity (%)
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
#' poverty_composition(
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
#' poverty_composition(
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

poverty_composition <- function(data, separateur = NULL, params = list()) {

  if (!all(c("var_cons", "var_poids", "var_seuil") %in% names(params))) {
    stop("params must include 'var_cons', 'var_poids', and 'var_seuil'")
  }

  # 1. Préparer les données
  data <- data |>
    dplyr::mutate(
      cons  = .data[[params$var_cons]],
      poids = .data[[params$var_poids]],
      seuil = if (is.numeric(params$var_seuil)) params$var_seuil else .data[[params$var_seuil]],
      poor  = ifelse(cons < seuil, 1, 0),
      gap   = ifelse(cons < seuil, (seuil - cons) / seuil, 0),
      gap2  = gap^2
    )

  # 2. Gérer les labels
  if (!is.null(separateur)) {
    data <- data |>
      dplyr::mutate(across(all_of(separateur), ~ if (haven::is.labelled(.x)) {
        haven::as_factor(.x)
      } else {
        as.character(.x)
      }))
  }

  # 3. Calcul total national pour normalisation
  total_weight <- sum(data$poids, na.rm = TRUE)
  total_fgt0 <- sum(data$poids * data$poor, na.rm = TRUE)
  total_fgt1 <- sum(data$poids * data$gap, na.rm = TRUE)
  total_fgt2 <- sum(data$poids * data$gap2, na.rm = TRUE)

  # 4. Fonction de résumé par groupe
  composition_summary <- function(df) {
    weight <- sum(df$poids, na.rm = TRUE)
    fgt0 <- sum(df$poids * df$poor, na.rm = TRUE)
    fgt1 <- sum(df$poids * df$gap, na.rm = TRUE)
    fgt2 <- sum(df$poids * df$gap2, na.rm = TRUE)

    data.frame(
      headcount = round(100 * fgt0 / weight, 1),
      contribution_to_headcount = round(100 * fgt0 / total_fgt0, 1),
      contribution_to_gap       = round(100 * fgt1 / total_fgt1, 1),
      contribution_to_severity  = round(100 * fgt2 / total_fgt2, 1)
    )
  }

  # 5. Calcul principal
  if (is.null(separateur)) {
    result <- composition_summary(data)
  } else {
    result <- data |>
      dplyr::group_by(across(all_of(separateur))) |>
      dplyr::group_modify(~ composition_summary(.x)) |>
      dplyr::ungroup()
  }

  # 6. Ligne Total
  total_row <- composition_summary(data)
  if (!is.null(separateur)) {
    total_values <- as.list(setNames(rep("Total", length(separateur)), separateur))
    total_row <- cbind(as.data.frame(total_values), total_row)
    result <- dplyr::bind_rows(result, total_row)
  }

  return(result)
}

