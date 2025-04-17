#' Evaluate Transfer Scenarios on Poverty Reduction
#'
#' Simulates and compares multiple targeted cash transfer scenarios in terms of
#' poverty reduction, cost, and efficiency. Each scenario applies a fixed amount
#' to a subset of households defined by a logical condition.
#'
#' @param data A data.frame containing household-level information
#' @param scenarios A named list of scenarios. Each element must be a list with:
#'   \itemize{
#'     \item \code{name}: label of the scenario (e.g., "Rural households")
#'     \item \code{condition}: a logical expression (quoted) defining eligibility
#'     \item \code{amount}: fixed transfer amount (numeric)
#'     \item \code{id} (optional): custom identifier (otherwise auto-generated)
#'   }
#' @param baseline Name of the baseline welfare (consumption) variable (e.g., "pcexp")
#' @param params A list with:
#'   \itemize{
#'     \item \code{var_poids}: name of the weight variable
#'     \item \code{var_seuil}: poverty line, as a column name or numeric
#'   }
#' @param pib Total GDP (numeric), to compute cost as a share of GDP
#' @param save_scenario Logical. If TRUE, saves the modified dataset with simulated variables
#' @param path_scenario File path where the simulated dataset will be saved (if \code{save_scenario = TRUE})
#' @param plot Logical. If TRUE, displays a barplot of scenario efficiency
#'
#' @return A list with two elements:
#' \describe{
#'   \item{table}{A data.frame containing scenario results (id, headcount, cost, efficiency, etc.)}
#'   \item{plot_efficiency}{A ggplot2 object showing the efficiency bar plot}
#' }
#'
#' @examples
#' df <- data.frame(
#'   pcexp = c(80000, 150000, 200000, 50000, 120000, 400000),
#'   hhweight = c(1.2, 1.5, 1.3, 0.8, 1.0, 1.7),
#'   zref = rep(150000, 6),
#'   milieu = c("Rural", "Rural", "Urban", "Rural", "Urban", "Urban"),
#'   hage = c(3, 7, 35, 67, 10, 80),
#'   hhandig = c(0, 0, 1, 0, 1, 0)
#' )
#'
#' scenarios <- list(
#'   list(name = "Universal", condition = quote(TRUE), amount = 100000),
#'   list(name = "Rural only", condition = quote(milieu == "Rural"), amount = 100000),
#'   list(name = "Child < 5", condition = quote(hage < 5), amount = 100000),
#'   list(name = "Disabled", condition = quote(hhandig == 1), amount = 100000)
#' )
#'
#' transfers_summary(
#'   data = df,
#'   scenarios = scenarios,
#'   baseline = "pcexp",
#'   params = list(var_poids = "hhweight", var_seuil = "zref"),
#'   pib = 1e12,
#'   save_scenario = FALSE,
#'   plot = TRUE
#' )
#'
#' @importFrom stats setNames
#' @import dplyr
#' @import ggplot2
#' @import haven
#' @export


transfers_summary <- function(data,
                              scenarios,
                              baseline,
                              params,
                              pib,
                              save_scenario = FALSE,
                              path_scenario = ".",
                              plot = TRUE) {

  data_clean <- data %>%
    mutate(across(where(haven::is.labelled), haven::as_factor))

  welfare_temp <- data_clean
  summary_table <- data.frame()

  poverty_headcount <- function(cons, poids, seuil) {
    cons <- as.numeric(cons)
    poids <- as.numeric(poids)
    seuil <- as.numeric(seuil)
    H <- mean((cons < seuil) * poids, na.rm = TRUE) / mean(poids, na.rm = TRUE)
    return(H)
  }

  H0 <- poverty_headcount(
    cons = data_clean[[baseline]],
    poids = data_clean[[params$var_poids]],
    seuil = data_clean[[params$var_seuil]]
  )

  if (is.null(names(scenarios)) || all(names(scenarios) == "")) {
    names(scenarios) <- paste0("s", seq_along(scenarios))
  }

  for (i in seq_along(scenarios)) {
    sc <- scenarios[[i]]
    id <- names(scenarios)[i]

    if (is.null(sc$amount)) stop(paste0("Montant manquant pour le scenario '", id, "'"))
    amount <- sc$amount

    eligible <- eval(sc$condition, envir = data_clean)

    cons_col <- paste0("cons_sim_", id)
    welfare_temp[[cons_col]] <- as.numeric(data_clean[[baseline]]) + ifelse(eligible, amount, 0)

    H1 <- poverty_headcount(
      cons = welfare_temp[[cons_col]],
      poids = data_clean[[params$var_poids]],
      seuil = data_clean[[params$var_seuil]]
    )

    delta <- as.numeric(H0 - H1)
    cost_total <- sum(ifelse(eligible, amount, 0) * as.numeric(data_clean[[params$var_poids]]), na.rm = TRUE)
    cost_col <- paste0("cost_", id)
    welfare_temp[[cost_col]] <- ifelse(eligible, amount, 0)

    efficiency <- ifelse(cost_total == 0, NA, delta / (cost_total / pib))

    headcount_pct <- H1 * 100
    delta_pct <- delta * 100
    cost_pct_pib <- (cost_total / pib) * 100

    res <- data.frame(
      id = id,
      scenario = if (!is.null(sc$name)) sc$name else id,
      amount = amount,
      headcount = headcount_pct,
      delta_poverty = delta_pct,
      cost = cost_total,
      cost_perc_pib = cost_pct_pib,
      efficiency = efficiency
    )

    summary_table <- bind_rows(summary_table, res)
  }

  if (save_scenario) {
    save_path <- file.path(path_scenario, "welfare_temp.dta")
    write_dta(welfare_temp, save_path)
  }

  # Création du graphique de l'efficacité
  plot_efficiency <- ggplot(summary_table, aes(x = id, y = efficiency)) +
    geom_col(fill = "steelblue") +
    labs(title = "Efficacite des scenarios",
         x = "Scenario",
         y = "Reduction du taux de pauvrete par % du PIB") +
    theme_minimal()

  if (plot) print(plot_efficiency)

  return(list(
    table = summary_table,
    plot_efficiency = plot_efficiency
  ))
}



