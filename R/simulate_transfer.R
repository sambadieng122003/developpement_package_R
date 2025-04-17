#' Simulate a Cash Transfer
#'
#' Applies a fixed transfer amount to eligible households based on a condition,
#' and optionally returns the cost of the scenario.
#'
#' @param data A data.frame with household-level information
#' @param condition A logical expression (quoted) defining eligible households
#' @param amount Numeric. The fixed transfer amount (e.g., 100000)
#' @param var_cons Name of the consumption variable (e.g., "pcexp")
#' @param var_poids (Optional) Name of the weight variable (for costing)
#' @param return_cost Logical. If TRUE, returns a list with cost and simulated consumption
#'
#' @return Either a numeric vector of simulated consumption (if return_cost = FALSE),
#'         or a list with \code{cons_sim} and \code{cost}.
#'
#' @export




simulate_transfer <- function(data,
                              condition,
                              amount,
                              var_cons,
                              var_poids = NULL,
                              return_cost = FALSE) {


  # Validation
  if (!var_cons %in% names(data)) stop("Consumption variable not found in data.")
  if (!is.numeric(amount) || amount <= 0) stop("Transfer amount must be a positive number.")

  # Evaluate eligibility condition
  eligibles <- eval(condition, envir = data)
  if (!is.logical(eligibles)) stop("Condition must return a logical vector.")

  # Simulated consumption
  new_cons <- data[[var_cons]] + ifelse(eligibles, amount, 0)

  # Optional cost calculation
  if (return_cost) {
    if (is.null(var_poids)) stop("Weight variable must be specified to compute total cost.")
    if (!var_poids %in% names(data)) stop("Weight variable not found in data.")

    weights <- data[[var_poids]]
    total_cost <- sum(ifelse(eligibles, amount * weights, 0), na.rm = TRUE)

    return(list(cons_sim = new_cons, cost = total_cost))
  }

  return(new_cons)
}
