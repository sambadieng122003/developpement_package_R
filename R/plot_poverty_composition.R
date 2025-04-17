#' @importFrom stats reorder
utils::globalVariables(c("."))

#' Plot Poverty Distribution by Group
#'
#' Visualizes the contribution of each group to the national poverty indicator.
#' Values are expressed in percentage and should sum to 100%.
#'
#' @param result A data.frame returned by `poverty_composition()`
#' @param indicator A character string: "contribution_to_headcount", "contribution_to_gap", or "contribution_to_severity"
#' @param save Logical. If TRUE, saves the plot as a PNG file.
#' @param filename Character. Name of the file if `save = TRUE`
#'
#' @return A ggplot object showing the contribution of each group.
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang sym
#' @export



plot_poverty_composition <- function(result, indicator = "contribution_to_headcount", save = FALSE, filename = "composition_plot.png") {


  # Vérification de l'indicateur choisi
  if (!indicator %in% c("contribution_to_headcount", "contribution_to_gap", "contribution_to_severity")) {
    stop("Lindicateur doit etre 'contribution_to_headcount', 'contribution_to_gap', ou 'contribution_to_severity'")
  }

  # Vérification de l'existence de la colonne
  if (!indicator %in% names(result)) {
    stop(paste("La colonne", indicator, "n existe pas dans les resultats fournis."))
  }

  # Supprimer la ligne "Total" si elle existe
  if ("Total" %in% result[[1]]) {
    result <- result %>% filter(.[[1]] != "Total")
  }

  # Création du graphique
  plot <- ggplot(result, aes(x = reorder(!!sym(names(result)[1]), !!sym(indicator)),
                             y = !!sym(indicator), fill = !!sym(indicator))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(!!sym(indicator), 1), "%")),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = paste("Contribution to", gsub("_", " ", indicator)),
         x = names(result)[1], y = "Contribution (%)") +
    theme_minimal() +
    scale_fill_gradient(low = "lightcoral", high = "darkred") +
    theme(legend.position = "none") +
    ylim(0, max(result[[indicator]], na.rm = TRUE) * 1.15)

  # Sauvegarde
  if (save) {
    ggsave(filename, plot, width = 8, height = 6)
  }

  print(plot)
}
