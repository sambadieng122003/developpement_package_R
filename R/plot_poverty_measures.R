#' @importFrom stats reorder
#' @importFrom utils globalVariables
utils::globalVariables(c(".", "reorder"))

#' Plot Poverty Measures by Group
#'
#' This function generates a horizontal barplot of poverty indicators—**headcount ratio**, **poverty gap**, or **poverty severity**—
#' using the output of the `poverty_measures()` function. It displays the values on the bars and optionally saves the plot to a PNG file.
#'
#' @param result A `data.frame` returned by the `poverty_measures()` function. It must contain the columns `poverty_headcount`, `poverty_gap`, or `poverty_severity`.
#' @param indicator A character string indicating the poverty indicator to plot. Valid options are `"headcount"`, `"gap"`, or `"severity"`.
#' @param save Logical. If `TRUE`, the plot is saved as a PNG image. Default is `FALSE`.
#' @param filename Character. The name of the file to save the plot if `save = TRUE`. Default is `"poverty_plot.png"`.
#'
#' @return A `ggplot` object representing the selected poverty measure by group. The function also prints the plot to the viewer.
#'
#' @details
#' The first column of `result` is assumed to be the grouping variable (e.g. region, gender, etc.).
#' The function automatically removes the row labeled `"Total"` before plotting, if present.
#'
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang sym
#' @export

plot_poverty_measures <- function(result, indicator = "headcount", save = FALSE, filename = "poverty_plot.png") {


  # Vérification de l'indicateur choisi
  if (!indicator %in% c("headcount", "gap", "severity")) {
    stop("L'indicateur doit etre 'headcount', 'gap' ou 'severity'")
  }

  # Sélection de la colonne correspondant à l'indicateur choisi
  indicator_column <- switch(indicator,
                             "headcount" = "poverty_headcount",
                             "gap" = "poverty_gap",
                             "severity" = "poverty_severity")

  # Vérification de l'existence de la colonne
  if (!indicator_column %in% names(result)) {
    stop(paste("La colonne", indicator_column, "n'existe pas dans les resultats fournis."))
  }

  # Supprimer la ligne "Total" s'il y en a une
  if ("Total" %in% result[[1]]) {
    result <- result %>% filter(.[[1]] != "Total")
  }

  # Création du graphique avec ggplot2
  plot <- ggplot(result, aes(x = reorder(!!sym(names(result)[1]), !!sym(indicator_column)),
                             y = !!sym(indicator_column), fill = !!sym(indicator_column))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(!!sym(indicator_column), 1)),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = paste("Poverty", indicator, "by group"),
         x = names(result)[1], y = paste(indicator, "(%)")) +
    theme_minimal() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme(legend.position = "none") +
    ylim(0, max(result[[indicator_column]], na.rm = TRUE) * 1.15)

  # Sauvegarde optionnelle du graphique
  if (save) {
    ggsave(filename, plot, width = 8, height = 6)
  }

  # Affichage du graphique
  print(plot)
}

