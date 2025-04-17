#' Plot Poverty Distribution by Group
#'
#' This function visualizes the distribution of poverty across groups,
#' using the output of `poverty_distribution()`. It can display either the
#' share of poor or the share of population by group.
#'
#' @param result A `data.frame` returned by the function `poverty_distribution()`.
#' @param indicator A character string specifying the indicator to plot:
#'   must be either `"share_of_poor"` or `"share_of_population"`.
#' @param save Logical. If `TRUE`, saves the plot as a PNG image. Default is `FALSE`.
#' @param filename A character string specifying the name of the PNG file if `save = TRUE`.
#'
#' @return A `ggplot` object representing the selected poverty distribution measure. The function also prints the plot to the viewer.
#'
#' @details
#' The first column of `result` is assumed to be the grouping variable (e.g. region, gender, etc.).
#' If the `Total` row is present in the result, it will be excluded from the plot.
#'
#' The plot is rendered as a horizontal bar chart with percentage labels directly on each bar.
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang sym
#' @export

plot_poverty_distribution <- function(result, indicator = "share_poor", save = FALSE, filename = "distribution_plot.png") {


  # Vérification de l'indicateur choisi
  if (!indicator %in% c("share_of_poor", "share_of_population")) {
    stop("L'indicateur doit etre 'share_of_poor' ou 'share_of_population'")
  }

  # Vérification de l'existence de la colonne
  if (!indicator %in% names(result)) {
    stop(paste("La colonne", indicator, "n'existe pas dans les resultats fournis."))
  }

  # Supprimer la ligne "Total" si elle est présente
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
    labs(title = paste("Poverty", gsub("_", " ", indicator), "by group"),
         x = names(result)[1], y = "Percentage (%)") +
    theme_minimal() +
    scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
    theme(legend.position = "none") +
    ylim(0, max(result[[indicator]], na.rm = TRUE) * 1.15)

  # Sauvegarde
  if (save) {
    ggsave(filename, plot, width = 8, height = 6)
  }

  print(plot)
}

