test_that("full_generate_report generates an Excel file successfully", {
  # Générer des données fictives minimales pour chaque entrée
  df <- data.frame(
    group = c("Rural", "Urban"),
    poverty_headcount = c(40, 25),
    poverty_gap = c(15, 8),
    poverty_severity = c(5, 3)
  )

  summary_list <- list(
    poverty_measures = df,
    poverty_distribution = df |>
      dplyr::mutate(share_of_poor = poverty_headcount, share_of_population = poverty_headcount),
    poverty_composition = df |>
      dplyr::mutate(
        contribution_to_headcount = poverty_headcount,
        contribution_to_gap = poverty_gap,
        contribution_to_severity = poverty_severity
      ),
    poverty_measures_plot = ggplot2::ggplot(df, ggplot2::aes(x = group, y = poverty_headcount)) + ggplot2::geom_col(),
    poverty_distribution_plot = ggplot2::ggplot(df, ggplot2::aes(x = group, y = poverty_gap)) + ggplot2::geom_col(),
    poverty_composition_plot = ggplot2::ggplot(df, ggplot2::aes(x = group, y = poverty_severity)) + ggplot2::geom_col()
  )

  results_transfer <- list(
    table = data.frame(
      id = "s1", scenario = "Universal", amount = 100000, headcount = 30,
      delta_poverty = 10, cost = 5000000, cost_perc_pib = 0.1, efficiency = 100
    ),
    plot_efficiency = ggplot2::ggplot(data.frame(id = "s1", efficiency = 100),
                                      ggplot2::aes(x = id, y = efficiency)) + ggplot2::geom_col()
  )

  inequality_df <- data.frame(
    group = c("Rural", "Urban"),
    gini = c(35, 28),
    theil = c(20, 14)
  )

  lorenz_plots <- list(
    Rural = ggplot2::ggplot(data.frame(x = c(0, 0.5, 1), y = c(0, 0.3, 1)),
                            ggplot2::aes(x = x, y = y)) + ggplot2::geom_line()
  )

  population <- ggplot2::ggplot(data.frame(x = c("Rural", "Urban"), y = c(60, 40)),
                                ggplot2::aes(x = x, y = y)) + ggplot2::geom_col()

  outfile <- tempfile(fileext = ".xlsx")

  # Appeler la fonction
  expect_error(
    full_generate_report(
      summary_list = summary_list,
      transfer_results = results_transfer,
      inequality_df = inequality_df,
      lorenz_plots = lorenz_plots,
      population = population,
      file = outfile
    ),
    NA
  )

  # Vérifier que le fichier a bien été créé
  expect_true(file.exists(outfile))

  # Supprimer le fichier généré
  unlink(outfile)
})
