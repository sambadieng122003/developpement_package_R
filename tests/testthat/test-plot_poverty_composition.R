test_that("plot_poverty_composition returns a ggplot object", {
  welfare <- data.frame(
    milieu = c("Rural", "Urbain", "Rural", "Urbain"),
    pcexp = c(80000, 120000, 70000, 150000),
    hhweight = c(1.2, 0.8, 1.5, 1.0),
    zref = rep(100000, 4)
  )

  result <- poverty_composition(
    data = welfare,
    separateur = c("milieu"),
    params = list(var_cons = "pcexp", var_poids = "hhweight", var_seuil = "zref")
  )

  p <- plot_poverty_composition(result, indicator = "contribution_to_headcount")

  expect_s3_class(p, "gg")
})
