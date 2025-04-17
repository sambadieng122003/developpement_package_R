test_that("poverty_summary returns a named list of results", {
  # Base fictive
  df <- data.frame(
    pcexp = c(100000, 200000, 150000, 300000, 120000, 50000),
    hhweight = c(1.5, 1.2, 1.8, 2.0, 1.3, 0.9),
    zref = rep(150000, 6),
    milieu = c("Urbain", "Rural", "Urbain", "Urbain", "Rural", "Rural")
  )

  # Appel de la fonction
  res <- poverty_summary(
    data = df,
    separateur = "milieu",
    params = list(
      var_cons = "pcexp",
      var_poids = "hhweight",
      var_seuil = "zref"
    )
  )

  # Tests
  expect_type(res, "list")
  expect_named(res, c("poverty_measures", "poverty_distribution", "poverty_composition"))
  expect_s3_class(res$poverty_measures, "data.frame")
  expect_s3_class(res$poverty_distribution, "data.frame")
  expect_s3_class(res$poverty_composition, "data.frame")
})
