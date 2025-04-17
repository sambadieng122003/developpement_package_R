test_that("poverty_distribution returns expected columns and structure", {
  # Base fictive
  df <- data.frame(
    pcexp = c(100000, 200000, 150000, 300000, 120000, 50000),
    hhweight = c(1.5, 1.2, 1.8, 2.0, 1.3, 0.9),
    zref = rep(150000, 6),
    milieu = c("Urbain", "Rural", "Urbain", "Urbain", "Rural", "Rural")
  )

  # Appel de la fonction
  res <- poverty_distribution(
    data = df,
    separateur = "milieu",
    params = list(
      var_cons = "pcexp",
      var_poids = "hhweight",
      var_seuil = "zref"
    )
  )

  # Tests
  expect_s3_class(res, "data.frame")
  expect_true(all(c("poverty_headcount", "share_of_poor", "share_of_population") %in% names(res)))
  expect_true("Total" %in% res[[1]])
})
