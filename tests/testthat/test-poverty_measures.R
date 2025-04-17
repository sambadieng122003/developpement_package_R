test_that("poverty_measures returns correct column names", {
  df <- data.frame(
    pcexp = c(100000, 150000, 50000),
    hhweight = c(1.5, 2, 1),
    zref = c(150000, 150000, 150000)
  )

  res <- poverty_measures(
    data = df,
    separateur = NULL,
    params = list(
      var_cons = "pcexp",
      var_poids = "hhweight",
      var_seuil = "zref"
    )
  )

  expect_true(all(c("poverty_headcount", "poverty_gap", "poverty_severity") %in% names(res)))
})
