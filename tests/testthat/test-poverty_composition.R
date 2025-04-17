test_that("poverty_composition returns expected structure and values", {
  df <- data.frame(
    pcexp = c(100000, 200000, 150000, 300000, 120000, 50000),
    hhweight = c(1.5, 1.2, 1.8, 2.0, 1.3, 0.9),
    zref = rep(150000, 6),
    milieu = c("Urbain", "Rural", "Urbain", "Urbain", "Rural", "Rural")
  )

  res <- poverty_composition(
    data = df,
    separateur = "milieu",
    params = list(
      var_cons = "pcexp",
      var_poids = "hhweight",
      var_seuil = "zref"
    )
  )

  # Supprimer la ligne Total pour tester les contributions
  res_no_total <- res[res[[1]] != "Total", ]

  expect_s3_class(res, "data.frame")
  expect_true(all(c("contribution_to_headcount", "contribution_to_gap", "contribution_to_severity") %in% names(res)))
  expect_equal(round(sum(res_no_total$contribution_to_headcount, na.rm = TRUE), 1), 100)
  expect_equal(round(sum(res_no_total$contribution_to_gap, na.rm = TRUE), 1), 100)
  expect_equal(round(sum(res_no_total$contribution_to_severity, na.rm = TRUE), 1), 100)
})

