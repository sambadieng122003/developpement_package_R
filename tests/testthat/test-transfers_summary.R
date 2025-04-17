test_that("transfers_summary returns expected structure", {
  df <- data.frame(
    pcexp = c(80000, 150000, 200000),
    hhweight = c(1, 1, 1),
    zref = rep(150000, 3),
    hage = c(3, 30, 70),
    milieu = c("Rural", "Urban", "Rural"),
    hhandig = c("Oui", "Non", "Non")
  )

  scenarios <- list(
    list(name = "Universal", condition = quote(TRUE), amount = 10000)
  )

  res <- transfers_summary(
    data = df,
    scenarios = scenarios,
    baseline = "pcexp",
    params = list(var_poids = "hhweight", var_seuil = "zref"),
    pib = 1e12,
    save_scenario = FALSE,
    plot = FALSE
  )

  expect_type(res, "list")
  expect_named(res, c("table", "plot_efficiency"))
  expect_s3_class(res$table, "data.frame")
  expect_s3_class(res$plot_efficiency, "gg")
})

