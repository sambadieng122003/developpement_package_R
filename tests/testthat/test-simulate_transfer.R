test_that("simulate_transfer returns correct vector and cost", {
  df <- data.frame(
    pcexp = c(100000, 200000, 150000, 50000, 120000),
    hhweight = c(1.2, 1.5, 1.0, 0.8, 1.1),
    milieu = c("Rural", "Rural", "Urbain", "Rural", "Urbain")
  )

  # Cas simple : transfert universel sans coût
  result1 <- simulate_transfer(
    data = df,
    condition = quote(TRUE),
    amount = 10000,
    var_cons = "pcexp"
  )

  expect_type(result1, "double")
  expect_length(result1, nrow(df))
  expect_equal(result1, df$pcexp + 10000)

  # Cas : transfert avec coût calculé
  result2 <- simulate_transfer(
    data = df,
    condition = quote(milieu == "Rural"),
    amount = 20000,
    var_cons = "pcexp",
    var_poids = "hhweight",
    return_cost = TRUE
  )

  expect_type(result2, "list")
  expect_named(result2, c("cons_sim", "cost"))
  expect_length(result2$cons_sim, nrow(df))

  # Vérifier le montant total du coût (3 ruraux : obs 1, 2, 4)
  expected_cost <- sum(c(1.2, 1.5, 0.8) * 20000)
  expect_equal(round(result2$cost, 1), round(expected_cost, 1))
})
