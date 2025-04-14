test_that("inequality_indices retourne un data.frame avec gini et theil", {
  df <- data.frame(
    revenu = c(100, 200, 300, 400, 1000, 2000),
    sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme")
  )

  result <- inequality_indices(df, variable = "revenu")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("gini", "theil") %in% names(result)))
  expect_true(all(result$gini >= 0 & result$gini <= 100))  # mis à jour
  expect_true(all(result$theil >= 0))
})

test_that("inequality_indices fonctionne avec separateur", {
  df <- data.frame(
    revenu = c(100, 200, 300, 400, 1000, 2000),
    sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme")
  )

  result <- inequality_indices(df, variable = "revenu", separateur = "sexe")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("gini", "theil") %in% names(result)))
  expect_true("sexe" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_true(all(result$gini >= 0 & result$gini <= 100))  # mis à jour
  expect_true(all(result$theil >= 0))
})

test_that("inequality_indices retourne NA pour des donnees insuffisantes", {
  df <- data.frame(
    revenu = c(NA, 100),
    sexe = c("Homme", "Homme")
  )

  result <- inequality_indices(df, variable = "revenu", separateur = "sexe")
  expect_true(is.na(result$gini[1]))
  expect_true(is.na(result$theil[1]))
})

test_that("inequality_indices declenche une erreur si la variable est invalide", {
  df <- data.frame(revenu = c(100, 200))

  expect_error(
    inequality_indices(df, variable = "invalide"),
    "La variable specifiee n'existe pas"
  )
})

test_that("inequality_indices declenche une erreur si le separateur est invalide", {
  df <- data.frame(revenu = c(100, 200))

  expect_error(
    inequality_indices(df, variable = "revenu", separateur = "region"),
    "Le separateur specifie n'existe pas"
  )
})
