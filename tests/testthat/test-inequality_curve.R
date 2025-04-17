test_that("inequality_curve retourne un graphique ggplot sans separateur", {
  df <- data.frame(
    revenu = c(100, 200, 300, 400, 1000, 2000)
  )

  result <- inequality_curve(df, variable = "revenu")
  expect_s3_class(result, "ggplot")
})

test_that("inequality_curve retourne une liste de graphiques avec separateur", {
  df <- data.frame(
    revenu = c(100, 200, 300, 400, 1000, 2000),
    sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme")
  )

  result <- inequality_curve(df, variable = "revenu", separateur = "sexe")

  expect_type(result, "list")
  expect_true(all(sapply(result, inherits, what = "ggplot")))
  expect_equal(length(result), 2)
})

test_that("inequality_curve gère les donnees insuffisantes", {
  df <- data.frame(
    revenu = c(100),
    sexe = c("Homme")
  )

  result <- inequality_curve(df, variable = "revenu", separateur = "sexe")
  expect_equal(result, list()) # Aucune courbe generee
})

test_that("inequality_curve renvoie une erreur pour variable inexistante", {
  df <- data.frame(revenu = c(100, 200))

  expect_error(
    inequality_curve(df, variable = "invalide"),
    "La variable specifiee n'existe pas"
  )
})

test_that("inequality_curve renvoie une erreur pour separateur inexistant", {
  df <- data.frame(revenu = c(100, 200))

  expect_error(
    inequality_curve(df, variable = "revenu", separateur = "region"),
    "Le separateur specifie n'existe pas"
  )
})

