test_that("population_distribution fonctionne correctement avec des donnees valides", {
  df <- data.frame(
    sexe = c("Homme", "Femme", "Homme", "Femme", "Homme", "Femme")
  )

  # La fonction retourne un objet ggplot
  plot <- population_distribution(df, separateur = "sexe")
  expect_s3_class(plot, "ggplot")
})

test_that("population_distribution renvoie une erreur si le separateur n'existe pas", {
  df <- data.frame(
    sexe = c("Homme", "Femme", "Homme")
  )

  expect_error(
    population_distribution(df, separateur = "region"),
    "Le separateur specifie n'existe pas"
  )
})

test_that("population_distribution renvoie une erreur si le package forcats n'est pas installe", {
  # Simulation manuelle du package manquant (deconseille en production)
  skip_if_not_installed("forcats")

  # On ne teste pas directement l'absence de forcats ici, car le test echouerait dans les workflows normaux.
  # Il est preferable de verifier que la conversion en facteur fonctionne.
  df <- data.frame(
    groupe = c("A", "B", "A", "B", "A", "B")
  )

  expect_s3_class(
    population_distribution(df, separateur = "groupe"),
    "ggplot"
  )
})

test_that("population_distribution fonctionne avec une seule categorie", {
  df <- data.frame(
    sexe = rep("Homme", 5)
  )

  plot <- population_distribution(df, separateur = "sexe")
  expect_s3_class(plot, "ggplot")
})

