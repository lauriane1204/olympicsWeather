library(testthat)
library(tibble)

perform_request <- function(lat, lon) {
  url <- "https://api.open-meteo.com/v1/forecast"
  response_table <-
    httr2::request(url) |>
    httr2::req_url_query(latitude = lat, longitude = lon,
                         hourly = c("temperature_2m",
                                    "apparent_temperature",
                                    "precipitation_probability",
                                    "precipitation"),
                         .multi = "comma") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    tibble::as_tibble()
  return(response_table)
}

unnest_data <- function(resp) {
  tibble::tibble(date_heure = unlist(resp$hourly[1][[1]]),
                 temperature_celsius = unlist(resp$hourly[2][[1]]),
                 apparent_temperature_celsius = unlist(resp$hourly[3][[1]]),
                 precipitation_proba = unlist(resp$hourly[4][[1]]),
                 precipitation = unlist(resp$hourly[5][[1]]))
}

resp <- perform_request(23.2,2.1)
result <- unnest_data(resp)

test_that("la fonction renvoie le bon nombre de lignes", {
  expect_equal(nrow(result), length(resp$hourly[[1]]))
})

test_that("les valeurs de la colonne temperature correspondent aux valeurs proposees en entree", {
  expect_equal(result$temperature_celsius, unlist(resp$hourly[[2]]))
})

test_that("test des noms des colonnes en sortie", {
  expect_equal(names(result), c("date_heure",
                                "temperature_celsius",
                                "apparent_temperature_celsius",
                                "precipitation_proba",
                                "precipitation"))
})

test_that("test du nombre de colonnes en sortie", {
  expect_equal(ncol(result), 5)
})
