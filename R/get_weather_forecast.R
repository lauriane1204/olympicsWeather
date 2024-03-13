#'data meteo
#'
#'@description permet d obtenir des donnes de previsions meteorologiques en fonction de la lat. et de la long. ou d une adresse
#'
#' @param x Latitude (num) ou adresse (string)
#' @param y Longitude (num) (Obligatoire uniquement si x est la latitude).
#'
#' @return Un tibble contenant des donnees de previsions meteorologiques
#'
#' @export
#'
#' @import tibble
#' @import jsonlite
#' @import httr2
#' @import tidygeocoder
#' @import dplyr
#' @import devtools
#'
#' @examples
#' get_forecast(26.80, 4.83)
#' get_forecast("nantes")


get_forecast <- function(x, y = NULL) {

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


  get_gps_coordonnee <- function(adresse) {
    adresse_tibble <- tibble::tribble(
      ~addr,
      adresse
    ) |>
      tidygeocoder::geocode(addr)

    lat <- adresse_tibble$lat
    long <- adresse_tibble$long

    coordonnees <- c(lat, long)
    return(coordonnees)
  }



  if (!is.null(y)) {

    lat <- x
    lon <- y
    response <- perform_request(lat, lon)
  } else if (is.character(x)) {

    coordonnees <- get_gps_coordonnee(x)
    lat <- coordonnees[1]
    lon <- coordonnees[2]
    response <- perform_request(lat, lon)
  } else {
    stop("erreur")
  }

  data <- unnest_data(response)
  return(data)
}




#VISUALISATION JOUR

#' Visualisation JOUR
#'
#' @description Cette fonction permet de visualiser les donnees de previsions meteorologiques par jours en fonction de la latitude et de la longitude ou d une adresse
#'
#' @param x Latitude (num) ou adresse (string).
#' @param y Longitude (num) (Obligatoire uniquement si x est la latitude).
#'
#' @return Des graphiques ggplot.
#'
#' @export
#' @import tibble
#' @import jsonlite
#' @import httr2
#' @import tidygeocoder
#' @import dplyr
#' @import devtools
#' @import ggplot2
#' @import tidyr
#'
#' @examples
#' visualiser_forecast_d(26.80, 4.83)
#' visualiser_forecast_d("nantes")

visualiser_forecast_d <- function(x, y = NULL) {
  get_gps_coordonnee <- function(adresse) {
    adresse_tibble <- tibble::tribble(
      ~addr,
      adresse
    ) |>
      tidygeocoder::geocode(addr)

    lat <- adresse_tibble$lat
    long <- adresse_tibble$long

    coordonnees <- c(lat, long)
    return(coordonnees)
  }

  if (is.numeric(x) && is.numeric(y)) {

    data <- get_forecast(x, y)
  } else if (is.character(x)) {

    coordonnees <- get_gps_coordonnee(x)
    data <- get_forecast(coordonnees[1], coordonnees[2])
  } else {
    stop("Les arguments ne sont ni des coordonnees numeriques ni une adresse valide.")
  }

  data_long <- tidyr::pivot_longer(data, cols = c(temperature_celsius, apparent_temperature_celsius, precipitation_proba, precipitation), names_to = "variable", values_to = "value")

  data_long$date_heure <- as.POSIXct(data_long$date_heure, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  data_long$date <- as.Date(data_long$date_heure)
  data_long$day_of_week <- weekdays(data_long$date)

  data_aggregated <- data_long |>
    group_by(date, day_of_week, variable) |>
    summarise(value = mean(value, na.rm = TRUE))

  ggplot2::ggplot(data_aggregated, aes(x = paste(date, day_of_week), y = value, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::geom_text(aes(label = round(value, 1)), position = position_stack(vjust = 0.5), size = 3) +
    ggplot2::labs(title = "Previsions Meteo",
                  x = "Date et Jour",
                  y = "Valeur") +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



#Visualisation heure

#VISUALISATION PAR HEURE

#' Visualisation METEO heure
#'
#' Cette fonction permet de visualiser les donnees de previsions meteorologiques par heures en fonction de la latitude et de la longitude ou d une adresse.
#'
#' @param x Latitude (num) ou adresse (string).
#' @param y Longitude (num) (Obligatoire uniquement si x est la latitude).
#'
#' @return Des graphiques ggplot
#'
#' @export
#' @import tibble
#' @import jsonlite
#' @import httr2
#' @import tidygeocoder
#' @import dplyr
#' @import devtools
#' @import ggplot2
#' @import tidyr
#'
#' @examples
#' visualiser_forecast_h(42., 2.65)
#' visualiser_forecast_h("Nantes")

visualiser_forecast_h <- function(x, y = NULL) {
  get_gps_coordonnee <- function(adresse) {
    adresse_tibble <- tibble::tribble(
      ~addr,
      adresse
    ) |>
      tidygeocoder::geocode(addr)

    lat <- adresse_tibble$lat
    long <- adresse_tibble$long

    coordonnees <- c(lat, long)
    return(coordonnees)
  }

  if (is.numeric(x) && is.numeric(y)) {

    data <- get_forecast(x, y)
  } else if (is.character(x)) {

    coordonnees <- get_gps_coordonnee(x)
    data <- get_forecast(coordonnees[1], coordonnees[2])
  } else {
    stop("Les arguments ne sont ni des coordonnees numeriques ni une adresse valide.")
  }

  data_long <- tidyr::pivot_longer(data, cols = c(temperature_celsius, apparent_temperature_celsius, precipitation_proba, precipitation), names_to = "variable", values_to = "value")

  indices_a_afficher <- c(1, 97, 194, 291, 388, 481, 577)
  breaks_labels <- data_long |>
    dplyr::slice(indices_a_afficher) |>
    dplyr::pull(date_heure)

  ggplot2::ggplot(data_long, ggplot2::aes(x = date_heure, y = value, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::labs(title = "Previsions Meteo",
                  x = "Date et Heure",
                  y = "Valeur") +
    ggplot2::scale_x_discrete(breaks = breaks_labels, labels = breaks_labels) +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
    ggplot2::theme_minimal()
}
