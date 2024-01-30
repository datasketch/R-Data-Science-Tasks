library(glue)
library(dplyr)
library(purrr)
library(shiny)
library(leaflet)
library(stringr)
library(lubridate)
library(highcharter)

filter_if_not_empty <- function(df, column, input) {
  # df: data frame
  # col: column to filter
  # input: input to filter
  # returns: filtered data frame
  if (length(input) > 0) {
    df |>
      filter(
        {{ column }} %in% input
      )
  } else {
    df
  }
}

filter_if_not_empty_str <- function(df, column, input) {
  # df: data frame
  # col: column to filter
  # input: input to filter
  # returns: filtered data frame
  if (input != "") {
    df |>
      filter(
        str_detect(
          string = tolower({{ column }}),
          pattern = tolower(input)
        )
      )
  } else {
    df
  }
}

filter_df <- function(df, input) {
  # df: data frame
  # input: input to filter
  # returns: filtered data frame
  df |>
    filter(
      fecha >= input$date[1] & fecha <= input$date[2],
      edad >= input$age[1] & edad <= input$age[2]
    ) |>
    filter_if_not_empty_str(
      column = victima,
      input = input$victim
    ) |>
    filter_if_not_empty(
      column = departamento,
      input = input$department
    ) |>
    filter_if_not_empty(
      column = municipio,
      input = input$municipality
    ) |>
    filter_if_not_empty(
      column = barrio,
      input = input$neighborhood
    ) |>
    filter_if_not_empty(
      column = relacion_victima,
      input = input$relationship
    )
}

make_cards <- function(df) {
  # df: data frame
  # returns: list of action buttons (cards)
  total <- nrow(df)

  card_names <- paste0("card-", seq_len(total))

  map(
    seq_len(total),
    function(x) {
      case <- df[x, ]
      
      actionButton(
        inputId = card_names[x],
        label = HTML(
          glue(
            "<div class='card'>",
            "<div class='card-body'>",
            "<h4 class='card-title'>",
            "{case$victima}",
            "</h4>",
            "<p class='card-text'>",
            "Edad: {case$edad}",
            "</br>",
            "Fecha: {case$fecha}",
            "</br>",
            "Departamento: {case$departamento}",
            "</br>",
            "Municipio: {case$municipio}",
            "</br>",
            "Barrio: {case$barrio}",
            "</p>",
            "</div>",
            "</div>"
          )
        )
      )
    }
  )
}

make_map <- function(df, departments) {
  # df: data frame
  # departments: departments to filter
  # returns: leaflet map
  df |>
    filter_if_not_empty(
      column = departamento,
      input = departments
    ) |>
    filter(
      !is.na(longitud) & !is.na(latitud)
    ) |>
    leaflet() |>
    addTiles() |>
    addMarkers(
      lng = ~ longitud,
      lat = ~ latitud,
      popup = ~ glue(
        "<div class='popup'>",
        "<div class='popup-body'>",
        "<h4 class='popup-title'>",
        "{victima}",
        "</h4>",
        "<p class='popup-text'>",
        "Edad: {edad}",
        "</br>",
        "Fecha: {fecha}",
        "</br>",
        "Departamento: {departamento}",
        "</br>",
        "Municipio: {municipio}",
        "</br>",
        "Barrio: {barrio}",
        "</p>",
        "</div>",
        "</div>"
      )
    )
}

plot_by_age <- function(df, input) {
  # df: data frame
  # input: input to plot
  # returns: plot
  df |>
    filter(
      edad >= input[1] & edad <= input[2]
    ) |>
    group_by(
      age = edad
    ) |>
    summarise(
      n = n()
    ) |>
    hchart(
      type = "column",
      hcaes(
        x = age,
        y = n
      )
    ) |>
    hc_title(
      text = "Número de feminicidios por edad"
    ) |>
    hc_xAxis(
      title = list(
        text = "Edad"
      )
    ) |>
    hc_yAxis(
      title = list(
        text = "Número de feminicidios"
      )
    )
}

plot_by_month <- function(df, column, input) {
  # df: data frame
  # column: column to plot
  # input: input to plot
  # returns: highcharts plot
  if (length(input) == 0) {
    df |>
      group_by(
        month = month(fecha, label = TRUE)
      ) |>
      summarise(
        n = n()
      ) |>
      hchart(
        type = "column",
        hcaes(
          x = month,
          y = n
        )
      ) |>
      hc_title(
        text = "Número de feminicidios por mes"
      ) |>
      hc_xAxis(
        title = list(
          text = "Mes"
        )
      ) |>
      hc_yAxis(
        title = list(
          text = "Número de feminicidios"
        )
      )
  } else {
    df |>
      filter(
        {{ column }} %in% input
      ) |>
      group_by(
        group_column = {{ column }},
        month = month(fecha, label = TRUE)
      ) |>
      summarise(
        n = n(),
        .groups = "drop"
      ) |>
      hchart(
        type = "column",
        hcaes(
          x = month,
          y = n,
          group = group_column
        )
      ) |>
      hc_title(
        text = "Número de feminicidios por mes"
      ) |>
      hc_xAxis(
        title = list(
          text = "Mes"
        )
      ) |>
      hc_yAxis(
        title = list(
          text = "Número de feminicidios"
        )
      ) |>
      hc_plotOptions(
        column = list(
          stacking = "normal"
        )
      ) |>
      hc_legend(
        enabled = TRUE
      )
  }
}
