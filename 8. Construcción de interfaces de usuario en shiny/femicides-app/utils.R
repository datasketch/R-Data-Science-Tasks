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
        ),
        class = "card"
      )
    }
  )
}

make_modals <- function(df, input) {
  # df: data frame
  # input: input to filter
  # returns: list of modals
  total <- nrow(df)

  card_names <- paste0("card-", seq_len(total))

  map(
    seq_len(total),
    function(x) {
      case <- df[x, ]

      parsed_crimes <- gsub(
        pattern = "\\|",
        replacement = ", ",
        x = case$delito
      )

      parsed_modalities <- gsub(
        pattern = "\\|",
        replacement = ", ",
        x = case$modalidad
      )

      observeEvent(
        input[[card_names[x]]],
        {
          showModal(
            modalDialog(
              title = case$victima,
              HTML(
                glue(
                  "<div class='modal-body'>",
                  "<p class='modal-text'>",
                  "<b>Edad:</b> {case$edad}",
                  "</br>",
                  "<b>Fecha:</b> {case$fecha}",
                  "</br>",
                  "<b>Departamento:</b> {case$departamento}",
                  "</br>",
                  "<b>Municipio:</b> {case$municipio}",
                  "</br>",
                  "<b>Barrio:</b> {case$barrio}",
                  "</p>",
                  "<p class='modal-text'>",
                  "<b>Presunto agresor:</b> {case$presunto_agresor}",
                  "</br>",
                  "<b>Relación con la víctima:</b> {case$relacion_victima}",
                  "</br>",
                  "<b>Delito cometido:</b> {parsed_crimes}",
                  "</br>",
                  "<b>Modalidad:</b> {parsed_modalities}",
                  "</br>",
                  "</p>",
                  ifelse(
                    !is.na(case$comentario),
                    glue(
                      "<p class='modal-text'>",
                      "<b>Comentario:</b>",
                      "</br>",
                      "{case$comentario}",
                      "</p>",
                    ),
                    ""
                  ),
                  ifelse(
                    !is.na(case$fuente),
                    glue(
                      "<p class='modal-text'>",
                      "<b>Fuente:</b>",
                      "</br>",
                      "<a href='{case$fuente}' target='_blank'> {case$fuente} </a>",
                      "</p>",
                    ),
                    ""
                  ),
                  ifelse(
                    !is.na(case$fuente_2),
                    glue(
                      "<p class='modal-text'>",
                      "<b>Fuente 2:</b>",
                      "</br>",
                      "<a href='{case$fuente_2}' target='_blank'> {case$fuente_2} </a>",
                      "</p>",
                    ),
                    ""
                  ),
                  "</div>"
                )
              ),
              footer = modalButton(
                "Cerrar"
              ),
              size = "m",
              easyClose = TRUE
            )
          )
        }
      )
    }
  )
}

make_map <- function(df, departments) {
  # df: data frame
  # departments: departments to filter
  # returns: leaflet map
  df <- df |>
    filter_if_not_empty(
      column = departamento,
      input = departments
    ) |>
    filter(
      !is.na(longitud) & !is.na(latitud)
    )

  leaflet(df) |>
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
  df <- df |>
    filter(
      edad >= input[1] & edad <= input[2]
    ) |>
    group_by(
      age = edad
    ) |>
    summarise(
      n = n()
    )

  df |>
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
    df <- df |>
      group_by(
        month = month(fecha, label = TRUE)
      ) |>
      summarise(
        n = n()
      )

    df |>
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
    df <- df |>
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
      )

    df |>
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
