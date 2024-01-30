library(dplyr)
library(readr)
library(shiny)
library(leaflet)
library(highcharter)

source("utils.R")

data <- read_csv(
  "data/feminicidios.csv",
  show_col_types = FALSE,
)

ui <- navbarPage(
  "Feminicidios en Colombia 2017",
  # Filter femicides by different variables
  tabPanel(
    "Casos",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(
          inputId = "date",
          label = "Rango de fechas",
          start = min(data$fecha),
          end = max(data$fecha),
          min = min(data$fecha),
          max = max(data$fecha),
          language = "es",
          separator = " - ",
        ),
        textInput(
          inputId = "victim",
          label = "Nombre de la víctima",
          value = NULL
        ),
        sliderInput(
          inputId = "age",
          label = "Edad de la víctima",
          min = min(data$edad, na.rm = TRUE),
          max = max(data$edad, na.rm = TRUE),
          value = c(
            min(data$edad, na.rm = TRUE),
            max(data$edad, na.rm = TRUE)
          ),
          step = 1
        ),
        selectInput(
          inputId = "department",
          label = "Departamento",
          choices = sort(unique(data$departamento)),
          selected = NULL,
          multiple = TRUE
        ),
        uiOutput("municipality"),
        uiOutput("neighborhood"),
        selectInput(
          inputId = "relationship",
          label = "Relación con la víctima",
          choices = sort(unique(data$relacion_victima)),
          selected = NULL,
          multiple = TRUE
        )
      ),

      mainPanel(
        tabsetPanel(
          tabPanel(
            "Tarjetas",
            uiOutput("cards")
          ),
          tabPanel(
            "Tabla",
            tableOutput("table")
          )
        )
      )
    )
  ),
  # Map femicides by department
  tabPanel(
    "Mapa",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "department_map",
          label = "Departamento",
          choices = sort(unique(data$departamento)),
          selected = NULL,
          multiple = TRUE
        )
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  # Graph femicides by different variables
  navbarMenu(
    "Gráficas",
    tabPanel(
      "Edad",
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "age_plot",
            label = "Edad de la víctima",
            min = min(data$edad, na.rm = TRUE),
            max = max(data$edad, na.rm = TRUE),
            value = c(
              min(data$edad, na.rm = TRUE),
              max(data$edad, na.rm = TRUE)
            ),
            step = 1
          )
        ),
        mainPanel(
          highchartOutput("femicides_by_age")
        )
      )
    ),
    tabPanel(
      "Departamento",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "department_plot",
            label = "Departamento",
            choices = sort(unique(data$departamento)),
            selected = NULL,
            multiple = TRUE
          )
        ),
        mainPanel(
          highchartOutput("femicides_by_department")
        )
      )
    ),
    tabPanel(
      "Relación",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "relationship_plot",
            label = "Relación con la víctima",
            choices = sort(unique(data$relacion_victima)),
            selected = NULL,
            multiple = TRUE
          )
        ),
        mainPanel(
          highchartOutput("femicides_by_relationship")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # --------------------

  # Get a filtered data frame by selected departments
  filtered_by_departments <- reactive({
    data |>
      filter(
        departamento %in% input$department,
      )
  })

  # Create a select input for municipalities based on selected departments
  observeEvent(input$department, {
    output$municipality <- renderUI({
      if (length(input$department) == 1) {
        choices <- sort(
          unique(filtered_by_departments()$municipio)
        )

        selectInput(
          inputId = "municipality",
          label = "Municipio",
          choices = choices,
          selected = NULL,
          multiple = TRUE
        )
      }
    })

    output$neighborhood <- renderUI({
      NULL
    })
  })

  # Get a filtered data frame by selected municipalities
  filtered_by_municipalities <- reactive({
    filtered_by_departments() |>
      filter(
        municipio %in% input$municipality,
      )
  })

  # Create a select input for neighborhoods based on selected municipalities
  observeEvent(input$municipality, {
    output$neighborhood <- renderUI({
      if (length(input$municipality) == 1) {
        choices <- sort(
          unique(filtered_by_municipalities()$barrio)
        )

        if (length(choices) > 0) {
          selectInput(
            inputId = "neighborhood",
            label = "Barrio",
            choices = choices,
            selected = NULL,
            multiple = TRUE
          )
        }
      }
    })
  })

  # Filter data frame by selected inputs
  filtered_df <- reactive({
    data |>
      filter_df(
        input = input
      )
  })

  # Render cards
  output$cards <- renderUI({
    make_cards(filtered_df())
  })

  # Render table
  output$table <- renderTable({
    filtered_df()
  })

  # --------------------
  
  # Render map
  output$map <- renderLeaflet({
    filtered_df() |>
      make_map(departments = input$department_map)
  })
  

  # --------------------

  # Render graphs
  output$femicides_by_age <- renderHighchart({
    data |>
      plot_by_age(
        input = input$age_plot
      )
  })

  output$femicides_by_department <- renderHighchart({
    data |>
      plot_by_month(
        column = departamento,
        input = input$department_plot
      )
  })

  output$femicides_by_relationship <- renderHighchart({
    data |>
      plot_by_month(
        column = relacion_victima,
        input = input$relationship_plot
      )
  })
}

shinyApp(ui = ui, server = server)
