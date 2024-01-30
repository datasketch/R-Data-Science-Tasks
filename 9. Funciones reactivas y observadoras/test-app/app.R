library(shiny)

ui <- fluidPage(
    titlePanel("Funciones reactivas y observadoras"),

    sidebarLayout(
      sidebarPanel(
        numericInput("num", "Introduce un número", 0),
        actionButton("incrementar", "Incrementar"),
        actionButton("actualizar", "Actualizar"),
        textInput("input1", "Introduce un texto", ""),
        textInput("input2", "Introduce otro texto", ""),
        actionButton("boton", "Botón"),
        textInput("otroInput", "Introduce otro texto", "")
      ),
      mainPanel(
        h4("Bloque 1"),
        textOutput("value"),
        h4("Bloque 3"),
        textOutput("valorContador"),
        h4("Bloque 4"),
        textOutput("resultadoA"),
        textOutput("resultadoB"),
        h4("Bloque 5"),
        uiOutput("bloque5")
      )
    )
)

server <- function(input, output) {
  # Bloque 1
  # ¿Qué hace este bloque?
  #   R/ Crea una variable reactiva que se actualiza cada vez que cambia el
  #   valor de input$num. El valor de la variable reactiva es el doble del valor
  #   de input$num. Posteriormente, se renderiza el valor de la variable reactiva
  #   en la UI.
  #
  # ¿Cómo se relaciona con la reactividad/observación?
  #   R/ La variable reactiva se actualiza cada vez que cambia el valor de
  #   input$num. El valor de la variable reactiva se renderiza en la UI.
  resultado <- reactive({
    input$num * 2
  })
  
  output$value <- renderText({
    # resultado()
    paste("El doble de", input$num, "es", resultado())
  })
  
  # Bloque 2
  # ¿Qué hace este bloque?
  #   R/ Crea un observador que se activa cada vez que cambia el valor de
  #   input$num e imprime el valor de input$num en la consola.
  #
  # ¿Cómo se relaciona con la reactividad/observación?
  #   R/ El observador está "observando" el valor de input$num, por lo que
  #   cada vez que este cambia, se ejecuta el código dentro del observador
  #   (en este caso, imprimir el valor de input$num en la consola).
  observe({
    # print(input$num)
    # Otras acciones que no devuelven un valor directamente a la UI
    print("Bloque 2")
    print(paste("El valor de input$num es:", input$num))
    print(paste("El valor de resultado() es:", resultado()))
  })
  
  # Bloque 3
  # ¿Qué hace este bloque?
  #   R/ Mediante un reactiveVal, se crea una variable reactiva llamada
  #   contador (con valor inicial 0), y mediante un observeEvent, se
  #   incrementa el valor de contador en 1 cada vez que se hace clic en
  #   el botón 'input$incrementar'. Posteriormente, se renderiza el valor
  #   de contador en la UI.
  #
  # ¿Cómo se relaciona con la reactividad/observación?
  #   R/ El reactiveVal crea una variable reactiva que no depende directamente
  #   de ningún input, pero va a ser actualizada mediante un observeEvent cada
  #   vez que se haga clic en el botón 'input$incrementar'
  contador <- reactiveVal(0)
  
  observeEvent(input$incrementar, {
    contador(contador() + 1)
  })
  
  output$valorContador <- renderText({
    # contador()
    paste("El valor del contador es", contador())
  })
  
  # Bloque 4
  # ¿Qué hace este bloque?
  #   R/ Mediante un reactiveValues, se crean dos variables reactivas llamadas
  #   a y b (con valores iniciales 10 y 20, respectivamente), y mediante un
  #   observeEvent, se actualizan los valores de a y b (siguiendo las
  #   operaciones a <- a * 2 y b <- b + 5) cada vez que se hace clic en el
  #   botón 'input$actualizar'. Posteriormente, se renderizan los valores de
  #   a y b en la UI.
  #
  # ¿Cómo se relaciona con la reactividad/observación?
  #   R/ El reactiveValues crea dos variables reactivas que no dependen
  #   directamente de ningún input, pero van a ser actualizadas mediante un
  #   observeEvent cada vez que se haga clic en el botón 'input$actualizar'.
  valores <- reactiveValues(a = 10, b = 20)
  
  observeEvent(input$actualizar, {
    valores$a <- valores$a * 2
    valores$b <- valores$b + 5
  })
  
  output$resultadoA <- renderText({
    # valores$a
    paste("El valor de a es", valores$a)
  })
  
  output$resultadoB <- renderText({
    # valores$b
    paste("El valor de b es", valores$b)
  })
  
  # Bloque 5
  # ¿Qué hace este bloque?
  #   R/ Crea un observador que se activa cada vez que cambia el valor de
  #   input$input1 o input$input2 e imprime el valor de ambos inputs en la
  #   consola.
  #
  # ¿Cómo se relaciona con la reactividad/observación?
  #   R/ Dado que dentro del observador se encuentran los inputs input$input1
  #   e input$input2, el observador se activará cada vez que cambie el valor
  #   de cualquiera de estos inputs y ejecutará el código dentro del mismo.
  observe({
    # Este código se ejecutará cada vez que cualquiera de estos inputs cambie
    input$input1
    input$input2
    # Algún efecto secundario, como actualizar un valor reactivo o la UI
    print("Bloque 5")
    print(paste("Los valores son:", input$input1, "y", input$input2))
    output$bloque5 <- renderUI({
      h4("Bloque 5")
      p(paste("Los valores son:", input$input1, "y", input$input2))
    })
  })
  
  # Bloque 6
  # ¿Qué hace este bloque?
  #   R/ A partir de un observeEvent, se "observa" el valor de input$boton,
  #   es decir, cada vez que se haga clic en el botón 'input$boton', se
  #   ejecutará el código dentro del observeEvent. En este caso, se imprime
  #   el valor de input$otroInput en la consola.
  #
  # ¿Cómo se relaciona con la reactividad/observación?
  #   R/ El observeEvent está "observando" el valor de input$boton, por lo que
  #   cada vez que este cambia, se ejecuta el código dentro del observeEvent,
  #   sin embargo, a pesar de que el valor de input$otroInput puede cambiar,
  #   el código dentro del observeEvent no se ejecutará a menos que se haga
  #   clic en el botón 'input$boton'.
  observeEvent(input$boton, {
    # Este código se ejecutará solo cuando se haga clic en 'input$boton'
    # Puede depender de otros inputs, pero solo se activa con el evento del botón
    print("Bloque 6")
    print(paste("El valor del input es:", input$otroInput))
  })
}

shinyApp(ui = ui, server = server)
