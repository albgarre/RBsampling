#' F8 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_F8_ui <- function(id) {
  ns <- NS(id)
  tagList(

    layout_columns(
      col_widths = c(6, 6),
      layout_columns(
        col_widths = 12,

        ## Parameter delta

        popover(
          numericInput(ns("d"),
                       "Precisión absoluta deseada (unidades de la medida)",
                       2),
          "",
          "Se recomienda..."
        ),

        ## Parameter s

        popover(
          numericInput(ns("s"), "Desv. standard esperada de la concentración (unidades de la medida)",
                       1.5),
          "",
          "Basado en datos históricos"
        ),

        ## Parameter alpha

        popover(
          numericInput(ns("conf"), "Nivel de confianza (%)",
                       95),
          "",
          "Como norma general, 0.95"
        ),

        ## Correccion por poblacion finita

        hr(),
        "Si se conoce el tamaño de la población, se puede obtener un misma confianza estadística con un tamaño muestral menor.",

        checkboxInput(ns("use_PPF"),
                      "¿Usar corrección por población finita?"
        ),

        ## Parameter N

        conditionalPanel(
          ns = ns,
          condition = "input.use_PPF",
          popover(
            numericInput(ns("big_N"), "Tamaño de la población",
                         1000),
            "",
            "Basado en el censo disponible"
          )
        )

      ),
      layout_columns(
        col_widths = 12,
        uiOutput(ns("F1_outcome"))
      )


    )

  )
}

#' F8 Server Functions
#'
#' @noRd
mod_F8_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$F1_outcome <- renderUI({

      ## Get the value of Z

      alpha <- 1 - input$conf/100
      Z <- qnorm(1 - alpha/2)

      ## Get N

      N <- input$big_N

      ## Get s2

      s2 <- log(input$s)^2

      ## Get d

      d <- log(1 + input$d/100)

      ## Calculate

      if (input$use_PPF) {

        print("Poblacion finita")

        n <- N*s2*Z^2/( (N-1)*d^2 +s2*Z^2 )

      } else {
        print("Poblacion infinita")

        n <- Z^2 * s2 / d^2

      }

      n <- ceiling(n)


      ## Build the value box

      value_box(title = "Número mínimo de muestras aleatorias:",
                value = n)

    })

  })
}

## To be copied in the UI
# mod_F8_ui("F8_1")

## To be copied in the server
# mod_F8_server("F8_1")
