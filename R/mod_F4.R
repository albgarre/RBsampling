#' F4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_F4_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # card(
    #   min_height = "0px",
    #   card_title("F4: Estimación sin CPF"),
    #   card_body(
        layout_columns(
          col_widths = c(6, 6),
          layout_columns(
            col_widths = 12,

            ## Parameter p

            popover(
              selectInput(ns("F1_p_type"), "Prevalencia esperada",
                          c(`Conocida` = "known",
                            `Desconocida (50%)` = "unknown"
                          )
              ),
              "",
              "Basada en datos históricos. Si se desconode, 50% da un escenario conservativo."
            ),

            conditionalPanel(
              ns = ns,
              condition = "input.F1_p_type == 'known'",
              numericInput(ns("F1_p"), "(%)", 50)
            ),

            ## Parameter d

            popover(
              numericInput(ns("d"), "Precisión deseada (en por 1)",
                           0.02,
                           step = .01),
              "",
              "Se recomienda 0,05 en vigilancia exploratoria y valores entre 0,02 y 0,03 en combinaciones prioritarias."
            ),

            ## Parameter alpha

            popover(
              numericInput(ns("conf"), "Nivel de confianza (%)",
                           95),
              "",
              "Se recomienda usar valures de 95%"

            ),

            ## Correccion por poblacion finita

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
    #   )
    # )

  )
}

#' F4 Server Functions
#'
#' @noRd
mod_F4_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$F1_outcome <- renderUI({

      ## Get the value of Z

      alpha <- 1 - input$conf/100
      Z <- qnorm(1 - alpha/2)

      ## Get the value of p

      p <- switch(
        input$F1_p_type,
        unknown = 0.5,
        known = input$F1_p/100

      )

      ## Get the value of d

      d <- input$d

      ## Get the value of N

      N <- input$big_N

      ## Calculate

      if (input$use_PPF) {

        print("con correccion")

        n <- N * Z^2 * p * (1-p) / ( (N-1)*d^2 + Z^2*p*(1-p) )
      } else {
        print("sin correccion")

        n <- Z^2 * p * (1-p)/d^2
      }

      n <- ceiling(n)

      ## Check whether correction is recommended

      if (n/N < .1) {
        showNotification(paste("No se recomienda la corrección debido a que la fracción muestreada es",
                               round(n/N, 2), "< 0,1"),
                         type = "warning",
                         duration = NULL)
      }





      ## Build the value box

      value_box(title = "Número mínimo de muestras aleatorias:",
                value = n)

    })

  })
}

## To be copied in the UI
# mod_F4_ui("F4_1")

## To be copied in the server
# mod_F4_server("F4_1")
