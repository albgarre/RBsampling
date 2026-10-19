#' F2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_F2_ui <- function(id) {
  ns <- NS(id)
  tagList(

    card(
      min_height = "0px",
      card_title("Tamaño muestral basado en prevalencia de diseño"),
    card_body(
        layout_columns(
          col_widths = c(6, 6),
          layout_columns(
            col_widths = 12,

            ## Parameter C

            popover(
              selectInput(ns("F1_C_type"), "Probabilidad de detección deseada",
                          c(`Peligros graves (95%)` = "graves",
                            `Vigilancia exploratoria (90%)` = "explor",
                            `Certificación de exportación (99%)` = "export",
                            `Manual` = "user"
                          )
              ),
              "",
              "Se incluyen valores recomendados en función del tipo de peligro. Se puede definir un valor concreto con 'Manual'"
            ),
            conditionalPanel(
              ns = ns,
              condition = "input.F1_C_type == 'user'",
              numericInput(ns("F1_C"), "(%)", 90),
            ),

            ## Parameter p

            popover(
              selectInput(ns("F1_p_type"), "Prevalencia mínima de diseño",
                          c(`Peligros muy graves (1%)` = "graves",
                            `Rutina (5%)` = "rutina",
                            `Valor de usuario` = "user"
                          )
              ),
              "",
              "Se incluyen valores recomendados en función del tipo de peligro. Se puede definir un valor concreto con 'Manual'"
            ),
            conditionalPanel(
              ns = ns,
              condition = "input.F1_p_type == 'user'",
              numericInput(ns("F1_p"), "(%)", 1)
            ),


            ## Parameter SeU

            checkboxInput(ns("use_SeU"),
                          "¿Considerar sensibilidad a nivel de unidad?"
            ),

            conditionalPanel(
              ns = ns,
              condition = "input.use_SeU",

              popover(
                numericInput(ns("SeU"), "Sensibilidad a nivel de unidad (%)",
                             90),
                "",
                "Este valor depende de la aproximación metodológica y el equipamiento utilizados."
              )

              )




          ),
          layout_columns(
            col_widths = 12,
            uiOutput(ns("F1_outcome"))
          )


        )
    )
    )


  )
}

#' F2 Server Functions
#'
#' @noRd
mod_F2_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$F1_outcome <- renderUI({

      ## Get the value of C

      C <- switch(
        input$F1_C_type,
        graves = 0.95,
        explor = 0.9,
        export = 0.99,
        user = input$F1_C/100

      )

      ## Get the value of p

      p <- switch(
        input$F1_p_type,
        graves = 0.01,
        rutina = 0.05,
        user = input$F1_p/100

      )

      ## Get the SeU

      if (input$use_SeU) {

        SeU <- input$SeU/100

      } else {

        SeU <- 1

      }

      ## Calculate

      n <- log(1-C)/log(1-p*SeU)
      n <- ceiling(n)


      ## Build the value box

      value_box(title = "Número mínimo de muestras aleatorias:",
                value = n)

    })

  })
}

## To be copied in the UI
# mod_F2_ui("F2_1")

## To be copied in the server
# mod_F2_server("F2_1")
