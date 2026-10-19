#' calculadoras UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_calculadoras_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1,
      mod_F2_ui("F2_1")
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_title(h2("Tamaño muestral para cálculo de prevalencia")),
        mod_F4_ui("F4_1")
      )
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_title(h2("Tamaño muestral para población normal")),
        mod_F7_ui("F7_1")
      )
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_title(h2("Tamaño muestral para población log-normal")),
        mod_F8_ui("F8_1")
      )
    )

  )
}

#' calculadoras Server Functions
#'
#' @noRd
mod_calculadoras_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_calculadoras_ui("calculadoras_1")

## To be copied in the server
# mod_calculadoras_server("calculadoras_1")
