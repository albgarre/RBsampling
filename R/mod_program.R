#' program UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_column_wrap navset_card_pill nav_item popover value_box card_footer card_body layout_columns card_title card tooltip layout_sidebar sidebar
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_table rhandsontable
#' @importFrom rlang set_names
#'
mod_program_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(  # DEFINICIÓN CAPACIDAD DE CAMPO #######################
      card(
        card_title("Capacidad de campo"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            layout_columns(
              col_widths = 12,
              numericInput(ns("cap_lab"), "Capacidad laboratorial", 0),
              numericInput(ns("cap_campo"), "Capacidad de campo", 0),
              numericInput(ns("cap_eur"), "Capacidad presupuestaria", 0),
              numericInput(ns("cap_acc"), "Capacidad de acceso", 0)
            ),
            layout_columns(
              col_widths = 12,
              uiOutput(ns("out_capOper"))
            )
          )
        )
      )


    ),
    layout_column_wrap(  # DEFINICION DE ESCENARIOS ############################
      width = 1,
      # h2("Definición de")
      navset_card_pill(
        title = "Definición de escenarios",
        nav_panel(
          title = "Añadir/Modificar",
          card_body(
            layout_column_wrap(
              width = 1/3,
              textInput(ns("new_name"), "Nombre", "Peligro/alimento #1"),
              selectInput(ns("new_riskLevel"), "Nivel de riesgo",
                          choices = c(`Muy alto` = "my_alto",
                                      Alto = "alto",
                                      Medio = "medio",
                                      Bajo = "bajo"
                          )
              ),
              selectInput(ns("new_type"), "Objetivo del muestreo",
                          choices = c(`Detección de prevalencia mínima` = "detect",
                                      `Estimación de prevalencia` = "prev",
                                      `Estimación de concentración` = "conc",
                                      `Manual` = "man"
                          )
              )
            )
          ),
          conditionalPanel(
            condition = "input.new_type == 'detect'",
            ns = ns,
            ## Parameter C

            layout_columns(
              col_widths = c(6, 6),
              # width = 1/2,
              # heights_equal = "row",

              layout_columns(
                col_widths = 12,
                # width = 1,
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
                # width = 1,
                col_widths = 12,
                uiOutput(ns("F1_outcome"))
              )

            )

          ),
          conditionalPanel(  # Estimation of prevalence ------------------------
            condition = "input.new_type == 'prev'",
            ns = ns,
            "prev"
          ),
          conditionalPanel(  # Estimation of concentration ---------------------
            condition = "input.new_type == 'conc'",
            ns = ns,
            "conc"
          ),
          conditionalPanel(  # Manual input ------------------------------------
            condition = "input.new_type == 'man'",
            ns = ns,
            numericInput(ns("n_manual"), "Número de muestras", 0)
          ),

          textInput(ns("new_comments"), "Comentarios", ""),

          card_footer(
            actionButton(ns("add_scenario"), "Añadir escenario")
          )
        ),
        nav_panel(
          title = "Eliminar",
          "CC"
        )
      )
    ),

    layout_column_wrap(  # Comparing needs versus capacity----------------------
      width = 1,
      card(
        card_title("Necesidades vs. capacidad"),
        tableOutput(ns("summary_cases")),
        layout_columns(
          col_widths = c(6, 6),
          layout_columns(
            col_widths = 12,
            uiOutput(ns("boxes_sizes"))
          ),
          layout_columns(
            col_widths = 12,
            uiOutput(ns("box_comparison"))
          )
        )
      )
    ),
    layout_column_wrap(  # Diseño muestral -------------------------------------

      navset_card_pill(
        title = "Diseño muestral",
        sidebar = sidebar(
          selectInput(ns("scenario_for_design"),
                      "Escenario",
                      choices = c()
                      ),
          textOutput(ns("n_samples_scenario"))
        ),
        nav_panel("Estratos",
                  radioButtons(
                    ns("tipo_muestreo"),
                    "Tipo de diseño muestral",
                    c(`Aleatorio` = "random",
                      `Estratificado proporcional al tamaño` = "size",
                      `Estratificado óptimo de Neyman` = "neisy")
                    ),
                  conditionalPanel(
                    ns = ns,
                    condition = "input.tipo_muestreo == 'size'",
                    layout_columns(
                      col_widths = c(6, 6),
                      layout_columns(
                        col_widths = 12,
                        tooltip(
                          bs_icon("info-circle"),
                          "",
                          "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
                        ),
                        rHandsontableOutput(ns("size_input"))
                      ),
                      tableOutput(ns("outcome_prop"))
                    )
                  ),
                  conditionalPanel(
                    ns = ns,
                    condition = "input.tipo_muestreo == 'neisy'",
                    layout_columns(
                      col_widths = c(6, 6),
                      layout_columns(
                        col_widths = 12,
                        tooltip(
                          bs_icon("info-circle"),
                          "",
                          "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
                        ),
                        rHandsontableOutput(ns("neisy_input"))
                      ),
                      tableOutput(ns("outcome_neisy"))
                    )
                  )
                  ),
        nav_panel("Temporal",
                  radioButtons(
                    ns("tipo_temporal"),
                    "Tipo de distribución temporal",
                    c(`Aleatorio` = "random",
                      `Proporcional a la producción` = "prop")
                    ),
                  conditionalPanel(
                    ns = ns,
                    condition = "input.tipo_temporal == 'prop'",
                    layout_columns(
                      col_widths = c(6, 6),
                      layout_columns(
                        col_widths = 12,
                        tooltip(
                          bs_icon("info-circle"),
                          "",
                          "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
                        ),
                        rHandsontableOutput(ns("time_input"))
                      ),
                      tableOutput(ns("outcome_time"))
                    )
                  )
                  ),
        nav_panel("Calendario",
                  tableOutput(ns("calendar_table")),
                  card_footer(
                    downloadButton(ns("download_calendar"))
                  )

                  ),
        )

    )
  )
}

#' program Server Functions
#'
#' @importFrom purrr map map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr summarize pull mutate filter select left_join
#' @importFrom tidyr pivot_wider
#' @importFrom writexl write_xlsx
#'
#' @noRd
mod_program_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    ## Capacidad operativa

    cap_oper <- reactive({

      min(c(input$cap_lab, input$cap_campo,
            input$cap_eur, input$cap_acc))

    })

    output$out_capOper <- renderUI({

      validate(
        need(cap_oper(), "")
      )


      value_box(title = "Capacidad operativa",
                value = cap_oper())

    })



    ## Reactives for the scenarios

    my_scenarios <- reactiveVal(list())



    ## Calculation of the sampling size

    n_detection <- reactive({ # Sample size by detection prevalence

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
      ceiling(n)

    })

    output$F1_outcome <- renderUI({

      validate(
        need(n_detection(), ""),
      )

      ## Build the value box

      value_box(title = "Número mínimo de muestras aleatorias:",
                value = n_detection())

    })

    ## Adding scenarios

    observeEvent(input$add_scenario, {

      out_scenarios <- my_scenarios()

      ## Get the metadata

      new_scenario <- list()

      new_scenario[["name"]] <- input$new_name
      new_scenario[["risk_level"]] <- input$new_riskLevel
      new_scenario[["type"]] <- input$new_type
      new_scenario[["comments"]] <- input$new_comments

      ## Get the n

      if (input$new_type == "detect") {

        this_n <- n_detection()

      } else if (input$new_type == "prev") {

      } else if (input$new_type == "conc") {

      } else if (input$new_type == "man") {

        this_n <- input$n_manual

      }


      new_scenario[["n"]] <- this_n

      ## Update the reactive

      out_scenarios[[input$new_name]] <- new_scenario

      my_scenarios(out_scenarios)

    })

    ## Comparison of capacity and requirements

    output$summary_cases <- renderTable({

      validate(
        need(length(my_scenarios()) > 0, "Define at least one scenario"),
      )

      # browser()

      my_scenarios() |>
        map_dfr(
          ~ tibble(Escenario = .$name,
                   `Nivel de riesgo` = .$risk_level,
                   `Objetivo del muestreo` = .$type,
                   `Número de muestras` = .$n,
                   `Comentarios` = .$comments
          ))



    })

    output$boxes_sizes <- renderUI({

      validate(
        need(length(my_scenarios()) > 0, ""),
      )

      ## Calculate the numbers

      n_needed <- my_scenarios() |>
        map_dfr(
          ~ tibble(
                   n = .$n,

          )) |>
        summarize(n = sum(n)) |>
        pull(n)

      n_oper <- cap_oper()

      ## Return the boxes

      tagList(
        value_box(title = "Capacidad operativa",
                  value = cap_oper()),
        value_box(title = "Número total de muestras",
                  value = n_needed)

      )
    })

    output$box_comparison <- renderUI({

      validate(
        need(length(my_scenarios()) > 0, ""),
      )

      ## Calculate the numbers

      n_needed <- my_scenarios() |>
        map_dfr(
          ~ tibble(
            n = .$n,

          )) |>
        summarize(n = sum(n)) |>
        pull(n)

      n_oper <- cap_oper()

      cap <- n_oper - n_needed

      ## Return the box

      if (cap > 0) {

        value_box(title = "Hay un excendente de capacidad operativa",
                  value = cap,
                  theme = "primary")

      } else if (cap == 0) {

        value_box(title = "Hay una concordancia perfecta",
                  value = cap,
                  theme = "success")

      } else {

        value_box(title = "Hay una falta de capacidad operativa",
                  value = cap,
                  theme = "danger")

      }

    })

    ## Diseño muestral ---------------------------------------------------------

    observeEvent(my_scenarios(), {

      validate(
        need(length(my_scenarios()) > 0, "")
      )

      ## Get the scenarios with at least one

      sampled_scenarios <- my_scenarios() |>
        map_dfr(
          ~ tibble(scenario = .$name,
                   n = .$n
          )) |>
        filter(n > 0) |>
        pull(scenario)

      updateSelectInput(
        session = session,
        "scenario_for_design",
        choices = sampled_scenarios
      )


    })

    output$n_samples_scenario <- renderText({

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      paste0("Number of samples: ", ntot)

    })

    output$size_input <- renderRHandsontable({  # Proportional estrato

      if (!is.null(input$size_input)) {
        DF <- hot_to_r(input$size_input)

      } else {
        DF <- data.frame(Estrato = c("E1", "E2", "E3"),
                         `Tamaño` = c(1000, 2000, 7000)
        )
      }

      DF |>
        set_names(c("Estrato", "Tamaño")) |>
        rhandsontable() |>
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$outcome_prop <- renderTable({

      ## Get the table

      DF <- hot_to_r(input$size_input)

      ## Get the number of samples

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      DF |>
        mutate(f = `Tamaño`/sum(`Tamaño`, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    output$neisy_input <- renderRHandsontable({  # Neyman

      if (!is.null(input$neisy_input)) {
        DF <- hot_to_r(input$neisy_input)

      } else {
        DF <- data.frame(Estrato = c("E1", "E2", "E3"),
                         `Tamaño` = c(1000, 2000, 7000),
                         `Desv.std esperada` = c(5, 3, 10)
        )
      }

      DF |>
        set_names(c("Estrato", "Tamaño", "Desv.std esperada")) |>
        rhandsontable() |>
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$outcome_neisy <- renderTable({

      ## Get the table

      DF <- hot_to_r(input$neisy_input)

      ## Get the number of samples

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      DF |>
        mutate(eps = `Tamaño`*`Desv.std esperada`,
               f = eps/sum(eps, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    output$time_input <- renderRHandsontable({  # Proportional to production

      if (!is.null(input$time_input)) {
        DF <- hot_to_r(input$time_input)

      } else {
        DF <- data.frame(Temporada = c("T1", "T2", "T3"),
                         `Producción` = c(1000, 2000, 7000)
        )
      }

      DF |>
        set_names(c("Temporada", "Producción")) |>
        rhandsontable() |>
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$outcome_time <- renderTable({

      ## Get the table

      DF <- hot_to_r(input$time_input)

      ## Get the number of samples

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      DF |>
        mutate(f = `Producción`/sum(`Producción`, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    ## Calendar

    final_calendar <- reactive({

      validate(
        need(length(my_scenarios()) > 0, "Define at least one scenario"),
      )

      ## Get the sample size

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      # browser()

      ## Get the type of estrato sampling

      if (input$tipo_muestreo == "random") {

        dist_estrato <- tibble(Estrato = "Unique", f = 1)

      } else if (input$tipo_muestreo == "size") {

        ## Get the table

        DF <- hot_to_r(input$size_input)

        ## Get the number of samples

        dist_estrato <- DF |>
          mutate(f = `Tamaño`/sum(`Tamaño`, na.rm = TRUE),
                 n = round(ntot*f)
          ) |>
          select(Estrato, f)

      } else if (input$tipo_muestreo == "neisy") {

        ## Get the table

        DF <- hot_to_r(input$neisy_input)

        dist_estrato <- DF |>
          mutate(eps = `Tamaño`*`Desv.std esperada`,
                 f = eps/sum(eps, na.rm = TRUE),
                 n = round(ntot*f)
          ) |>
          select(Estrato, f)


      }

      ## Get the type of temporal sampling

      if (input$tipo_temporal == "random") {

        dist_time <- tibble(Temporada = c("Primavera", "Verano", "Otoño", "Invierno"),
                            f_time = .25)

      } else if (input$tipo_temporal == "prop") {

        DF <- hot_to_r(input$time_input)

        dist_time <- DF |>
          mutate(f = `Producción`/sum(`Producción`, na.rm = TRUE),
                 `Número de muestras` = round(ntot*f)
          ) |>
          select(Temporada, f_time = f)

      }

      ## Scale stuff

      dist_time |>
        pull(Temporada) |>
        map_dfr(
          ~ mutate(dist_estrato, Temporada = .)
        ) |>
        left_join(dist_time) |>
        mutate(f_tot = f*f_time) |>
        mutate(n = ceiling(f_tot*ntot)) |>
        select(Estrato, Temporada, n) |>
        pivot_wider(names_from = Temporada, values_from = n)

    })

    output$calendar_table <- renderTable({

      validate(
        need(final_calendar(), "")
      )

      final_calendar()

    })

    output$download_calendar <- downloadHandler(

      filename = function() {
        paste0("sampling_calendar", ".xlsx")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write_xlsx(final_calendar(), file)
      }

    )

  })
}

## To be copied in the UI
# mod_program_ui("program_1")

## To be copied in the server
# mod_program_server("program_1")
