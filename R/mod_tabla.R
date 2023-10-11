#' tabla UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import DT shinyjs shinyalert
#' @importFrom shiny NS tagList
mod_tabla_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    DT::DTOutput(ns("tabla"))
    # card(
    #   class = "p-0",
    #   card_header("Solicitudes"),
    #   DT::DTOutput(ns("tabla"))
    # )
  )
}

#' tabla Server Functions
#'
#' @noRd
mod_tabla_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tabla <- renderDT({
      data <- seleccionar_variables(solicitudes) |>
        mutate(
          Atender = input_btns(inputId = ns("estatus"),
                               as.character(id),
                               tooltip = "Cambiar estatus",
                               icon = icon("arrow-right"),
                               status = "default", label = "")
        ) |>
        select(-id)

      crear_tabla(data)
    })

    observeEvent(input$estatus, {
      showModal(
        modalDialog(
          title = "¿Qué estatus desea asignar a la solicitud seleccionada?",
          pickerInput(inputId = ns("sel_estatus"), "Estatus", c("Aceptada", "Rechazada", "Pendiente"),
                      selected = "Pendiente"),
          easyClose = T,
          footer = actionBttn(inputId = ns("aceptar"), label =  "Aceptar", icon = icon("check"),
                              style = "minimal", color = "success")
        )
      )
    })

    observeEvent(input$aceptar,{
      # query <- glue::glue("UPDATE solicitudes SET estatus = {input$sel_estatus} WHERE id = {input$estatus}")
      # dbExecute(pool, query)
    })
  })
}

## To be copied in the UI
# mod_tabla_ui("tabla_1")

## To be copied in the server
# mod_tabla_server("tabla_1")
