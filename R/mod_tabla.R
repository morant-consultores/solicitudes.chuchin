#' tabla UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom shiny NS tagList
mod_tabla_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_sidebar(
      fillable = F,
      sidebar = sidebar(
        class = "bg-light",
        title = "Estatus de solicitudes",
        selectInput(
          inputId = ns("sel_solicitud"),
          label = "Tipo de solicitud",
          choices = c("Seleccione..." = "", sort(unique(solicitudes$estatus))),
          selected = ""
        ),
        actionBttn(
          inputId = ns("btn_nueva_solicitud"),
          label = "Nueva solicitud",
          icon = icon("plus"),
          style = "minimal",
          color = "success"
        )
      ),
      DT::DTOutput(ns("tabla"))
    )
  )
}

#' tabla Server Functions
#'
#' @noRd
mod_tabla_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    bd_tabla <- eventReactive(input$btn_nueva_solicitud, {
      req(input$sel_solicitud != "")
      data <- solicitudes |>
        filter(estatus == input$sel_solicitud) |>
        seleccionar_variables()

      if(input$sel_solicitud != "finalizada"){
        data <- data |>
          mutate(Atender = input_btns(inputId = ns("estatus"),
                               as.character(id),
                               tooltip = "Cambiar estatus",
                               icon = icon("arrow-right"),
                               status = "default", label = "")) |>
          select(-id)
      }
      data
    })

    output$tabla <- renderDT({
      crear_tabla(bd_tabla())
    })

    observeEvent(input$estatus, {
      if(input$sel_solicitud == "recibida"){
        choices <- c("Aceptada" = "en proceso", "Rechazada" = "rechazada")
      } else if(input$sel_solicitud == "en proceso"){
        choices <- c("Finalizada" = "finalizada", "Rechazada" = "rechazada")
      } else if(input$sel_solicitud == "rechazada"){
        choices <- c("Aceptar" = "en proceso")
      }

      showModal(
          modalDialog(
            title = "¿Qué estatus desea asignar a la solicitud seleccionada?",
            pickerInput(inputId = ns("sel_estatus"), "Estatus", choices),
            shinyjs::hidden(textAreaInput(inputId = ns("txt_motivo"), "Motivo de rechazo")),
            easyClose = T,
            footer = actionBttn(inputId = ns("aceptar"), label =  "Aceptar", icon = icon("check"),
                                style = "minimal", color = "success")
          )
      )
    })

    observeEvent(input$sel_estatus, {
      if(input$sel_estatus == "rechazada"){
        shinyjs::show("txt_motivo")
      } else{
        shinyjs::hide("txt_motivo")
      }
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
