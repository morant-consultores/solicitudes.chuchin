#' viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shinyWidgets highcharter
#' @importFrom shiny NS tagList
mod_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      value_box(
        id = ns("vbox_tipo"),
        title = "Tipo de solicitud más común",
        theme_color = "primary",
        showcase = bs_icon("pie-chart-fill"),
        value = textOutput(ns("tipo_text")),
      ),
      value_box(
        id = ns("vbox_nivel"),
        title = "Nivel de urgencia más recurrentes",
        theme_color = "danger",
        showcase = bs_icon("exclamation-diamond"),
        value = textOutput(ns("nivel_text")),
      ),
    ),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        class = "bg-light",
        title = "Filtros",
        pickerInput(
          inputId = ns("sel_tipo"),
          label = "Tipo de solicitud",
          choices = sort(unique(solicitudes$tipo_solicitud)),
          multiple = TRUE,
          selected = sort(unique(solicitudes$tipo_solicitud)),
          options = pickerOptions(container = "body", actionsBox = TRUE),
          width = "100%"
        ),
        awesomeCheckboxGroup(
          inputId = ns("sel_nivel"),
          label = "Urgencia",
          choices = c("Alta", "Media", "Baja"),
          inline = FALSE,
          selected = c("Alta", "Media", "Baja"),
          status = "danger"
        )
      ),
      card(
        class = "bg-light",
        card_header("Solicitudes en el tiempo"),
        highchartOutput(ns("g_tiempo")),
        full_screen = T,fill = F
      )
    ),
    layout_column_wrap(
      width = "200px",
      card(
        class = "bg-light",
        card_header("Solicitudes por tipo"),
        highchartOutput(ns("g_tipo"))
      ),
      card(
        class = "bg-light",
        card_header("Solicitudes por nivel de urgencia"),
        highchartOutput(ns("g_nivel"))
      )
    )
  )
}

#' viz Server Functions
#'
#' @noRd
mod_viz_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


# Reactives ---------------------------------------------------------------
    bd_tiempo <- reactive({
      req(!is.null(input$sel_tipo) & !is.null(input$sel_nivel))

      solicitudes %>%
        filter(tipo_solicitud %in% input$sel_tipo,
               urgencia %in% input$sel_nivel) %>%
        group_by(fecha = lubridate::as_date(fecha)) %>%
        summarise(n = n()) %>%
        arrange(fecha)
    })

    color <- reactiveVal("#007ea7")

    observeEvent(input$sel_nivel, {
      if(length(input$sel_nivel) > 1){
        color("#007ea7")
      } else if(input$sel_nivel == "Alta"){
        color("#bf0603")
      } else if(input$sel_nivel == "Media"){
        color("#f4d58d")
      } else {
        color("#708d81")
      }
    })

# Value Box ---------------------------------------------------------------
    output$tipo_text <- renderText({
      data <- contar_variable(solicitudes, "tipo_solicitud")
      data <- data %>%
        slice_max(pct, n = 1, with_ties = FALSE)
      paste0(data$tipo_solicitud, " (", round(data$pct, 2), "%)")
    })

    output$nivel_text <- renderText({
      data <- contar_variable(solicitudes, "urgencia")
      data <- data %>%
        slice_max(pct, n = 1, with_ties = FALSE)

      paste0(data$urgencia, " (", data$pct, "%)")
    })


# Gráficas ----------------------------------------------------------------
    output$g_tiempo <- renderHighchart({
      validate(need(nrow(bd_tiempo()) > 0, message = "Favor de seleccionar algún elemento de los filtros"))
      browser()

      graficar_tiempo(bd_tiempo(),
                      x = "fecha",y = "n",
                      x_axis = "Fecha",
                      y_axis = "Solicitudes",
                      name = "Solicitudes recibidas",
                      "Solicitudes en el tiempo",
                      color = color(),
                      pct = FALSE
      )

    })

    output$g_tipo <- renderHighchart({
      validate(need(nrow(bd_tiempo()) > 0, message = "Favor de seleccionar algún elemento de los filtros"))
      data <- contar_variable(solicitudes, "tipo_solicitud")

      tooltip <- list(pointFormat = "<b>Solicitudes totales</b>: {point.n}")

      graficar_barras(data, x = "tipo_solicitud", y = "pct",
                      x_label = "Tipo de solicitud",
                      y_label = "Solicitudes",
                      name = "Solicitudes recibidas",
                      tooltip = tooltip,
                      fill = "#ee6c4d",
                      pct = TRUE)
    })

    output$g_nivel <- renderHighchart({
      validate(need(nrow(bd_tiempo()) > 0, message = "Favor de seleccionar algún elemento de los filtros"))
      data <- contar_variable(solicitudes, "urgencia")

      tooltip <- list(pointFormat = "<b>Solicitudes totales</b>: {point.n}")

      graficar_barras(data, x = "urgencia",
                      y = "pct",
                      x_label = "Nivel de urgencia",
                      y_label = "Solicitudes",
                      name = "Solicitudes recibidas",
                      tooltip = tooltip,
                      fill = "#4c956c",
                      pct = TRUE)
    })

  })
}

## To be copied in the UI
# mod_viz_ui("viz_1")

## To be copied in the server
# mod_viz_server("viz_1")
