#' viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'#' @noRd
#' @importFrom shinyWidgets pickerInput pickerOptions actionBttn awesomeCheckboxGroup
#' @importFrom shinyjs hidden show hide toggle
#' @importFrom shiny NS tagList
#'

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
    layout_columns(
      fill = FALSE,
      value_box(
        id = ns("vbox_recibidas"),
        title = "Total de solicitudes recibidas",
        theme_color = "secondary",
        showcase = bs_icon("file-earmark-text-fill"),
        value = "lorem-ipsum",
      ),
      value_box(
        id = ns("vbox_atendidas"),
        title = "Total de solicitudes atendidas",
        theme_color = "secondary",
        showcase = bs_icon("file-earmark-text-fill"),
        value = "lorem-ipsum",
      ),
      value_box(
        id = ns("vbox_pendientes"),
        title = "Total de solicitudes en proceso",
        theme_color = "secondary",
        showcase = bs_icon("file-earmark-text-fill"),
        value = "lorem-ipsum",
      ),
      value_box(
        id = ns("vbox_rechazadas"),
        title = "Total de solicitudes rechazadas",
        theme_color = "secondary",
        showcase = bs_icon("file-earmark-text-fill"),
        value = "lorem-ipsum",
      )
    ),
    layout_sidebar(
      border = FALSE,
      fill = FALSE,
      sidebar = sidebar(
        class = "bg-light",
        title = "Filtros generales",
        #incluir filtro de fecha
        dateRangeInput(
          inputId = ns("sel_fecha"),
          label = "Fecha",
          start = min(solicitudes$fecha),
          end = max(solicitudes$fecha),
          min = min(solicitudes$fecha),
          max = max(solicitudes$fecha),
          format = "dd/mm/yy",
          separator = " - ",
          width = "100%"
        ),
        selectInput(ns("estatus"),
                    label = "Estatus",
                    choices = sort(unique(solicitudes$estatus)),
                    selected = "recibida"),
        actionBttn(ns("filtro_gen"),
                   label = "Filtrar",
                   style = "simple",
                   color = "primary",
                   size = "sm"),
      ),
      layout_sidebar(
        fillable = FALSE,
        sidebar = sidebar(
          position = "right",
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
        card(class = "bg-light",
             card_header("Solicitudes por tipo"),
             highchartOutput(ns("g_tipo"))
        ),
        card(class = "bg-light",
             card_header("Solicitudes por nivel de urgencia"),
             highchartOutput(ns("g_nivel"))
        )
      ),
      card(class = "bg-light",
           id = ns("r_recibida"),
           card_header("Análisis de solicitudes recibidas"),
           layout_columns(
             plotOutput(ns("prom_recibido")),
             plotOutput(ns("prom_cambio_proc"))
           )
      ),
      hidden(
        card(class = "bg-light",
             id = ns("r_proceso"),
             card_header(HTML("Análisis de solicitudes <b>aceptadas</b>")),
             layout_columns(
               plotOutput(ns("prom_proc")),
               plotOutput(ns("prom_cambio_fin"))
             )
        )
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

    # Observers ---------------------------------------------------------------

    observeEvent(input$filtro_gen,{
      toggle("r_recibida", condition = input$estatus == "recibida")
      toggle("r_proceso", condition = input$estatus == "en proceso")
      toggle("r_rechazada", condition = input$estatus == "rechazada")
      toggle("r_atendida", condition = input$estatus == "finalizada")
    })

    # Reactives ---------------------------------------------------------------
    bd_inicial <- reactive({
      input$filtro_gen
      solicitudes |>
        filter(estatus == isolate(input$estatus) &
                 fecha >= isolate(input$sel_fecha)[1] & fecha <= isolate(input$sel_fecha)[2])
    })


    bd_tiempo <- reactive({
      req(!is.null(input$sel_tipo) & !is.null(input$sel_nivel))

      crear_base_tiempo(bd_inicial(), input$sel_tipo, input$sel_nivel)
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
      validate(need(nrow(bd_tiempo()) > 0, "No hay datos para mostrar"))
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
      validate(need(nrow(bd_inicial()) > 0, "No hay datos para mostrar"))
      data <- contar_variable(bd_inicial(), "tipo_solicitud")

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
      validate(need(nrow(bd_inicial()) > 0, "No hay datos para mostrar"))
      data <- contar_variable(bd_inicial(), "urgencia")

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

    # Recibidos ---------------------------------------------------------------

    output$prom_recibido <- renderPlot({
      validate(need(nrow(bd_inicial()) > 0, "No hay datos para mostrar"))
      data <- bd_inicial() |>
        mutate(fecha_hoy = lubridate::now())

      data <- calcular_diferencia(data,
                                  estatus = "recibida",
                                  fecha_reciente = "fecha_hoy",
                                  fecha_antigua = "fecha")

      graficar_rainclouds(data,
                          color = "#007ea7",
                          x = "dif",
                          y = "estatus",
                          titulo = "Distribución del tiempo en recepción",
                          eje_x = "Tiempo en días"
      )

    })
    output$prom_cambio_proc <- renderPlot({
      validate(need(nrow(bd_inicial()) > 0, "No hay datos para mostrar"))
      data <- calcular_diferencia(bd_inicial(),
                                  estatus = "en proceso",
                                  fecha_reciente = "fecha_proceso",
                                  fecha_antigua = "fecha")

      graficar_rainclouds(data,
                          color = "#007ea7",
                          x = "dif",
                          y = "estatus",
                          titulo = "Distribución del tiempo en recepción",
                          eje_x = "Tiempo en días"
      )

    })

    # En proceso --------------------------------------------------------------




  })
}

## To be copied in the UI
# mod_viz_ui("viz_1")

## To be copied in the server
# mod_viz_server("viz_1")
