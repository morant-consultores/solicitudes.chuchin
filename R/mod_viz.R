#' viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'#' @noRd
#' @importFrom shinyWidgets pickerInput pickerOptions actionBttn awesomeCheckboxGroup
#' @importFrom shinyjs hidden show hide
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
        position = "right",
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
        card(class = "bg-light",
             card_header("Solicitudes por tipo"),
             highchartOutput(ns("g_tipo"))
        ),
        card(class = "bg-light",
             card_header("Solicitudes por nivel de urgencia"),
             highchartOutput(ns("g_nivel"))
        )
      ),
      # Recibidas ---------------------------------------------------------------
      layout_column_wrap(
        id = ns("r_recibida"),
        width = "200px",
        card(
          class = "bg-light",
          card_header("Tiempo en recibido"),
          plotOutput(ns("prom_recibido"))
        ),
        card(
          class = "bg-light",
          card_header("Tiempo para cambiar 'en proceso'"),
          plotOutput(ns("prom_cambio_proc"))
        )
      ),
      # En proceso --------------------------------------------------------------
      shinyjs::hidden(
        layout_column_wrap(
          id = ns("r_proceso"),
          width = "200px",
          card(
            class = "bg-light",
            card_header("Tiempo promedio siendo atendida"),
            plotOutput(ns("prom_atencion"))
          ),
          card(
            class = "bg-light",
            card_header("Tiempo para cambiar a 'atendida'"),
            plotOutput(ns("prom_cambio_aten"))
          )
        )
      ),
      # Rechazadas --------------------------------------------------------------
      layout_columns(
        col_widths = c(-2,8,-2),
        id = ns("r_rechazada"),
        card(
          class = "bg-light",
          card_header("Distribución de solicitudes rechzadas por tipo"),
          plotOutput(ns("tipo_rechazo"))
        )
      ),
      # Atendidas ---------------------------------------------------------------
      layout_columns(
        id = ns("r_atendida"),
        col_widths = c(6,6,12),
        card(
          class = "bg-light",
          card_header("Tiempo promedio entre recepción y atención"),
          plotOutput(ns("prom_rec_aten"))
        ),
        card(
          class = "bg-light",
          card_header("Tiempo promedio entre 'en proceso' y atención"),
          plotOutput(ns("prom_proc_aten"))
        ),
        card(
          class = "bg-light",
          card_header("Bullet del maestro"),
          plotOutput(ns("bullet"))
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

    # observeEvent(input$filtro_gen,{
    #   #Crear show/hides de acuerdo al estatus
    #   browser()
    #   if(input$estatus == "recibida"){
    #     shinyjs::show("r_recibida")
    #     shinyjs::hide("r_proceso")
    #     shinyjs::hide("r_rechazada")
    #     shinyjs::hide("r_atendida")
    #   } else if(input$estatus == "en proceso"){
    #     shinyjs::hide("r_recibida")
    #     shinyjs::show("r_proceso")
    #     shinyjs::hide("r_rechazada")
    #     shinyjs::hide("r_atendida")
    #   } else if(input$estatus == "rechazada"){
    #     shinyjs::hide("r_recibida")
    #     shinyjs::hide("r_proceso")
    #     shinyjs::show("r_rechazada")
    #     shinyjs::hide("r_atendida")
    #   } else {
    #     shinyjs::hide("r_recibida")
    #     shinyjs::hide("r_proceso")
    #     shinyjs::hide("r_rechazada")
    #     shinyjs::show("r_atendida")
    #   }
    # })

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

    # Recibidos ---------------------------------------------------------------

    output$prom_recibido <- renderPlot({
      data <- solicitudes |>
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
      data <- calcular_diferencia(solicitudes,
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
