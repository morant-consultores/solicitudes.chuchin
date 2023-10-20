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
           card_header(HTML("Análisis de solicitudes <b>recibidas</b>")),
           layout_columns(
             highchartOutput(ns("prom_recibido")),
             highchartOutput(ns("prom_cambio_proc"))
           )
      ),
      hidden(
        card(class = "bg-light",
             id = ns("r_proceso"),
             card_header(HTML("Análisis de solicitudes <b>aceptadas</b>")),
             layout_columns(
               highchartOutput(ns("prom_proc")),
               highchartOutput(ns("prom_cambio_fin"))
             )
        )
      ),
      hidden(
        card(class = "bg-light",
             id = ns("r_rechazada"),
             card_header(HTML("Análisis de solicitudes <b>rechazadas</b>")),
        )
      ),
      hidden(
        card(class = "bg-light",
             id = ns("r_finalizada"),
             card_header(HTML("Análisis de solicitudes <b>finalizadas</b>")),
             layout_columns(
               col_widths = c(6,6, -2, 8, -2),
               highchartOutput(ns("prom_rec_fin")),
               highchartOutput(ns("prom_acep_fin")),
               highchartOutput(ns("finalizadas_tipo_nivel"))
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
      toggle("r_finalizada", condition = input$estatus == "finalizada")
    })

    # Reactives ---------------------------------------------------------------
    bd_inicial <- reactive({
      input$filtro_gen
      solicitudes |>
        filter(fecha >= isolate(input$sel_fecha)[1] & fecha <= isolate(input$sel_fecha)[2])
    })

    bd_estatus <- reactive({
      bd_inicial() |>
        filter(estatus == isolate(input$estatus))
    })

    bd_tiempo <- reactive({
      req(!is.null(input$sel_tipo) & !is.null(input$sel_nivel))

      crear_base_tiempo(bd_estatus(), input$sel_tipo, input$sel_nivel)
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
      validate(need(nrow(bd_estatus()) > 0, "No hay datos para mostrar"))
      data <- contar_variable(bd_estatus(), "tipo_solicitud")

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
      validate(need(nrow(bd_estatus()) > 0, "No hay datos para mostrar"))
      data <- contar_variable(bd_estatus(), "urgencia")

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
    output$prom_recibido <- renderHighchart({
      validate(need(nrow(bd_inicial()) > 0, "No hay datos para mostrar"))
      data <- bd_estatus() |>
        mutate(fecha_hoy = lubridate::now())

      data <- calcular_diferencia(data,
                                  fecha_reciente = "fecha_hoy",
                                  fecha_antigua = "fecha",
                                  unidades = "days")

      graficar_boxplot(data,
                       var = "dif",
                       nombre = "",
                       grupo = "estatus",
                       titulo = "Distribución del tiempo en recepción",
                       eje_x = "Estatus",
                       eje_y = "Tiempo en días",
                       color = "#007ea7")

    })

    output$prom_cambio_proc <- renderHighchart({
      validate(need(nrow(filter(solicitudes, estatus == "en proceso")) > 0, "No hay datos para mostrar"))
      data <- bd_inicial() |>
        filter(!is.na(fecha_proceso)) |>
        calcular_diferencia(fecha_reciente = "fecha_proceso",
                            fecha_antigua = "fecha")

      graficar_boxplot(data,
                       var = "dif",
                       nombre = "",
                       grupo = "estatus",
                       titulo = "Tiempo entre 'recibida' y 'en proceso'",
                       eje_x = "Estatus",
                       eje_y = "Tiempo en días",
                       color = "#ee9b00")


    })

    # En proceso --------------------------------------------------------------
    output$prom_proc <- renderHighchart({
      validate(need(nrow(bd_inicial()) > 0, "No hay datos para mostrar"))
      data <- bd_estatus() |>
        mutate(fecha_hoy = lubridate::now())

      data <- calcular_diferencia(data,
                                  fecha_reciente = "fecha_hoy",
                                  fecha_antigua = "fecha_proceso",
                                  unidades = "days")

      graficar_boxplot(data,
                       var = "dif",
                       nombre = "",
                       grupo = "estatus",
                       titulo = "Distribución del tiempo en 'aceptada'",
                       eje_x = "Estatus",
                       eje_y = "Tiempo en días",
                       color = "#ee9b00")

    })

    output$prom_cambio_fin <- renderHighchart({
      validate(need(nrow(filter(bd_inicial(), estatus == "finalizada")) > 0, "No hay datos para mostrar"))

      data <- bd_inicial() |>
        filter(!is.na(fecha_atendida)) |>
        calcular_diferencia(fecha_reciente = "fecha_atendida",
                            fecha_antigua = "fecha_proceso")

      graficar_boxplot(data,
                       var = "dif",
                       nombre = "",
                       grupo = "estatus",
                       titulo = "Tiempo entre 'aceptada' y 'finalizada'",
                       eje_x = "Estatus",
                       eje_y = "Tiempo en días",
                       color = "#ee9b00")

    })


    # Rechazadas --------------------------------------------------------------

    # Finalizadas -------------------------------------------------------------
    output$prom_rec_fin <- renderHighchart({
      data <- calcular_diferencia(bd_estatus(),
                                  fecha_reciente = "fecha_atendida",
                                  fecha_antigua = "fecha",
                                  unidades = "days")

      graficar_boxplot(data,
                       var = "dif",
                       nombre = "",
                       grupo = "estatus",
                       titulo = "Distribución del tiempo en 'aceptada'",
                       eje_x = "Estatus",
                       eje_y = "Tiempo en días",
                       color = "#06d6a0")
    })

    output$prom_acep_fin <- renderHighchart({
      validate(need(nrow(filter(bd_inicial(), estatus == "finalizada")) > 0, "No hay datos para mostrar"))
      data <- calcular_diferencia(bd_estatus(),
                                  fecha_reciente = "fecha_atendida",
                                  fecha_antigua = "fecha_proceso",
                                  unidades = "days")

      graficar_boxplot(data,
                       var = "dif",
                       nombre = "",
                       grupo = "estatus",
                       titulo = "Distribución del tiempo en 'aceptada'",
                       eje_x = "Estatus",
                       eje_y = "Tiempo en días",
                       color = "#06d6a0")
    })

  })
}

## To be copied in the UI
# mod_viz_ui("viz_1")

## To be copied in the server
# mod_viz_server("viz_1")
