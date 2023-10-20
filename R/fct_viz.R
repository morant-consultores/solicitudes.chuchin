#' viz
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import dplyr ggplot2 highcharter
#' @noRd

#' @title Crea una base de datos con el comportamiento de la llegada de solicitudes.
#' @description Crea una base de datos con el comportamiento de la llegada de solicitudes.
#' @param bd Base de datos de solicitudes.
#' @param tipo Tipo de solicitud, recibe una o múltiples solicitudes.
#' @param nivel Nivel de urgencia, recibe uno o múltiples niveles.
#' @return Una base de datos con el comportamiento de la llegada de solicitudes.
#' @example crear_base_tiempo(solicitudes, input$sel_tipo, input$sel_nivel)

crear_base_tiempo <- function(bd, tipo, nivel) {
  bd %>%
    filter(tipo_solicitud %in% tipo,
           urgencia %in% nivel) %>%
    group_by(fecha = lubridate::as_date(fecha)) %>%
    summarise(n = n()) %>%
    arrange(fecha)
}


contar_variable <-  function(bd, var){
  bd |>
    summarise(n = n(), .by = all_of(var)) |>
    mutate(pct = round(n/sum(n)*100, 0)) |>
    arrange(desc(n))
}

graficar_tiempo <- function(bd, x, y, x_axis, y_axis, name, titulo, color, pct = F){
  if(pct == T){
    datalabels <- list(enabled = TRUE, format = "{y}%", style = list(fontSize = "12px"))
  } else {
    datalabels <- list(enabled = TRUE, style = list(fontSize = "12px"))
  }
  highchart() |>
    hc_add_series(data = bd,
                  hcaes(x = .data[[x]], y = .data[[y]]),
                  name = name,
                  type = "line",
                  dataLabels = datalabels,
                  color = color) |>
    hc_xAxis(title = list(enable = F),
             dateTimeLabelFormats = list(day = "%e-%b"),
             labels = list(style = list(color = "#adb5bd")),
             type = "datetime") |>
    hc_yAxis(title = list(text = y_axis, style = list(list(fontSize = "16px", color = "#495057"))),
             labels = list(style = list(color = "#adb5bd")),
             gridLineWidth = 0) |>
    hc_plotOptions(line = list(marker = list(enabled = FALSE))) |>
    hc_title(text = titulo) |>
    hc_legend(enabled = FALSE)
}

graficar_barras <- function(bd, x, y, x_label, y_label, name, tooltip, fill, pct = F){
  if(pct == T){
    datalabels <- list(enabled = TRUE, format = "{y}%", style = list(fontSize = "12px"))
  } else {
    datalabels <- list(enabled = TRUE, style = list(fontSize = "12px"))
  }

  highchart() |>
    hc_xAxis(title = list(text = x_label, style = list(fontSize = "16px", color = "#495057")),
             labels = list(style = list(color = "#adb5bd")),
             categories = bd[[x]]) |>
    hc_add_series(data = bd,
                  hcaes(x = !!sym(x), y = !!sym(y)),
                  name = name,
                  type = 'bar',
                  color = fill,
                  dataLabels = datalabels,
                  tooltip = tooltip) |>
    hc_yAxis(title = list(text = y_label,
                          style = list(fontSize = "16px", color = "#495057")),
             labels = list(enabled = FALSE),
             gridLineWidth = 0) |>
    hc_legend(enabled = FALSE)
}

#' @title Calcula la diferencia entre dos tiempos dependiendo del estatus
#' @description
#' Calcula la diferencia entre dos tiempos dependiendo del estatus.
#' También se calcula la media, el mínimo y el máximo, por si esta información es de utilidad.
#' @param bd base de datos
#' @param estatus estatus de la solicitud
#' @param fecha_reciente fecha más reciente dependiendo del estatus
#' @param fecha_antigua fecha más antigua dependiendo del estatus
#' @param unidades unidades de tiempo, por default es 'hours'.
#' @examples
#' data <- calcular_diferencia(ex_data, "finalizada", "fecha_atendida", "fecha")

calcular_diferencia <- function(bd, fecha_reciente, fecha_antigua, unidades = "hours"){
  bd |>
    reframe(dif = round(as.numeric(difftime(.data[[fecha_reciente]], .data[[fecha_antigua]], units = unidades)),0),
            .by = estatus)
}

#' @title Graficar rainclouds
#' @description Grafica rainclouds, las rainclouds son la combinación de un boxplot, un punto y una densidad.
#' @param bd Base de datos con una columna 'dif'
#' @param x Variable x, normalmente es 'dif'
#' @param y Variable y, normalmente es 'estatus'
#' @param titulo Título de la gráfica
#' @param eje_x Eje x de la gráfica
#' @param color Color de la gráfica
#' @return Raincloud plot
#' @examples
#' g <- graficar_rainclouds(data, x = "dif", y = "estatus", color = "#2a9d8f", titulo = "Tiempo de espera de las solicitudes finalizadas", eje_x = "Tiempo de espera (horas)")

graficar_rainclouds <- function(bd, x, y, color, titulo, eje_x){
  ggplot(bd, aes(x = .data[[x]], y = .data[[y]])) +
    geom_boxplot(fill =  color,
                 width = 0.1) +
    geom_point(
      color = color,
      shape = "|",
      size = 10,
      alpha = 0.5,
      position = position_nudge(y = -0.175)
    ) +
    ggdist::stat_slab(
      fill =  color,
      position = position_nudge(y = 0.075)
    ) +
    labs(title = titulo, x = eje_x) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
}

#' @title Graficar boxplot
#' @description Grafica un boxplot
#' @param bd Base de datos con una columna 'dif'
#' @param var Variable x, normalmente es 'dif'
#' @param nombre Nombre de la variable
#' @param color Color de la gráfica
#' @param grupo Variable y, normalmente es 'estatus'
#' @param titulo Título de la gráfica
#' @param eje_x Eje x de la gráfica
#' @param eje_y Eje y de la gráfica
#' @return Boxplot
#' @examples
#' g <- graficar_boxplot(data, var = "dif", nombre = "Tiempo de espera", color = "#2a9d8f", grupo = "estatus", titulo = "Tiempo de espera de las solicitudes finalizadas", eje_x = "Tiempo de espera (horas)", eje_y = "Estatus")

graficar_boxplot <- function(bd, var, nombre, color, grupo, titulo, eje_x, eje_y) {
  ja <- data_to_boxplot(data = bd, variable = .data[[var]], name = nombre, color = color, group_var = .data[[grupo]])

  highchart() |>
    hc_chart(type = 'boxplot', inverted = TRUE) |>
    hc_add_series_list(ja) |>
    hc_yAxis(title = list(text = eje_y, style = list(fontSize = "16px", color = "#495057")),
             labels = list(style = list(color = "#adb5bd")),
             gridLineWidth = 0) |>
    hc_xAxis(title = list(text = eje_x, style = list(fontSize = "16px", color = "#495057")),
             categories = bd[[grupo]]) |>
    hc_legend(enabled = FALSE) |>
    hc_title(text = list(titulo))
}

