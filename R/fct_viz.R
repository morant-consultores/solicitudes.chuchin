#' viz
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import dplyr ggplot2 highcharter
#' @noRd

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

calcular_diferencia <- function(bd, estatus, fecha_reciente, fecha_antigua, unidades = "hours"){
  bd |>
    filter(estatus == !!estatus) |>
    reframe(dif = as.numeric(difftime(.data[[fecha_reciente]], .data[[fecha_antigua]], units = unidades)),
            mean = mean(dif, na.rm = T),
            lower = min(dif, na.rm = T),
            upper = max(dif, na.rm = T), .by = estatus)
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
      position = position_nudge(y = 0.075),
      heigth = 0.75
    ) +
    labs(title = titulo, x = eje_x) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
}

