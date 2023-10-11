#' viz
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
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
             labels = list(style = list(color = "#adb5bd")),
             gridLineWidth = 0) |>
    hc_legend(enabled = FALSE)
}
