#' tabla
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import htmltools
#' @noRd

input_btns <- function(inputId, users, tooltip, icon = "", status = "default", label = "") {
  tag <- lapply(
    X = users,
    FUN = function(x) {
      res <- tags$button(
        class = paste0("btn btn-", status),
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s',  {priority: 'event'})",
          inputId, x
        ),
        icon,
        label,
        href = "www.google.com",
        `data-toggle` = "tooltip",
        `data-container` = "body"
      )
      res <- tagList(res, tags$script(HTML("$('[data-toggle=\"tooltip\"]').tooltip();")))
      doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}

seleccionar_variables <- function(bd){
  bd |>
    transmute(id,
              Fecha = lubridate::as_date(fecha),
              Ubicación = ubicacion,
              Remitente = remitente,
              Urgencia = urgencia,
              "Tipo de solicitud" = tipo_solicitud,
              Solicitud = texto)
}

crear_tabla <- function(bd){
  datatable(
    bd,
    filter = 'top',
    selection = 'none',
    rownames = FALSE,
    extensions = 'Responsive',
    options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
      drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
      pageLength = 10
    ),
    escape = FALSE
  )
}
