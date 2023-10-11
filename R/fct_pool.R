#' pool
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import pool odbc dplyr DBI dbplyr stringr
#' @noRd

pool <- pool::dbPool(
  drv = odbc::odbc(),
  Driver= 'ODBC Driver 17 for SQL Server',
  Database = "DIRECTORIO",
  Server = "tcp:morant.database.windows.net",
  UID = "emorones",
  PWD = "Presidevis-Emi",
  Port = 1433,
  timeout = 120
)

solicitudes <- tbl(pool, Id(schema = "chuchin", table = "solicitudes")) |>
  collect() |>
  mutate(urgencia = factor(stringr::str_to_title(urgencia), levels = c("Alta", "Media", "Baja")),
         tipo_solicitud = stringi::stri_trans_general(tipo_solicitud, "Latin-ASCII"),
         tipo_solicitud = case_when(grepl("servicios", tipo_solicitud, ignore.case = T) ~ "Servicios Públicos",
                                    grepl("temas", tipo_solicitud, ignore.case = T) ~ "Temas personales",
                                    T ~ stringr::str_to_title(tipo_solicitud))
  )
