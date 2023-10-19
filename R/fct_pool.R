#' pool
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom readr read_csv
#' @importFrom pool dbPool
#' @importFrom odbc odbc
#' @importFrom DBI Id
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

solicitudes <- readr::read_csv(app_sys(get_golem_config("dataset")))
