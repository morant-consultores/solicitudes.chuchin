#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib bsicons
#' @importFrom shinyjs useShinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "Solicitud de whatsapp",
      fluid = T,
      fillable = FALSE,
      theme = bs_theme(primary = "#3586C6",
                       font_scale = NULL,
                       preset = "cyborg"),
      nav_spacer(),
      nav_panel("Tablero de control", mod_viz_ui("viz_1"), icon = bs_icon("speedometer")),
      nav_panel("Seguimiento", mod_tabla_ui("tabla_1"), icon = bs_icon("table"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "solicitudes.chuchin"
    ),
    shinyjs::useShinyjs(),
    thematic::thematic_shiny()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
