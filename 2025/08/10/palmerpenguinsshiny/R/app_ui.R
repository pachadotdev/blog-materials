#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    
    # Filters

    sidebarLayout(
      sidebarPanel(
        selectInput("species", "Select Species:", choices = unique(penguins$species), multiple = TRUE),
        selectInput("island", "Select Island:", choices = unique(penguins$island), multiple = TRUE),
        selectInput("fill", "Select Fill Color:", choices = c("#3d809d", "#d04e66", "#365158"),
          multiple = FALSE, selected = "#3d809d"),
        sliderInput("bins", "Number of Bins:", min = 1, max = 50, value = 30)
      ),
      mainPanel(
        plotOutput("main_plot")
      )
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
      app_title = "palmerpenguinsshiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
