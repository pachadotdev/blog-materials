#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @importFrom dplyr filter
#'
#' @noRd
app_server <- function(input, output, session) {
  # Main plot ----

  # Filter by species and island, then show the distribution of body_mass_g
  output$main_plot <- renderPlot({
    req(input$species, input$island)
    penguins_sib %>%
      filter(
        species %in% input$species,
        island %in% input$island
      ) %>%
      ggplot(aes(x = body_mass_g)) +
      geom_histogram(bins = input$bins, fill = input$fill, color = "black") +
      labs(
        title = "Distribution of Body Mass (g)",
        x = "Body Mass (g)",
        y = "Count"
      ) +
      theme_minimal(base_size = 13)
  })

  # Bookmarking ----

  observe({
    # Trigger this observer every time an input changes
    # strip shiny related URL parameters
    shiny::reactiveValuesToList(input)
    setBookmarkExclude(c(
      "fill"
    ))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}
