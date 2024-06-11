library(tidyverse)
library(shiny)

# setwd("C:/Users/baile/Desktop/asi/sdr/live")
turnout <- read_csv("turnout_by_year_state_group.csv") # asi/prep/pums_cleaner.r
sdr <- read_csv("sdr_states.csv") # https://www.ncsl.org/elections-and-campaigns/same-day-voter-registration (post 2008 only)

ui <- fluidPage(
  titlePanel("Effects of SDR on Subgroups"),
  hr(),
  sidebarPanel(
    selectInput(
      inputId = "state",
      label = "Select State",
      choices = sdr$state
    ),
    checkboxInput(
      inputId = "midterms",
      label = "Include midterms",
      value = TRUE
    )
  ),
  mainPanel(
    h4("Voter Turnout By Group And State Over Time"),
    plotOutput("graph")
  )
)

server <- function(input, output) {
  construct_graph <- function() {
    if (input$midterms) {
      selected_years <- 1980 + 0:21 * 2
    } else {
      selected_years <- 1980 + 0:10 * 4
    }
    sdr_year <- filter(sdr, state == input$state)$sdr_start_year
    current_graph <-
      filter(
        turnout,
        State == input$state,
        is.element(Year, selected_years)
      ) %>%
      ggplot() + 
        geom_line(
          aes(
            x = Year,
            y = Turnout * 100,
            col = Group
          )
        ) +
        geom_vline(
          xintercept = sdr_year
        ) +
        labs(
          y = "Voter Turnout (%)",
          caption = "| = Year SDR was Implemented"
        )
    output$graph <- renderPlot(current_graph)
  }
  observeEvent(input$state, {construct_graph()})
  observeEvent(input$midterms, {construct_graph()})
}

shinyApp(ui = ui, server = server)