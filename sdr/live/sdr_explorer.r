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
    )
  ),
  mainPanel(
    h4("Voter Turnout By Group Over Time"),
    plotOutput("graph")
  )
)

server <- function(input, output) {
  # every time the generate button is pressed, the graph is updated.
  observeEvent(
    input$state,
    {
      sdr_year <- filter(sdr, state == input$state)$sdr_start_year
      current_graph <-
        filter(
          turnout,
          State == input$state
        ) %>%
        ggplot() + 
          geom_line(
            aes(
              x = Year,
              y = Turnout,
              col = Group
            )
          ) +
          geom_vline(
            xintercept = sdr_year
          ) +
          scale_x_binned(
            breaks = c(2010 + 2 * 0:6, sdr_year)
          )
      output$graph <- renderPlot(current_graph)
    }
  )
}

shinyApp(ui = ui, server = server)