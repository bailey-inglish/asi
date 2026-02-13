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
    selectInput(
      inputId = "comp",
      label = "Compare to",
      choices = c("(None)", sdr$state),
      selected = "(None)"
    ),
    checkboxInput(
      inputId = "midterms",
      label = "Include midterms",
      value = TRUE
    ),
  ),
  mainPanel(
    plotOutput("graph"),
    hr(),
    strong("Sources"),
    p("Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren, Daniel Backman, Annie Chen, Grace Cooper, Stephanie Richards, Megan Schouweiler, and Michael Westberry. IPUMS CPS: Version 11.0 [dataset]. Minneapolis, MN: IPUMS, 2023. https://doi.org/10.18128/D030.V11.0"),
    p("Michael McDonald. 2024. “1980-2022 General Election Turnout Rates (v1.1).” https://election.lab.ufl.edu/dataset/1980-2022-general-election-turnout-rates-v1-1/"),
    strong("Note"),
    p("Information on the specifics of each state's implementation of SDR is available from the ", a("National Conference of State Legislatures", href = "https://www.ncsl.org/elections-and-campaigns/same-day-voter-registration"))
  )
)

server <- function(input, output) {
  construct_graph <- function() {
    if (input$midterms) {
      selected_years <- 1980 + 0:21 * 2
    } else {
      selected_years <- 1980 + 0:10 * 4
    }
    sdr_year <- filter(sdr, state == input$state | state == input$comp)
    post_title <- ""
    if (input$comp != "(None)" & input$state != input$comp) {
      post_title <- str_c(" and ", input$comp)
    }
    current_graph <-
      filter(
        turnout,
        State == input$state | State == input$comp,
        is.element(Year, selected_years)
      ) %>%
      left_join(
        filter(sdr, state == input$state | state == input$comp),
        by = c("State" = "state")
      ) %>%
      ggplot() +
        geom_line(
          aes(
            x = Year,
            y = Turnout * 100,
            col = Group,
            lty = State
          ),
          alpha = 0.8
        ) +
        geom_vline(
          aes(
            xintercept = sdr_start_year,
            lty = State
          )
        ) +
        labs(
          title = str_c("Voter Turnout Over Time in ", input$state, post_title),
          y = "Voter Turnout (%)",
          caption = str_c("| = Year SDR was Implemented")
        ) +
        ylim(
          c(0, 100)
        ) +
        xlim(
          c(1980, 2024)
        )
    output$graph <- renderPlot(current_graph)
  }
  observeEvent(input$state, {construct_graph()})
  observeEvent(input$midterms, {construct_graph()})
  observeEvent(input$comp, {construct_graph()})
}

shinyApp(ui = ui, server = server)