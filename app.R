# Install and load packages ----------------------------------------------------

# install.packages(c("tidyverse", "ggplot2", "shiny", "gridExtra", "plotly",
#                    "treemapify", "readxl", "shinydashboard", "DT", "reactable"))

library(tidyverse)
library(ggplot2)
library(shiny)
library(gridExtra)
library(plotly)
library(treemapify)
library(readxl)
library(shinydashboard)
library(DT)
library(reactable)

# Load data --------------------------------------------------------------------

setwd("~/Documents/Programming/R/darts-statistics/")

results = tibble()
filenames <- list.files(path = "./results", pattern = "*.xlsx", full.names = TRUE)

for (filename in filenames) {
  numbers <- regmatches(filename, gregexpr("[[:digit:]]+", filename))
  results.current = read_excel(filename) %>%
    select("Phase", "Team 1", "Team 2", "Result team 1", "Result team 2") %>%
    mutate("Round" = as.character(unlist(numbers))[4]) %>%
    mutate("Season" = as.character(unlist(numbers))[1]) %>%
    mutate("Month" = as.character(unlist(numbers))[2]) %>%
    mutate("Day" = as.character(unlist(numbers))[3])
  colnames(results.current) <- c("phase", "player_1", "player_2", "legs_1", "legs_2",
                                 "round", "season", "month", "day")
  results <- bind_rows(results, results.current)
}

rm(filenames)
rm(filename)
rm(numbers)
rm(results.current)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  tags$style(HTML("
    table, th, td {
      border: none !important;
    }
    table {
      border-collapse: collapse !important;
    }
    .table-container {
      padding: 20px;
    }
  ")),
  
  titlePanel("Csuka Utca Invitational Masters - Statistics"),
  
  br(),
  
  sidebarLayout(
  
    sidebarPanel(width = 3,
      
      conditionalPanel(condition = "output.showInfo == true",
                       helpText("Season info"),
                       tableOutput("infoTable")),
      
      conditionalPanel(condition = "output.showSeason == true",
                       selectInput(
                         inputId = "season",
                         label = "Season:",
                         choices = unique(results$season),
                         selected = as.character(max(results$season))
                       )),
      
      conditionalPanel(condition = "output.showRound == true",
                       selectInput(
                         inputId = "round",
                         label = "Round:",
                         choices = NULL
                       )),
      
      conditionalPanel(condition = "output.showPlayer == true",
                       selectInput(
                         inputId = "player",
                         label = "Player:",
                         choices = sort(unique(c(results$player_2, results$player_1)))
                       )),
      
      conditionalPanel(condition = "output.showRival == true",
                       selectInput(
                         inputId = "rival",
                         label = "Rival:",
                         choices = sort(unique(c(results$player_2, results$player_1))),
                       )),
      
    ),
    
    mainPanel(width = 9,
      
      tabsetPanel(id = "plotTabs",
                  tabPanel("Current season", value = 1, htmlOutput("workInProgress1")),
                  tabPanel("Past seasons", value = 2, htmlOutput("workInProgress2")),
                  tabPanel("Round results", value = 3,
                           div(class = "table-container", reactableOutput("roundTable")),
                           div(class = "table-container", DTOutput("roundMatches"))),
                  tabPanel("All time table", value = 4, htmlOutput("workInProgress3")),
                  tabPanel("Rivalries", value = 5, htmlOutput("workInProgress4")),
                  tabPanel("Player bio", value = 6, htmlOutput("workInProgress5"))
      )
    )
  )
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  observe({
    selected <- input$player
    choices <- sort(unique(c(results$player_2, results$player_1)))
    updated_choices <- setdiff(choices, selected)
    updateSelectInput(session, "rival", choices = updated_choices)
  })
  
  observeEvent(input$season, {
    selected_season <- input$season
    round_choices <- sort(unique(results$round[results$season == selected_season]))
    max_round <- max(round_choices)
    updateSelectInput(session, "round", choices = round_choices, selected = max_round)
  })
  
  output$showInfo <- reactive({
    ifelse(input$plotTabs == 1 | input$plotTabs == 2, TRUE, FALSE)
  })
  outputOptions(output, "showInfo", suspendWhenHidden = FALSE)
  
  output$showSeason <- reactive({
    ifelse(input$plotTabs == 2 | input$plotTabs == 3, TRUE, FALSE)
  })
  outputOptions(output, "showSeason", suspendWhenHidden = FALSE)
  
  output$showRound <- reactive({
    ifelse(input$plotTabs == 3, TRUE, FALSE)
  })
  outputOptions(output, "showRound", suspendWhenHidden = FALSE)
  
  output$showPlayer <- reactive({
    ifelse(input$plotTabs == 5 | input$plotTabs == 6, TRUE, FALSE)
  })
  outputOptions(output, "showPlayer", suspendWhenHidden = FALSE)
  
  output$showRival <- reactive({
    ifelse(input$plotTabs == 5, TRUE, FALSE)
  })
  outputOptions(output, "showRival", suspendWhenHidden = FALSE)
  
  seasonInfoTable <- function() {
    renderTable({results[1:5, 1:2]}, colnames = FALSE)
  }
  
  roundTable <- function() {
    renderReactable(reactable(results[1:5, 1:2]))
  }
  
  roundResults <- reactive({
    results.round = results %>%
      filter(season == input$season, round == input$round) %>%
      arrange(phase)
  })
  
  output$workInProgress1 <- output$workInProgress2 <- output$workInProgress3 <-
    output$workInProgress4 <- output$workInProgress5 <-
    renderText("<br> Work in progress...<br>")
  
  output$infoTable <- seasonInfoTable()
  
  output$roundTable <- roundTable()
  
  output$roundMatches <- renderDT({
    datatable(roundResults(), options = list(pageLength = 10))
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
