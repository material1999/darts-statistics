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
  colnames(results.current) <- c("Phase", "Player 1", "Player 2", "Legs 1", "Legs 2",
                                 "Round", "Season", "Month", "Day")
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
                         choices = unique(results$Season),
                         selected = as.character(max(results$Season))
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
                         choices = sort(unique(c(results$`Player 2`, results$`Player 1`)))
                       )),
      
      conditionalPanel(condition = "output.showRival == true",
                       selectInput(
                         inputId = "rival",
                         label = "Rival:",
                         choices = sort(unique(c(results$`Player 2`, results$`Player 1`))),
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
    choices <- sort(unique(c(results$`Player 2`, results$`Player 1`)))
    updated_choices <- setdiff(choices, selected)
    updateSelectInput(session, "rival", choices = updated_choices)
  })
  
  observeEvent(input$season, {
    selected_season <- input$season
    round_choices <- sort(unique(results$Round[results$Season == selected_season]))
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
  
  output$workInProgress1 <- output$workInProgress2 <- output$workInProgress3 <-
    output$workInProgress4 <- output$workInProgress5 <-
    renderText("<br> Work in progress...<br>")
  
  output$infoTable <- renderTable({
    results[1:5, 1:2]
  }, colnames = FALSE)
  
  calculateRoundTable <- function() {
    
    results.round <- results %>%
      filter(Season == input$season, Round == input$round, Phase == "Group phase")
    
    # Initialize player statistics
    players <- unique(c(results.round$`Player 1`, results.round$`Player 2`))
    results.table <- data.frame(
      Player = players,
      Points = rep(0, length(players)),
      Wins = rep(0, length(players)),
      Losses = rep(0, length(players)),
      `Legs Won` = rep(0, length(players)),
      `Legs Lost` = rep(0, length(players)),
      `Leg Difference` = rep(0, length(players)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Process each match and update statistics
    for (i in 1:nrow(results.round)) {
      player1 <- results.round$`Player 1`[i]
      player2 <- results.round$`Player 2`[i]
      result1 <- as.numeric(sub("\\*", "", results.round$`Legs 1`[i]))
      result2 <- as.numeric(sub("\\*", "", results.round$`Legs 2`[i]))

      # Update legs won and lost
      results.table[results.table$Player == player1, "Legs Won"] <-
        results.table[results.table$Player == player1, "Legs Won"] + result1
      results.table[results.table$Player == player1, "Legs Lost"] <-
        results.table[results.table$Player == player1, "Legs Lost"] + result2
      results.table[results.table$Player == player2, "Legs Won"] <-
        results.table[results.table$Player == player2, "Legs Won"] + result2
      results.table[results.table$Player == player2, "Legs Lost"] <-
        results.table[results.table$Player == player2, "Legs Lost"] + result1

      # Update points
      if (result1 > result2) {
        results.table[results.table$Player == player1, "Points"] <-
          results.table[results.table$Player == player1, "Points"] + 1
        results.table[results.table$Player == player1, "Wins"] <-
          results.table[results.table$Player == player1, "Wins"] + 1
        results.table[results.table$Player == player2, "Losses"] <-
          results.table[results.table$Player == player2, "Losses"] + 1
      } else {
        results.table[results.table$Player == player2, "Points"] <-
          results.table[results.table$Player == player2, "Points"] + 1
        results.table[results.table$Player == player2, "Wins"] <-
          results.table[results.table$Player == player2, "Wins"] + 1
        results.table[results.table$Player == player1, "Losses"] <-
          results.table[results.table$Player == player1, "Losses"] + 1
      }
    }

    # Calculate leg difference
    results.table <- results.table %>%
      mutate(`Leg Difference` = `Legs Won` - `Legs Lost`)

    # Sort the table by Points, LegDifference, and LegsWon
    results.table <- results.table %>%
      arrange(desc(Points), desc(`Leg Difference`), desc(`Legs Won`))
    
    
    
    results.table <- results.table %>%
      mutate("#" = as.character(row_number())) %>%
      select("#", everything())
    
    return(results.table)
  }
  
  output$roundTable <- renderReactable({
    reactable(
      calculateRoundTable(),
      columns = list(
        "#" = colDef(maxWidth = 50),
        Player = colDef(minWidth = 275),
        Points = colDef(maxWidth = 75, align = "center",
                        style = function(value) {
                          list(background = "lightgrey")
                        }
        ),
        Wins = colDef(maxWidth = 75, align = "center"),
        Losses = colDef(maxWidth = 75, align = "center"),
        `Legs Won` = colDef(minWidth = 100, align = "center"),
        `Legs Lost` = colDef(minWidth = 100, align = "center"),
        `Leg Difference` = colDef(minWidth = 125, align = "center")
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$roundMatches <- renderDT({
    datatable(
      results %>%
        filter(Season == input$season, Round == input$round) %>%
        arrange(Phase),
      options = list(pageLength = 10)
    )
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
