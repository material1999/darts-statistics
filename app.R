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
    mutate("Day" = as.character(unlist(numbers))[3]) %>%
    mutate("Result team 1" = sub("\\*", "", `Result team 1`)) %>%
    mutate("Result team 2" = sub("\\*", "", `Result team 2`))
  
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
    .nav {
      margin-bottom: 20px;
    }
    .text-container {
      padding-left: 20px;
      padding-right: 20px;
      font-size: 2em;
    }
    .table-container {
      padding: 20px;
    }
    .table-container-2 {
      padding-left: 20px;
      padding-right: 20px;
      padding-bottom: 20px;
    }
  ")),
  
  titlePanel("Csuka Utca Invitational Masters - Statistics"),
  
  br(),
  
  sidebarLayout(
  
    sidebarPanel(width = 3,
      
      conditionalPanel(condition = "output.showInfo == true",
                       helpText("Season info"),
                       reactableOutput("infoTable")),
      
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
                  tabPanel("Current season", value = 1,
                           div(class = "text-container", p("Work in progress..."))),
                  tabPanel("Past seasons", value = 2,
                           div(class = "text-container", p("Work in progress..."))),
                  tabPanel("Round results", value = 3,
                           div(class = "text-container", strong("Group phase")),
                           div(class = "table-container", reactableOutput("roundTable")),
                           div(class = "text-container", strong("Knockout phase")),
                           div(class = "table-container", strong("TODO")),
                           div(class = "text-container", strong("Match results")),
                           div(class = "table-container-2", reactableOutput("roundMatches"))),
                  tabPanel("All time table", value = 4,
                           div(class = "text-container", p("Work in progress..."))),
                  tabPanel("Rivalries", value = 5,
                           div(class = "text-container", p("Work in progress..."))),
                  tabPanel("Player bio", value = 6,
                           div(class = "text-container", p("Work in progress...")))
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
  
  calculateSeasonInfo <- function() {
    
    info.table <- data.frame(
      Year = "asd",
      Rounds = "asd",
      `Macthes played` = 0,
      `Legs played` = 0,
      `Unique players` = 0,
      `180s` = 0,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    print(info.table)
    
    return(info.table)
  }
  
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
      result1 <- as.numeric(results.round$`Legs 1`[i])
      result2 <- as.numeric(results.round$`Legs 2`[i])

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
    
    # Sort the table based on score against each other if needed
    for (i in 2:nrow(results.table)) {
      if (results.table$Points[i-1] == results.table$Points[i] &
          results.table$`Leg Difference`[i-1] == results.table$`Leg Difference`[i] &
          results.table$`Legs Won`[i-1] == results.table$`Legs Won`[i]) {
        change1 = results.table$Player[i-1]
        change2 = results.table$Player[i]
        for (j in 1:nrow(results.round)) {
          player1 <- results.round$`Player 1`[j]
          player2 <- results.round$`Player 2`[j]
          result1 <- as.numeric(sub("\\*", "", results.round$`Legs 1`[j]))
          result2 <- as.numeric(sub("\\*", "", results.round$`Legs 2`[j]))
          if ((player1 == change1 & player2 == change2 & result2 > result1) |
              player1 == change2 & player2 == change1 & result1 > result2) {
            # print(paste(change1, "*change*", change2))
            tmp_row <- results.table[i-1,]
            results.table[i-1,] <- results.table[i,]
            results.table[i,] <- tmp_row
          }
        }
      }
    }
    
    # Add row numbers
    results.table <- results.table %>%
      mutate("#" = as.character(row_number())) %>%
      select("#", everything())
    
    return(results.table)
  }
  
  calculateRoundMatches <- function() {
    
    results.matches <- results %>%
      filter(Season == input$season, Round == input$round) %>% arrange(Phase)
    
    return(results.matches)
  }
  
  output$infoTable <- renderReactable({
    df <- data.frame(
      RowHeader = c("Header1", "Header2", "Header3"),
      Column1 = c(1, 2, 3),
      Column2 = c(4, 5, 6)
    )
    reactable(
      df,
      columns = list(
        RowHeader = colDef(name = " ", width = 150),
        Column1 = colDef(name = "Column 1"),
        Column2 = colDef(name = "Column 2")
      ),
      rownames = FALSE,
      highlight = TRUE
    )
    # reactable(calculateSeasonInfo())
  })
  
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
  
  output$roundMatches <- renderReactable({
    reactable(
      calculateRoundMatches(),
      columns = list(
        Phase = colDef(minWidth = 100, align = "center"),
        `Player 1` = colDef(minWidth = 100, align = "center"),
        `Player 2` = colDef(minWidth = 100, align = "center"),
        `Legs 1` = colDef(minWidth = 75, align = "center"),
        `Legs 2` = colDef(minWidth = 75, align = "center"),
        Round = colDef(maxWidth = 75, align = "center"),
        Season = colDef(maxWidth = 75, align = "center"),
        Month = colDef(maxWidth = 75, align = "center"),
        Day = colDef(maxWidth = 50, align = "center")
      ),
      searchable = TRUE, minRows = 10, highlight = TRUE, outlined = TRUE,
      striped = TRUE, sortable = FALSE, borderless = TRUE
    )
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
