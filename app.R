# Install and load packages ----------------------------------------------------

# install.packages(c("tidyverse", "ggplot2", "shiny", "gridExtra", "plotly",
#                    "treemapify", "readxl", "shinydashboard", "DT", "reactable",
#                    "bslib"))

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
library(bslib)

# Load data --------------------------------------------------------------------

setwd("~/Documents/Programming/R/darts-statistics/")

results <- data.frame()
filenames <- list.files(path = "./results", pattern = "*.xlsx", full.names = TRUE)

for (filename in filenames) {
  
  numbers <- regmatches(filename, gregexpr("[[:digit:]]+", filename))
  
  results.current <- read_excel(filename) %>%
    select("Phase", "Team 1", "Team 2", "Result team 1", "Result team 2", "Group") %>%
    mutate("Round" = as.character(unlist(numbers))[4]) %>%
    mutate("Season" = as.character(unlist(numbers))[1]) %>%
    mutate("Month" = as.character(unlist(numbers))[2]) %>%
    mutate("Day" = as.character(unlist(numbers))[3]) %>%
    mutate("Result team 1" = sub("\\*", "", `Result team 1`)) %>%
    mutate("Result team 2" = sub("\\*", "", `Result team 2`)) %>%
    mutate(Phase = if_else(Group == "Match B1", "Semi-final", Phase)) %>%
    mutate(Phase = if_else(Group == "Match B2", "Semi-final", Phase)) %>%
    mutate(Phase = if_else(Group == "Match B3", "Final", Phase)) %>%
    mutate(Phase = if_else(Group == "Match B4", "Bronze match", Phase)) %>%
    select(-"Group")
  
  colnames(results.current) <- c("Phase", "Player 1", "Player 2", "Legs 1", "Legs 2",
                                 "Round", "Season", "Month", "Day")
  
  results <- bind_rows(results, results.current)
  
}

results <- results[, c(1,2,4,5,3,6,7,8,9)]

filenames <- list.files(path = "./bonus", pattern = "*.xlsx", full.names = TRUE)

for (filename in filenames) {
  bonus = read_excel(filename)
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
    .title-container-top {
      padding-left: 20px;
      padding-right: 20px;
      font-size: 2em;
    }
    .title-container {
      padding-left: 20px;
      padding-right: 20px;
      padding-top: 20px;
      font-size: 2em;
    }
    .title-container-2 {
      padding-left: 5px;
      padding-right: 5px;
      padding-top: 20px;
      font-size: 2em;
    }
    .subtitle-container {
      padding-left: 5px;
      padding-right: 5px;
      padding-top: 20px;
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
    .subtable-container {
      padding-left: 0px;
      padding-right: 0px;
      padding-top: 20px;
      padding-bottom: 20px;
    }
    .hide-header {    
      display: none;
      visibility: collapse;
    }
    .padding-container {
      padding-bottom: 20px;
    }
  ")),
  
  titlePanel("Csuka Utca Invitational Masters - Statistics"),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 
                 conditionalPanel(condition = "output.showInfo == true",
                                  class = "padding-container",
                                  helpText("Season info"),
                                  reactableOutput("infoTable")
                                  ),
                 
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
                                    choices = sort(unique(c(results$`Player 2`,
                                                            results$`Player 1`)))
                                  )),
                 
                 conditionalPanel(condition = "output.showRival == true",
                                  selectInput(
                                    inputId = "rival",
                                    label = "Rival:",
                                    choices = sort(unique(c(results$`Player 2`,
                                                            results$`Player 1`))),
                                  )),
                 
    ),
    
    mainPanel(width = 9,
              
              tabsetPanel(id = "plotTabs",
                          tabPanel("Current season", value = 1,
                                   div(class = "title-container",
                                       p("Work in progress..."))),
                          tabPanel("Past seasons", value = 2,
                                   div(class = "title-container",
                                       p("Work in progress..."))),
                          tabPanel("Round results", value = 3,
                                   div(class = "title-container-top",
                                       strong("Group phase")),
                                   div(class = "table-container",
                                       reactableOutput("roundTable")),
                                   fluidRow(
                                     style = "margin:0px;",
                                     column(6,
                                            div(class = "title-container-2",
                                                strong("Knockout phase")),
                                            div(class = "subtitle-container", "Semi-finals"),
                                            div(class = "subtable-container",
                                                reactableOutput("semiFinalTable")),
                                            div(class = "subtitle-container", "Final"),
                                            div(class = "subtable-container",
                                                reactableOutput("finalTable")),
                                            div(class = "subtitle-container", "Bronze match"),
                                            div(class = "subtable-container",
                                                reactableOutput("bronzeTable"))
                                     ),
                                     column(6,
                                            div(class = "title-container-2",
                                                strong("Bonus points")),
                                            div(class = "subtable-container",
                                                reactableOutput("bonusPointsTable")),
                                            div(class = "title-container-2",
                                                strong("Round standings")),
                                            div(class = "subtable-container",
                                                reactableOutput("roundStandingsTable"))
                                     )
                                   ),
                                   div(class = "title-container",
                                       strong("Match results")),
                                   div(class = "table-container-2",
                                       reactableOutput("roundMatches"))),
                          tabPanel("All time table", value = 4,
                                   div(class = "title-container",
                                       p("Work in progress..."))),
                          tabPanel("Rivalries", value = 5,
                                   div(class = "title-container",
                                       p("Work in progress..."))),
                          tabPanel("Player bio", value = 6,
                                   div(class = "title-container",
                                       p("Work in progress...")))
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
    
    results.filtered = filter(results, Season == max(results$Season))
    bonus.filtered = filter(bonus, Season == max(bonus$Season))
    
    info.year = results.filtered$Season[1]
    info.rounds = max(as.numeric(results.filtered$Round))
    info.uniquePlayers = length(unique(c(results.filtered$`Player 1`,
                                         results.filtered$`Player 2`)))
    info.matchesPlayed = nrow(results.filtered)
    info.legsPlayed = sum(as.numeric(c(results.filtered$`Legs 1`,
                                       results.filtered$`Legs 2`)))
    
    info.highestCheckout = max(subset(bonus.filtered, as.numeric(Bonus) < 180)$Bonus)
    info.180s = length(which(as.numeric(bonus.filtered$Bonus) == 180))
    
    info.table <- data.frame(
      RowHeader = c(
        "Year",
        "Rounds",
        "Unique players",
        "Matches played",
        "Legs played",
        "Highest checkout",
        "180s"
      ),
      Stats = c(
        info.year,
        info.rounds,
        info.uniquePlayers,
        info.matchesPlayed,
        info.legsPlayed,
        info.highestCheckout,
        info.180s
      )
    )
    
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
      `Legs won` = rep(0, length(players)),
      `Legs lost` = rep(0, length(players)),
      `Leg difference` = rep(0, length(players)),
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
      results.table[results.table$Player == player1, "Legs won"] <-
        results.table[results.table$Player == player1, "Legs won"] + result1
      results.table[results.table$Player == player1, "Legs lost"] <-
        results.table[results.table$Player == player1, "Legs lost"] + result2
      results.table[results.table$Player == player2, "Legs won"] <-
        results.table[results.table$Player == player2, "Legs won"] + result2
      results.table[results.table$Player == player2, "Legs lost"] <-
        results.table[results.table$Player == player2, "Legs lost"] + result1
      
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
      mutate(`Leg difference` = `Legs won` - `Legs lost`)
    
    # Sort the table by Points, LegDifference, and LegsWon
    results.table <- results.table %>%
      arrange(desc(Points), desc(`Leg difference`), desc(`Legs won`))
    
    # Sort the table based on score against each other if needed
    for (i in 2:nrow(results.table)) {
      if (results.table$Points[i-1] == results.table$Points[i] &
          results.table$`Leg difference`[i-1] == results.table$`Leg difference`[i] &
          results.table$`Legs won`[i-1] == results.table$`Legs won`[i]) {
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
  
  calculateSemiFinalTable <- function() {
    
    results.semiFinals <- results %>%
      filter(Season == input$season, Round == input$round, Phase == "Semi-final") %>%
      select("Player 1", "Legs 1", "Legs 2", "Player 2")
    
    return(results.semiFinals)
  }
  
  calculateFinalTable <- function() {
    
    results.final <- results %>%
      filter(Season == input$season, Round == input$round, Phase == "Final") %>%
      select("Player 1", "Legs 1", "Legs 2", "Player 2")
    
    return(results.final)
  }
  
  calculateBronzeTable <- function() {
    
    results.bronze <- results %>%
      filter(Season == input$season, Round == input$round, Phase == "Bronze match") %>%
      select("Player 1", "Legs 1", "Legs 2", "Player 2")
    
    return(results.bronze)
  }
  
  calculateBonusPointsTable <- function() {
    
    bonus.round <- bonus %>%
      filter(Season == input$season, Round == input$round) %>%
      select("Player", "Bonus")
    
    return(bonus.round)
  }
  
  calculateRoundStandingsTable <- function() {
    
    
  }
  
  calculateRoundMatches <- function() {
    
    results.matches <- results %>%
      filter(Season == input$season, Round == input$round)
    
    results.matches <- results.matches %>%
      mutate("#" = as.character(row_number())) %>%
      select("#", everything())
    
    return(results.matches)
  }
  
  output$infoTable <- renderReactable({
    reactable(
      calculateSeasonInfo(),
      columns = list(
        RowHeader = colDef(headerClass = "hide-header", name = "", align = "left"),
        Stats = colDef(headerClass = "hide-header", name = "Stats", align = "right")
      ),
      rownames = FALSE, highlight = FALSE, striped = TRUE, sortable = FALSE,
      borderless = TRUE, outlined = TRUE,
      theme = reactableTheme(
        backgroundColor = "#DDDDDD"
      )
    )
  })
  
  output$roundTable <- renderReactable({
    reactable(
      calculateRoundTable(),
      columns = list(
        "#" = colDef(maxWidth = 50, align = "center"),
        Player = colDef(minWidth = 275),
        Points = colDef(maxWidth = 75, align = "center",
                        style = function(value) {
                          list(background = "lightgrey")
                        }
        ),
        Wins = colDef(maxWidth = 75, align = "center"),
        Losses = colDef(maxWidth = 75, align = "center"),
        `Legs won` = colDef(minWidth = 100, align = "center"),
        `Legs lost` = colDef(minWidth = 100, align = "center"),
        `Leg difference` = colDef(minWidth = 125, align = "center")
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$semiFinalTable  <- renderReactable({
    reactable(
      calculateSemiFinalTable(),
      columns = list(
        `Player 1` = colDef(minWidth = 100, align = "center"),
        `Legs 1` = colDef(minWidth = 50, align = "center",
                          style = function(value) {
                            list(background = "lightgrey")
                          }),
        `Legs 2` = colDef(minWidth = 50, align = "center",
                          style = function(value) {
                            list(background = "lightgrey")
                          }),
        `Player 2` = colDef(minWidth = 100, align = "center")
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$finalTable  <- renderReactable({
    reactable(
      calculateFinalTable(),
      columns = list(
        `Player 1` = colDef(minWidth = 100, align = "center"),
        `Legs 1` = colDef(minWidth = 50, align = "center",
                          style = function(value) {
                            list(background = "lightgrey")
                          }),
        `Legs 2` = colDef(minWidth = 50, align = "center",
                          style = function(value) {
                            list(background = "lightgrey")
                          }),
        `Player 2` = colDef(minWidth = 100, align = "center")
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$bronzeTable  <- renderReactable({
    reactable(
      calculateBronzeTable(),
      columns = list(
        `Player 1` = colDef(minWidth = 100, align = "center"),
        `Legs 1` = colDef(minWidth = 50, align = "center",
                          style = function(value) {
                            list(background = "lightgrey")
                          }),
        `Legs 2` = colDef(minWidth = 50, align = "center",
                          style = function(value) {
                            list(background = "lightgrey")
                          }),
        `Player 2` = colDef(minWidth = 100, align = "center")
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$bonusPointsTable  <- renderReactable({
    reactable(
      calculateBonusPointsTable(),
      columns = list(
        `Player` = colDef(minWidth = 100, align = "center"),
        `Bonus` = colDef(minWidth = 50, align = "center")
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$roundMatches <- renderReactable({
    reactable(
      calculateRoundMatches(),
      columns = list(
        "#" = colDef(maxWidth = 50, align = "center"),
        Phase = colDef(minWidth = 100),
        `Player 1` = colDef(minWidth = 100, align = "center"),
        `Legs 1` = colDef(minWidth = 75, align = "center",
                          style = function(value) {
                            list(background = "lightgrey")
                          }),
        `Legs 2` = colDef(minWidth = 75, align = "center",
                          style = function(value) {
                            list(background = "lightgrey")
                          }),
        `Player 2` = colDef(minWidth = 100, align = "center"),
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
