# Install and load packages ----------------------------------------------------

# install.packages(c("tidyverse", "ggplot2", "shiny", "gridExtra", "plotly",
#                    "treemapify", "readxl", "shinydashboard", "DT", "reactable",
#                    "bslib", "stringi"))

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
library(stringi)

# Load data --------------------------------------------------------------------

# setwd("~/Documents/Programming/R/darts-statistics/")

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

filename <- "./bonus/bonus_points.xlsx"
bonus = read_excel(filename)

filename <- "./bio/player_bio.xlsx"
bio = read_excel(filename)

rm(filenames)
rm(filename)
rm(numbers)
rm(results.current)

# Define UI --------------------------------------------------------------------

ui <- tagList(
  
  fluidRow(
    style = "margin: 0px; padding: 10px; border: 0px;
    display: flex; align-items: center; background-color: #e9e9e9;",
    column(9,
           align = "left",
           titlePanel(div(class = "title-panel",
                          "Csuka Utca Invitational Masters - Statistics"),
                      windowTitle = "Csuka Utca Invitational Masters - Statistics")
    ),
    column(3,
           align = "right",
           img(src = "favicon.png", width = "125px", height = "125px")
    )
  ),
  
  fluidPage(
    
    style = "background-color: #f5f5f5;",
    
    tags$head(
      tags$link(rel = "icon", href = "favicon.png")
    ),
    
    tags$style(HTML("
      body {
        overflow-y: scroll;
      }
      .nav {
        margin-bottom: 20px;
      }
      .title-container {
        padding-left: 10px;
        padding-right: 10px;
        padding-top: 20px;
        font-size: 2em;
      }
      .subtitle-container {
        padding-left: 0px;
        padding-right: 0px;
        padding-top: 20px;
        font-size: 2em;
      }
      .table-container {
        padding-left: 10px;
        padding-right: 10px;
        padding-bottom: 20px;
        padding-top: 20px;
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
      .title-panel {
        font-size: 1.3em;
        padding-bottom: 20px;
      }
      .bio {
        font-size: 0.5em;
      }
    ")),
    
    sidebarLayout(
      
      sidebarPanel(width = 3,
                   
                   style = "margin-left: 20px; margin-right: 10px; margin-top: 20px;
                   background-color: #efefef;",
                   
                   conditionalPanel(condition = "output.showSeasonInfo == true",
                                    class = "padding-container",
                                    helpText("Season info"),
                                    reactableOutput("seasonInfoTable")
                   ),
                   
                   conditionalPanel(condition = "output.showRoundInfo == true",
                                    class = "padding-container",
                                    helpText("Round info"),
                                    reactableOutput("roundInfoTable")
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
                
                style = "padding-left: 0px; padding-right: 20px; margin-top: 20px;
                background-color: #f5f5f5;",
                
                tabsetPanel(id = "plotTabs",
                            tabPanel("Current season", value = 1,
                                     div(class = "title-container",
                                         strong("Season standings")),
                                     div(class = "table-container",
                                         reactableOutput("standingsTable")),
                                     div(class = "title-container",
                                         strong("Points per round")),
                                     div(class = "table-container",
                                         reactableOutput("pointsPerRoundTable")),
                                     div(class = "title-container",
                                         strong("Interactive visuals")),
                                     div(class = "table-container",
                                         plotlyOutput("standingsPlot", height = "500px")),
                                     div(class = "table-container",
                                         plotlyOutput("overallPositionsPlot", height = "500px")),
                                     div(class = "table-container",
                                         plotlyOutput("positionsPlot", height = "500px"))),
                            tabPanel("Past seasons", value = 2,
                                     div(class = "title-container",
                                         p("Work in progress..."))),
                            tabPanel("Round results", value = 3,
                                     div(class = "title-container",
                                         strong("Final standings")),
                                     div(class = "table-container",
                                         reactableOutput("roundStandingsTable")),
                                     div(class = "title-container",
                                         strong("Group phase")),
                                     div(class = "table-container",
                                         reactableOutput("roundTable")),
                                     fluidRow(
                                       style = "padding: 10px;",
                                       column(6,
                                              div(class = "subtitle-container",
                                                  strong("Knockout phase")),
                                              div(class = "subtitle-container", "Semi-finals"),
                                              div(class = "subtable-container",
                                                  reactableOutput("semiFinalTable")),
                                              div(class = "subtitle-container", "Bronze match"),
                                              div(class = "subtable-container",
                                                  reactableOutput("bronzeTable")),
                                              div(class = "subtitle-container", "Final"),
                                              div(class = "subtable-container",
                                                  reactableOutput("finalTable"))
                                       ),
                                       column(6,
                                              div(class = "subtitle-container",
                                                  strong("Bonus points")),
                                              div(class = "subtitle-container", "Highest checkout"),
                                              div(class = "subtable-container",
                                                  reactableOutput("highestCheckoutTable")),
                                              div(class = "subtitle-container", "180s"),
                                              div(class = "subtable-container",
                                                  reactableOutput("oneEightyTable"))
                                       )
                                     ),
                                     div(class = "title-container",
                                         strong("Match results")),
                                     div(class = "table-container",
                                         reactableOutput("roundMatches"))),
                            tabPanel("All time table", value = 4,
                                     div(class = "title-container",
                                         p("Work in progress..."))),
                            tabPanel("Rivalries", value = 5,
                                     div(class = "title-container",
                                         p("Work in progress..."))),
                            tabPanel("Player bio", value = 6,
                                     fluidRow(
                                       style = "padding-left: 10px; padding-right: 10px;",
                                       column(4,
                                              div(class = "subtitle-container",
                                                  style = "padding-bottom: 20px;",
                                                  uiOutput("player_name")
                                                  ),
                                              uiOutput("player_image")
                                       ),
                                       column(3,
                                              div(class = "subtitle-container",
                                                  style = "padding-bottom: 20px;",
                                                  div(strong("Bio")),
                                                  strong(class = "bio", "Nickname"),
                                                  div(class = "bio", uiOutput("bio_nickname")),
                                                  strong(class = "bio", "Walk-On Music"),
                                                  div(class = "bio", uiOutput("bio_walkon")),
                                                  div(uiOutput("bio_walkon_audio"),
                                                      style = "padding-top: 20px;"
                                                      ),
                                                  strong(class = "bio", "Darts Used"),
                                                  div(class = "bio", uiOutput("bio_darts")),
                                                  strong(class = "bio", "Date of Birth"),
                                                  div(class = "bio", uiOutput("bio_birth")),
                                                  strong(class = "bio", "Hometown"),
                                                  div(class = "bio", uiOutput("bio_hometown")),
                                                  strong(class = "bio", "Career 180s"),
                                                  div(class = "bio", uiOutput("bio_oneeighty"))
                                                  )
                                       ),
                                       column(5,
                                              div(class = "subtitle-container",
                                                  style = "padding-bottom: 20px;",
                                                  div(strong("Stats"))
                                                  )
                                       )
                                     ))
                )
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
  
  output$player_name <- renderUI({
    strong(input$player)
  })
  
  output$bio_nickname <- renderUI({
    bio[bio$`Player` == input$player, ]$`Nickname`
  })
  
  output$bio_walkon <- renderUI({
    bio[bio$`Player` == input$player, ]$`Walk-On Music`
  })
  
  output$bio_walkon_audio <- renderUI({
    audio_file <- paste0("walk-on/",
                         stri_trans_general(bio[bio$`Player` == input$player, ]$`Walk-On Music`,
                                            "Latin-ASCII"),
                         ".mp3")
    tags$audio(
      src = audio_file,
      type = "audio/mp3",
      controls = "controls"
    )
  })
  
  output$bio_darts <- renderUI({
    bio[bio$`Player` == input$player, ]$`Darts Used`
  })
  
  output$bio_birth <- renderUI({
    bio[bio$`Player` == input$player, ]$`Date of Birth`
  })
  
  output$bio_hometown <- renderUI({
    bio[bio$`Player` == input$player, ]$`Hometown`
  })
  
  output$bio_oneeighty <- renderUI({
    bio[bio$`Player` == input$player, ]$`Career 180s`
  })
  
  output$player_image <- renderUI({
    tags$figure(
      tags$img(
        src = "avatar/" %>%
          paste0(stri_trans_general(tolower(input$player), "Latin-ASCII")) %>%
          gsub(" ", "_", .) %>%
          paste0(".jpg"),
        width = 250,
        alt = "No image found..."
      )
    )
  })
  
  output$showSeasonInfo <- reactive({
    ifelse(input$plotTabs == 1 | input$plotTabs == 2, TRUE, FALSE)
  })
  outputOptions(output, "showSeasonInfo", suspendWhenHidden = FALSE)
  
  output$showRoundInfo <- reactive({
    ifelse(input$plotTabs == 3, TRUE, FALSE)
  })
  outputOptions(output, "showRoundInfo", suspendWhenHidden = FALSE)
  
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
  
  calculateSeasonInfo <- function(season) {
    
    results.filtered = filter(results, Season == season)
    bonus.filtered = filter(bonus, Season == max(bonus$Season))
    
    info.year = results.filtered$Season[1]
    info.rounds = max(as.numeric(results.filtered$Round))
    info.uniquePlayers = length(unique(c(results.filtered$`Player 1`,
                                         results.filtered$`Player 2`)))
    
    winner1 = results.filtered %>%
      filter(Phase == "Final",`Legs 1` > `Legs 2`) %>%
      select(`Player 1`)
    colnames(winner1) <- c("Player")
    winner2 = results.filtered %>%
      filter(Phase == "Final",`Legs 2` > `Legs 1`) %>%
      select(`Player 2`)
    colnames(winner2) <- c("Player")
    info.uniqueWinners = nrow(unique(rbind(winner1, winner2)))
    
    info.matchesPlayed = nrow(results.filtered)
    info.legsPlayed = sum(as.numeric(c(results.filtered$`Legs 1`,
                                       results.filtered$`Legs 2`)))
    info.highestCheckout = max(0, as.numeric(unlist(subset(bonus.filtered,
                                                           as.numeric(Bonus) < 180)$Bonus)))
    info.180s = length(which(as.numeric(bonus.filtered$Bonus) == 180))
    
    info.table <- data.frame(
      RowHeader = c(
        "Year",
        "Rounds",
        "Unique players",
        "Unique winners",
        "Matches played",
        "Legs played",
        "Highest checkout",
        "180s"
      ),
      Stats = c(
        info.year,
        info.rounds,
        info.uniquePlayers,
        info.uniqueWinners,
        info.matchesPlayed,
        info.legsPlayed,
        info.highestCheckout,
        info.180s
      )
    )
    
    return(info.table)
  }
  
  calculateRoundInfo <- function(season, round) {
    
    results.filtered = filter(results, Season == season, Round == round)
    bonus.filtered = filter(bonus, Season == season, Round == round)
    
    info.date = as.character(
      paste(
        sep = "-",
        results.filtered$Season[1],
        results.filtered$Month[1],
        results.filtered$Day[1])
    )
    info.uniquePlayers = length(unique(c(results.filtered$`Player 1`,
                                         results.filtered$`Player 2`)))
    info.matchesPlayed = nrow(results.filtered)
    info.legsPlayed = sum(as.numeric(c(results.filtered$`Legs 1`,
                                       results.filtered$`Legs 2`)))
    
    info.highestCheckout = max(0, subset(bonus.filtered, as.numeric(Bonus) < 180)$Bonus)
    info.180s = length(which(as.numeric(bonus.filtered$Bonus) == 180))
    
    info.table <- data.frame(
      RowHeader = c(
        "Date",
        "Players",
        "Matches played",
        "Legs played",
        "Highest checkout",
        "180s"
      ),
      Stats = c(
        info.date,
        info.uniquePlayers,
        info.matchesPlayed,
        info.legsPlayed,
        info.highestCheckout,
        info.180s
      )
    )
    
    return(info.table)
  }
  
  calculateRoundTable <- function(season, round) {
    
    results.round <- results %>%
      filter(Season == season, Round == round, Phase == "Group phase")
    
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
    
    for (i in 1:nrow(results.round)) {
      player1 <- results.round$`Player 1`[i]
      player2 <- results.round$`Player 2`[i]
      result1 <- as.numeric(results.round$`Legs 1`[i])
      result2 <- as.numeric(results.round$`Legs 2`[i])
      
      results.table[results.table$Player == player1, "Legs won"] <-
        results.table[results.table$Player == player1, "Legs won"] + result1
      results.table[results.table$Player == player1, "Legs lost"] <-
        results.table[results.table$Player == player1, "Legs lost"] + result2
      results.table[results.table$Player == player2, "Legs won"] <-
        results.table[results.table$Player == player2, "Legs won"] + result2
      results.table[results.table$Player == player2, "Legs lost"] <-
        results.table[results.table$Player == player2, "Legs lost"] + result1
      
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
    
    results.table <- results.table %>%
      mutate(`Leg difference` = `Legs won` - `Legs lost`)
    
    results.table <- results.table %>%
      arrange(desc(Points), desc(`Leg difference`), desc(`Legs won`))
    
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
            tmp_row <- results.table[i-1,]
            results.table[i-1,] <- results.table[i,]
            results.table[i,] <- tmp_row
          }
        }
      }
    }
    
    results.table <- results.table %>%
      mutate("#" = as.character(row_number())) %>%
      select("#", everything())
    
    return(results.table)
  }
  
  calculateSemiFinalTable <- function(season, round) {
    
    results.semiFinals <- results %>%
      filter(Season == season, Round == round, Phase == "Semi-final") %>%
      select("Player 1", "Legs 1", "Legs 2", "Player 2")
    
    return(results.semiFinals)
  }
  
  calculateFinalTable <- function(season, round) {
    
    results.final <- results %>%
      filter(Season == season, Round == round, Phase == "Final") %>%
      select("Player 1", "Legs 1", "Legs 2", "Player 2")
    
    return(results.final)
  }
  
  calculateBronzeTable <- function(season, round) {
    
    results.bronze <- results %>%
      filter(Season == season, Round == round, Phase == "Bronze match") %>%
      select("Player 1", "Legs 1", "Legs 2", "Player 2")
    
    return(results.bronze)
  }
  
  calculateHighestCheckoutTable <- function(season, round) {
    
    bonus.round.checkout <- bonus %>%
      filter(Season == season, Round == round, as.numeric(Bonus) < 180) %>%
      select("Player", "Bonus")
    
    colnames(bonus.round.checkout) <- c("Player", "Checkout")
    
    return(bonus.round.checkout)
  }
  
  calculateOneEightyTable <- function(season, round) {
    
    bonus.round.oneEighty <- bonus %>%
      filter(Season == season, Round == round, as.numeric(Bonus) == 180) %>%
      select("Player", "Bonus") %>%
      group_by(Player) %>%
      summarise("180s" = n()) %>%
      arrange(desc(`180s`))
    
    return(bonus.round.oneEighty)
  }
  
  calculateRoundStandingsTable <- function(season, round) {
    
    results.group <- calculateRoundTable(season, round)
    results.semi <- results %>%
      filter(Season == season, Round == round, Phase == "Semi-final")
    results.bronze <- results %>%
      filter(Season == season, Round == round, Phase == "Bronze match")
    results.final <- results %>%
      filter(Season == season, Round == round, Phase == "Final")
    results.checkout <- calculateHighestCheckoutTable(season, round)
    results.oneEighty <- calculateOneEightyTable(season, round)
    
    players <- results.group$Player
    results.table <- data.frame(
      Player = players,
      `Matches won` = rep(0, length(players)),
      `Leg difference` = rep(0, length(players)),
      `Legs won` = rep(0, length(players)),
      Points = rep(0, length(players)),
      `Bonus points` = rep(0, length(players)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    if (results.final$`Legs 1` > results.final$`Legs 2`) {
      results.table[results.table$Player == results.final$`Player 1`, "Points"] <- 8
      results.table[results.table$Player == results.final$`Player 2`, "Points"] <- 6
    } else {
      results.table[results.table$Player == results.final$`Player 2`, "Points"] <- 8
      results.table[results.table$Player == results.final$`Player 1`, "Points"] <- 6
    }
    
    for (i in 1:nrow(results.semi)) {
      if (results.semi$`Legs 1`[i] > results.semi$`Legs 2`[i]) {
        results.table[results.table$Player == results.semi$`Player 2`[i], "Points"] <- 4
      } else {
        results.table[results.table$Player == results.semi$`Player 1`[i], "Points"] <- 4
      }
    }
    
    results.table[results.table$Player == results.group$`Player`[5], "Points"] <- 2
    results.table[results.table$Player == results.group$`Player`[6], "Points"] <- 1
    
    for (i in 1:nrow(results.table)) {
      results.table[results.table$Player == results.group$`Player`[i], "Matches won"] <-
        results.group[results.table$Player == results.group$`Player`[i], "Wins"]
      results.table[results.table$Player == results.group$`Player`[i], "Leg difference"] <-
        results.group[results.table$Player == results.group$`Player`[i], "Leg difference"]
      results.table[results.table$Player == results.group$`Player`[i], "Legs won"] <-
        results.group[results.table$Player == results.group$`Player`[i], "Legs won"]
    }
    
    for (i in 1:nrow(results.semi)) {
      results.table[results.table$Player == results.semi$`Player 1`[i], "Legs won"] <-
        results.table[results.table$Player == results.semi$`Player 1`[i], "Legs won"] +
        as.numeric(results.semi$`Legs 1`[i])
      results.table[results.table$Player == results.semi$`Player 2`[i], "Legs won"] <-
        results.table[results.table$Player == results.semi$`Player 2`[i], "Legs won"] +
        as.numeric(results.semi$`Legs 2`[i])
      results.table[results.table$Player == results.semi$`Player 1`[i], "Leg difference"] <-
        results.table[results.table$Player == results.semi$`Player 1`[i], "Leg difference"] +
        as.numeric(results.semi$`Legs 1`[i]) -
        as.numeric(results.semi$`Legs 2`[i])
      results.table[results.table$Player == results.semi$`Player 2`[i], "Leg difference"] <-
        results.table[results.table$Player == results.semi$`Player 2`[i], "Leg difference"] +
        as.numeric(results.semi$`Legs 2`[i]) -
        as.numeric(results.semi$`Legs 1`[i])
      if (results.semi$`Legs 1`[i] > results.semi$`Legs 2`[i]) {
        results.table[results.table$Player == results.semi$`Player 1`[i], "Matches won"] <-
          results.table[results.table$Player == results.semi$`Player 1`[i], "Matches won"] + 1
      } else {
        results.table[results.table$Player == results.semi$`Player 2`[i], "Matches won"] <-
          results.table[results.table$Player == results.semi$`Player 2`[i], "Matches won"] + 1
      }
    }
    
    if (nrow(results.bronze) == 1) {
      results.table[results.table$Player == results.bronze$`Player 1`, "Legs won"] <-
        results.table[results.table$Player == results.bronze$`Player 1`, "Legs won"] +
        as.numeric(results.bronze$`Legs 1`)
      results.table[results.table$Player == results.bronze$`Player 2`, "Legs won"] <-
        results.table[results.table$Player == results.bronze$`Player 2`, "Legs won"] +
        as.numeric(results.bronze$`Legs 2`)
      results.table[results.table$Player == results.bronze$`Player 1`, "Leg difference"] <-
        results.table[results.table$Player == results.bronze$`Player 1`, "Leg difference"] +
        as.numeric(results.bronze$`Legs 1`) -
        as.numeric(results.bronze$`Legs 2`)
      results.table[results.table$Player == results.bronze$`Player 2`, "Leg difference"] <-
        results.table[results.table$Player == results.bronze$`Player 2`, "Leg difference"] +
        as.numeric(results.bronze$`Legs 2`) -
        as.numeric(results.bronze$`Legs 1`)
      if (results.bronze$`Legs 1` > results.bronze$`Legs 2`) {
        results.table[results.table$Player == results.bronze$`Player 1`, "Matches won"] <-
          results.table[results.table$Player == results.bronze$`Player 1`, "Matches won"] + 1
      } else {
        results.table[results.table$Player == results.bronze$`Player 2`, "Matches won"] <-
          results.table[results.table$Player == results.bronze$`Player 2`, "Matches won"] + 1
      }
    }
    
    results.table[results.table$Player == results.final$`Player 1`, "Legs won"] <-
      results.table[results.table$Player == results.final$`Player 1`, "Legs won"] +
      as.numeric(results.final$`Legs 1`)
    results.table[results.table$Player == results.final$`Player 2`, "Legs won"] <-
      results.table[results.table$Player == results.final$`Player 2`, "Legs won"] +
      as.numeric(results.final$`Legs 2`)
    results.table[results.table$Player == results.final$`Player 1`, "Leg difference"] <-
      results.table[results.table$Player == results.final$`Player 1`, "Leg difference"] +
      as.numeric(results.final$`Legs 1`) -
      as.numeric(results.final$`Legs 2`)
    results.table[results.table$Player == results.final$`Player 2`, "Leg difference"] <-
      results.table[results.table$Player == results.final$`Player 2`, "Leg difference"] +
      as.numeric(results.final$`Legs 2`) -
      as.numeric(results.final$`Legs 1`)
    if (results.final$`Legs 1` > results.final$`Legs 2`) {
      results.table[results.table$Player == results.final$`Player 1`, "Matches won"] <-
        results.table[results.table$Player == results.final$`Player 1`, "Matches won"] + 1
    } else {
      results.table[results.table$Player == results.final$`Player 2`, "Matches won"] <-
        results.table[results.table$Player == results.final$`Player 2`, "Matches won"] + 1
    }
    
    results.table[results.table$Player == results.checkout$`Player`, "Bonus points"] <- 1
    
    if (nrow(results.oneEighty) > 0) {
      for (i in 1:nrow(results.oneEighty)) {
        results.table[results.table$Player == results.oneEighty$`Player`[i], "Bonus points"] <-
          results.table[results.table$Player == results.oneEighty$`Player`[i], "Bonus points"] +
          2 * results.oneEighty$`180s`[i]
      }
    }
    
    results.table <- results.table %>%
      arrange(desc(Points),
              desc(`Matches won`),
              desc(`Leg difference`),
              desc(`Legs won`),
              desc(`Bonus points`))
    
    if (nrow(results.bronze) == 1) {
      if (results.bronze$`Legs 1` > results.bronze$`Legs 2` &
          results.table$`Player`[4] == results.bronze$`Player 1`) {
        tmp_row <- results.table[3,]
        results.table[3,] <- results.table[4,]
        results.table[4,] <- tmp_row
      } else if (results.bronze$`Legs 2` > results.bronze$`Legs 1` &
                 results.table$`Player`[4] == results.bronze$`Player 2`) {
        tmp_row <- results.table[3,]
        results.table[3,] <- results.table[4,]
        results.table[4,] <- tmp_row
      }
    } else if (results.group[results.group$Player == results.table$Player[3], "#"] >
               results.group[results.group$Player == results.table$Player[4], "#"]) {
      tmp_row <- results.table[3,]
      results.table[3,] <- results.table[4,]
      results.table[4,] <- tmp_row
    }
    
    results.table <- results.table %>%
      mutate("#" = as.character(row_number())) %>%
      select("#", everything())
    
    return(results.table)
  }
  
  calculateStandingsTable <- function(season) {
    
    results.season <- results %>%
      filter(Season == season)
    
    players <- unique(c(results.season$`Player 1`, results.season$`Player 2`))
    results.table <- data.frame(
      Player = players,
      `Nights won` = rep(0, length(players)),
      `Top-4 finishes` = rep(0, length(players)),
      `Matches won` = rep(0, length(players)),
      `Leg difference` = rep(0, length(players)),
      `Legs won` = rep(0, length(players)),
      Points = rep(0, length(players)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    for (r in unique(results.season$`Round`)) {
      results.current <- calculateRoundStandingsTable(season, r)
      for (j in 1:nrow(results.current)) {
        if (j == 1) {
          results.table[results.table$Player == results.current$Player[j], "Nights won"] <-
            results.table[results.table$Player == results.current$Player[j], "Nights won"] + 1
        }
        if (j <= 4) {
          results.table[results.table$Player == results.current$Player[j], "Top-4 finishes"] <-
            results.table[results.table$Player == results.current$Player[j], "Top-4 finishes"] + 1
        }
        results.table[results.table$Player == results.current$Player[j], "Points"] <-
          results.table[results.table$Player == results.current$Player[j], "Points"] +
          results.current$Points[j] + results.current$`Bonus points`[j]
        results.table[results.table$Player == results.current$Player[j], "Legs won"] <-
          results.table[results.table$Player == results.current$Player[j], "Legs won"] +
          results.current$`Legs won`[j]
        results.table[results.table$Player == results.current$Player[j], "Leg difference"] <-
          results.table[results.table$Player == results.current$Player[j], "Leg difference"] +
          results.current$`Leg difference`[j]
        results.table[results.table$Player == results.current$Player[j], "Matches won"] <-
          results.table[results.table$Player == results.current$Player[j], "Matches won"] +
          results.current$`Matches won`[j]
      }
    }
    
    results.table <- results.table %>%
      arrange(desc(Points),
              desc(`Nights won`),
              desc(`Top-4 finishes`),
              desc(`Matches won`),
              desc(`Leg difference`),
              desc(`Legs won`))
    
    results.table <- results.table %>%
      mutate("#" = as.character(row_number())) %>%
      select("#", everything())
    
    return(results.table)
  }
  
  calculatePointsPerRoundTable <- function(season) {
    
    results.season <- results %>%
      filter(Season == season)
    
    players <- sort(unique(c(results.season$`Player 1`, results.season$`Player 2`)))
    results.table <- data.frame(
      Player = players,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    for (r in unique(results.season$`Round`)) {
      results.current <- calculateRoundStandingsTable(season, r)
      new_column_name <- paste("R", as.character(r), sep = "")
      results.table[, new_column_name] <- rep(0, length(players))
      for (j in 1:nrow(results.current)) {
        results.table[results.table$Player == results.current$Player[j], new_column_name] <-
          results.table[results.table$Player == results.current$Player[j], new_column_name] +
          results.current$Points[j] + results.current$`Bonus points`[j]
      }
    }
    
    results.table <- results.table %>%
      rowwise() %>%
      mutate(Total = sum(c_across(starts_with("R")), na.rm = TRUE)) %>%
      mutate(Average = round(mean(c_across(starts_with("R")), na.rm = TRUE), 2)) %>%
      arrange(desc(Total))
    
    return(results.table)
    
  }
  
  createStandingsPlot <- function(season) {
    
    results.season <- results %>%
      filter(Season == season)
    
    players <- sort(unique(c(results.season$`Player 1`, results.season$`Player 2`)))
    
    results.table <- data.frame(
      Player = players,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    results.table <- results.table %>%
      mutate(`Round 0` = 0)
    
    cumulative_points <- rep(0, length(players))
    
    for (r in unique(results.season$`Round`)) {
      round_column_name <- paste("Round", r, sep = " ")
      results.table[, round_column_name] <- NA
    }
    
    for (r in unique(results.season$`Round`)) {
      results.current <- calculateRoundStandingsTable(season, r)
      round_column_name <- paste("Round", r, sep = " ")
      
      for (j in 1:nrow(results.current)) {
        round_points <- results.current$Points[j] + results.current$`Bonus points`[j]
        
        player_index <- which(results.table$Player == results.current$Player[j])
        cumulative_points[player_index] <- cumulative_points[player_index] + round_points
        
        results.table[player_index, round_column_name] <- cumulative_points[player_index]
      }
    }
    
    my_custom_palette <- c("#1b9e77", "#d95f02", "#1f78b4", "#e7298a", "#66a61e", "#e6ab02",
                           "#ff0000", "#666666", "#7570b3", "#b2df8a", "#fb9a99")
    
    results.long <- results.table %>%
      pivot_longer(cols = starts_with("Round"), names_to = "Round", values_to = "TotalPoints") %>%
      mutate(Round = as.numeric(gsub("Round ", "", Round))) %>%
      filter(!is.na(TotalPoints))
    
    max_round <- max(results.long$Round, na.rm = TRUE)
    max_points <- max(results.long$TotalPoints, na.rm = TRUE)
    
    p <- ggplot(results.long, aes(x = Round, y = TotalPoints, group = Player, color = Player,
                                  text = paste("Player:", Player,
                                               "<br>Round:", Round,
                                               "<br>Total Points:", TotalPoints))) +
      geom_line(linewidth = 0.75) +
      geom_point(size = 1.5) +
      theme_minimal() +
      labs(title = "Total Points After Each Round",
           x = "Rounds",
           y = "Total Points") +
      scale_x_continuous(breaks = 0:max_round, limits = c(0, max_round)) +
      scale_y_continuous(limits = c(0, max_points)) +
      scale_color_manual(values = my_custom_palette) +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = c("text")) %>%
      layout(hovermode = "closest")
  }
  
  createOverallPositionsPlot <- function(season) {
    
    results.season <- results %>%
      filter(Season == season)
    
    players <- sort(unique(c(results.season$`Player 1`, results.season$`Player 2`)))
    
    results.table <- data.frame(
      Player = players,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    cumulative_points <- rep(0, length(players))
    
    overall_ranking_history <- data.frame()
    
    for (r in unique(results.season$`Round`)) {
      results.current <- calculateRoundStandingsTable(season, r)
      
      for (j in 1:nrow(results.current)) {
        round_points <- results.current$Points[j] + results.current$`Bonus points`[j]
        
        player_index <- which(results.table$Player == results.current$Player[j])
        cumulative_points[player_index] <- cumulative_points[player_index] + round_points
        
        results.table[player_index, paste("Round", r, sep = " ")] <- cumulative_points[player_index]
      }
      
      current_overall_ranks <- data.frame(Player = results.table$Player,
                                          Cumulative_Points = cumulative_points) %>%
        arrange(desc(Cumulative_Points)) %>%
        mutate(Overall_Rank = row_number())
      
      current_overall_ranks$Round <- r
      overall_ranking_history <- rbind(overall_ranking_history, current_overall_ranks)
    }
    
    my_custom_palette <- c("#1b9e77", "#d95f02", "#1f78b4", "#e7298a", "#66a61e", "#e6ab02",
                           "#ff0000", "#666666", "#7570b3", "#b2df8a", "#fb9a99")
    
    p <- ggplot(overall_ranking_history, aes(x = Round, y = Overall_Rank, group = Player, color = Player, 
                                             text = paste("Player:", Player, 
                                                          "<br>Round:", Round, 
                                                          "<br>Overall Ranking:", Overall_Rank))) +
      geom_line(linewidth = 0.75) +
      geom_point(size = 1.5) +
      theme_minimal() +
      labs(title = "Overall Rankings After Each Round", 
           x = "Rounds", 
           y = "Overall Rankings") +
      scale_x_discrete(breaks = unique(overall_ranking_history$Round)) +
      scale_y_reverse(breaks = 1:length(players), limits = c(length(players), 1)) +
      scale_color_manual(values = my_custom_palette) +
      theme(legend.position = "right", 
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = c("text")) %>%
      layout(hovermode = "closest")
  }
  
  createPositionsPlot <- function(season) {
    
    results.season <- results %>%
      filter(Season == season)
    
    players <- sort(unique(c(results.season$`Player 1`, results.season$`Player 2`)))
    results.table <- data.frame(
      Player = players,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    for (r in unique(results.season$`Round`)) {
      round_column_name <- paste("Round", r, sep = " ")
      results.table[, round_column_name] <- NA
    }
    
    for (r in unique(results.season$`Round`)) {
      results.current <- calculateRoundStandingsTable(season, r)
      round_column_name <- paste("Round", r, sep = " ")
      
      for (j in 1:nrow(results.current)) {
        results.table[results.table$Player == results.current$Player[j], round_column_name] <- j
      }
    }
    
    my_custom_palette <- c("#1b9e77", "#d95f02", "#1f78b4", "#e7298a", "#66a61e", "#e6ab02",
                           "#ff0000", "#666666", "#7570b3", "#b2df8a", "#fb9a99")
    
    results.long <- results.table %>%
      pivot_longer(cols = starts_with("Round"), names_to = "Round", values_to = "Position") %>%
      mutate(Round = as.numeric(gsub("Round ", "", Round))) %>%
      filter(!is.na(Position))
    
    max_round <- max(results.long$Round, na.rm = TRUE)
    max_position <- max(results.long$Position, na.rm = TRUE)
    
    p <- ggplot(results.long, aes(x = Round, y = Position, group = Player, color = Player, 
                                  text = paste("Player:", Player,
                                               "<br>Round:", Round,
                                               "<br>Ranking:", Position))) +
      geom_line(linewidth = 0.75) +
      geom_point(size = 1.5) +
      theme_minimal() +
      labs(title = "Round Rankings",
           x = "Rounds",
           y = "Rankings") +
      scale_x_continuous(breaks = 1:max_round, limits = c(1, max_round)) +
      scale_y_reverse(breaks = 1:max_position, limits = c(max_position, 1)) +
      scale_color_manual(values = my_custom_palette) +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = c("text")) %>%
      layout(hovermode = "closest")
  }
  
  calculateRoundMatches <- function(season, round) {
    
    results.matches <- results %>%
      filter(Season == season, Round == round)
    
    results.matches <- results.matches %>%
      mutate("#" = as.character(row_number())) %>%
      select("#", everything())
    
    return(results.matches)
  }
  
  output$seasonInfoTable <- renderReactable({
    reactable(
      calculateSeasonInfo(max(results$Season)),
      columns = list(
        RowHeader = colDef(headerClass = "hide-header", name = "", align = "left"),
        Stats = colDef(headerClass = "hide-header", name = "Stats", align = "right")
      ),
      rownames = FALSE, highlight = FALSE, striped = TRUE, sortable = FALSE,
      borderless = TRUE, outlined = TRUE,
      theme = reactableTheme(
        backgroundColor = "#f7f7f7"
      )
    )
  })
  
  output$roundTable <- renderReactable({
    reactable(
      calculateRoundTable(input$season, input$round),
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
      calculateSemiFinalTable(input$season, input$round),
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
      calculateFinalTable(input$season, input$round),
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
      calculateBronzeTable(input$season, input$round),
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
  
  output$roundInfoTable <- renderReactable({
    reactable(
      calculateRoundInfo(input$season, input$round),
      columns = list(
        RowHeader = colDef(headerClass = "hide-header", name = "", align = "left"),
        Stats = colDef(headerClass = "hide-header", name = "Stats", align = "right")
      ),
      rownames = FALSE, highlight = FALSE, striped = TRUE, sortable = FALSE,
      borderless = TRUE, outlined = TRUE,
      theme = reactableTheme(
        backgroundColor = "#f7f7f7"
      )
    )
  })
  
  output$highestCheckoutTable  <- renderReactable({
    reactable(
      calculateHighestCheckoutTable(input$season, input$round),
      columns = list(
        `Player` = colDef(minWidth = 100, align = "center"),
        `Checkout` = colDef(minWidth = 50, align = "center",
                            style = function(value) {
                              list(background = "lightgrey")
                            })
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$oneEightyTable  <- renderReactable({
    reactable(
      calculateOneEightyTable(input$season, input$round),
      columns = list(
        `Player` = colDef(minWidth = 100, align = "center"),
        `180s` = colDef(minWidth = 50, align = "center",
                        style = function(value) {
                          list(background = "lightgrey")
                        })
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$roundStandingsTable <- renderReactable({
    reactable(
      calculateRoundStandingsTable(input$season, input$round),
      rowStyle = function(index) {
        if (index == 1) {
          list(background = "#f4c136")
        } else if (index == 2) {
          list(background = "#c0c0c0")
        } else if (index == 3) {
          list(background = "#cd7f32")
        }
      },
      columns = list(
        "#" = colDef(maxWidth = 50, align = "center"),
        Player = colDef(minWidth = 275),
        `Matches won` = colDef(maxWidth = 125, align = "center"),
        `Leg difference` = colDef(maxWidth = 125, align = "center"),
        `Legs won` = colDef(maxWidth = 100, align = "center"),
        Points = colDef(maxWidth = 75, align = "center",
                        style = function(value) {
                          list(background = "lightgrey")
                        }
        ),
        `Bonus points` = colDef(maxWidth = 125, align = "center",
                                style = function(value) {
                                  list(background = "#b8b8d2")
                                }
        )
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$standingsTable <- renderReactable({
    reactable(
      calculateStandingsTable(max(results$Season)),
      defaultPageSize = 15,
      rowStyle = function(index) {
        if (index == 1) {
          list(background = "#f4c136")
        } else if (index == 2) {
          list(background = "#c0c0c0")
        } else if (index == 3) {
          list(background = "#cd7f32")
        }
      },
      columns = list(
        "#" = colDef(maxWidth = 50, align = "center"),
        Player = colDef(minWidth = 275),
        `Nights won` = colDef(minWidth = 100, align = "center"),
        `Top-4 finishes` = colDef(minWidth = 125, align = "center"),
        `Matches won` = colDef(minWidth = 125, align = "center"),
        `Leg difference` = colDef(minWidth = 125, align = "center"),
        `Legs won` = colDef(minWidth = 100, align = "center"),
        Points = colDef(maxWidth = 75, align = "center",
                        style = function(value) {
                          list(background = "lightgrey")
                        }
        )
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$pointsPerRoundTable <- renderReactable({
    reactable(
      calculatePointsPerRoundTable(max(results$Season)),
      defaultPageSize = 15,
      defaultColDef = colDef(
        align = "center",
        maxWidth = 75
      ),
      columns = list(
        Player = colDef(minWidth = 150, maxWidth = 1500, align = "left"),
        Total = colDef(maxWidth = 75,
                       style = function(value) {
                         list(background = "lightgrey")
                       }
        ),
        Average = colDef(maxWidth = 75,
                         style = function(value) {
                           list(background = "#b8b8d2")
                         }
        )
      ),
      highlight = TRUE, outlined = TRUE, striped = TRUE, sortable = FALSE,
      borderless = TRUE
    )
  })
  
  output$standingsPlot <- renderPlotly({
    createStandingsPlot(max(results$Season))
  })
  
  output$overallPositionsPlot <- renderPlotly({
    createOverallPositionsPlot(max(results$Season))
  })
  
  output$positionsPlot <- renderPlotly({
    createPositionsPlot(max(results$Season))
  })
  
  output$roundMatches <- renderReactable({
    reactable(
      calculateRoundMatches(input$season, input$round),
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
