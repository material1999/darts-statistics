# Install and load packages ----------------------------------------------------

# install.packages(c("tidyverse", "ggplot2", "shiny", "gridExtra", "plotly",
#                    "treemapify", "readxl", "shinydashboard", "DT"))

library(tidyverse)
library(ggplot2)
library(shiny)
library(gridExtra)
library(plotly)
library(treemapify)
library(readxl)
library(shinydashboard)
library(DT)

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
      border: none !important; /* Hide table borders */
    }
    table {
      border-collapse: collapse !important;
    }
  ")),
  
  titlePanel("Csuka Utca Invitational Masters - Statistics"),
  
  br(),
  
  sidebarLayout(
  
    sidebarPanel(
      
      conditionalPanel(condition = "output.showInfo == true",
                       br(),
                       tableOutput("infoTable")),
      
      conditionalPanel(condition = "output.showSeason == true",
                       br(),
                       selectInput(
                         inputId = "season",
                         label = "Season:",
                         choices = unique(results$season),
                         selected = as.character(max(results$season))
                       )),
      
      conditionalPanel(condition = "output.showRound == true",
                       br(),
                       selectInput(
                         inputId = "round",
                         label = "Round:",
                         choices = unique(results$round),
                         selected = "1"
                       )),
      
    ),
    
    mainPanel(
      
      tabsetPanel(id = "plotTabs",
                  tabPanel("Current season", value = 1, plotOutput("plot1", height = "600px")),
                  tabPanel("Past seasons", value = 2, plotlyOutput("plot2", height = "600px")),
                  tabPanel("Round results", value = 3, DTOutput("plot3")),
                  tabPanel("All time table", value = 4, plotlyOutput("plot4", height = "600px")),
                  tabPanel("Rivalries", value = 5, plotlyOutput("plot5", height = "600px")),
                  tabPanel("Player bio", value = 6, plotOutput("plot6", height = "600px"))
      )
    )
  )
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
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
  
  seasonInfoTable <- function() {
    renderTable({results[1:5, 1:2]}, colnames = FALSE)
  }
  
  roundResults <- reactive({
    results.round = results %>%
      filter(season == input$season, round == input$round) %>%
      arrange(phase)
  })
  
  output$infoTable <- seasonInfoTable()
  
  output$plot3 <- renderDT({
    datatable(roundResults(), options = list(pageLength = 10))
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
