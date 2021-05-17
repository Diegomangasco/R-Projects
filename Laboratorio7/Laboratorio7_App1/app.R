#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
    fluidRow(
      column(12, h2("Statistiche Serie A"))),
    br(),
    fluidRow(
      column(6, textOutput("firstSummary")),
      column(6, textOutput("secondSummary"))),
    br(),
    fluidRow(
      column(6, verbatimTextOutput("firstData")),
      column(6, verbatimTextOutput("secondData"))),
    br(),
    fluidRow(
      column(6, numericInput("numberC", "Insert number of columns", value = 4, min = 1, max = 20)),
      column(6, actionButton("action", "Plot the table with text"))),
    br(),
    br(),
    dataTableOutput("tableOut1"),
    br(), 
    fluidRow(
      column(6, actionButton("actionSlider", "Plot the table with slider")),
      column(6, sliderInput("sliderC", "Number of columns", value = c(4, 7), min = 1, max = 20))),
    br(),
    br(),
    dataTableOutput("tableOut2"),
    
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  nomi1 <- c("Media, Mediana, MidRange, Moda")
  dati1 <- c(3.1, 2, 10.5, 1)
  nomi2 <- c("Deviazione-Standard, MAD, IQR , Range")
  dati2 <- c(3.24, 1, 3, 19)
  output$firstSummary <- renderText(nomi1)
  output$firstData <- renderPrint(dati1)
  output$secondSummary <- renderText(nomi2)
  output$secondData <- renderPrint(dati2)
  
  table <- data.frame(numero_gol = c(1:20),
                      frequenze = c(104, 48, 26, 16, 13, 7, 5, 3, 9, 1, 2, 1, 3, 2, 1, 1, 0, 1, 0, 1))
  
  ##numberOfGol <- reactive(    ##Solo con inserimento nella label di testo, senza bottone
    ##{n <- table %>% arrange(numero_gol) %>% slice(1:input$numberC)
    ##return(n)})
  ##output$tableOut1 <- renderDataTable({numberOfGol()})
  
  numberOfGol <- eventReactive(
    input$action, 
    {n <- table %>% arrange(numero_gol) %>% slice(1:input$numberC)
    return(n)})
  output$tableOut1 <- renderDataTable({numberOfGol()})
  
  numberOfGol2 <- eventReactive(
    input$actionSlider,
    {n <- table %>% arrange(numero_gol) %>% slice(input$sliderC[1]:input$sliderC[2])
    return(n)}
  )
  output$tableOut2 <- renderDataTable({numberOfGol2()})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
