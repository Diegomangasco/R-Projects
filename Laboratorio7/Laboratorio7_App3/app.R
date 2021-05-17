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

ui <- fluidPage(
    
    fluidRow(
        column(12, offset = 4, h2("Gol e ruoli delle squadre di Serie A"))),
    br(),
    
    sidebarLayout(
        
        sidebarPanel(
            fluidRow(
                column(12, offset = 0, fileInput("file", "Load csv file Marcatori Serie A", accept = c(".csv")))),
            br(),
            fluidRow(
                column(5, offset = 0, selectInput("select", "Seleziona i ruoli",
                                                  choices = c("Tutti",
                                                              "Centrocampista",
                                                              "Attaccante",
                                                              "Difensore"), 
                                                  selected = "Tutti",
                                                  multiple = TRUE)),
                column(5, offset = 0, selectInput("select2", "Seleziona le squadre",
                                                  choices = c("Tutte", "Juventus", "Inter", "Milan", "Atalanta", "Crotone",
                                                              "Parma", "Fiorentina", "Napoli", "Roma", "Genoa",
                                                              "Bologna", "Lazio", "Sassuolo", "Sampdoria", "Verona",
                                                              "Udinese", "Cagliari", "Torino", "Spezia", "Benevento"), 
                                                  selected = "Tutte",
                                                  multiple = TRUE))),
            width = 4
        ),
        mainPanel(
            fluidRow(
                column(12, dataTableOutput("tableOut"))
            ),
            br(),
            fluidRow(
                column(12, plotOutput("freqGraph"))
            )
        )
    )
        
)


server <- function(input, output) {
    
    df<- reactive({req(input$file)
        read.csv(input$file$datapath, header = TRUE, sep=",")})
    
    action <- eventReactive(
        c(input$select, input$select2),
        {
            if(!"Tutti" %in% input$select){
                freq <- df() %>% 
                    group_by(Squadra) %>%
                    filter(Gol > 0 & Ruolo %in% input$select) %>%
                    summarise(Numero_Marcatori = n()) %>%
                    group_by(Squadra) %>%
                    slice(1) %>%
                    arrange(-Numero_Marcatori)
                
            }
            else{
                freq <- df() %>% 
                    group_by(Squadra) %>%
                    filter(Gol > 0) %>%
                    summarise(Numero_Marcatori = n()) %>%
                    group_by(Squadra) %>%
                    slice(1) %>%
                    arrange(-Numero_Marcatori)
            }
            
            if(!"Tutte" %in% input$select2){
                freq <- freq %>% 
                    filter(Squadra %in% input$select2)
            }
            return(freq)
        })
    output$tableOut <- renderDataTable({action()})
    output$freqGraph <- renderPlot({
        ggplot(action(), aes(x = action()$Squadra, y = action()$Numero_Marcatori)) +
        geom_bar(stat = "identity") +
        ylab("") +
        ggtitle("Marcatori") +
        xlab("")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
