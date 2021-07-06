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
library(tools)
library(shinyFeedback)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyFeedback(),
    fluidRow(
        column(12, offset = 5, tags$h2(tags$b("Global Oil Reserves")))
    ),
    br(),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(5, offset = 4, fileInput("file", "Load csv OilReserves", accept = c(".csv")))
            ),
            fluidRow(
                column(3, offset = 1, selectInput("types",
                                                  "Types of visualization", 
                                                  choices = c("None", "Vertical barplot", "Horizontal barplot",
                                                           "Boxplot for statistics", "Pie plot"),
                                                  selected = NULL,
                                                  multiple = FALSE)),
                column(2, offset = 1, radioButtons(inputId = "opec", label = "Companies",
                                      c("OPEC", "NO OPEC", "ALL"),
                                      selected = "ALL")),
                column(2, offset = 0, radioButtons(inputId = "button", label = "Table",
                                      c("Yes", "No"),
                                      selected = "No")),
                column(2, offset = 0, radioButtons(inputId = "order", label = "Order",
                                      c("Ascending", "Descending", "Casual"),
                                      selected = "Casual"))
            ),
            width = 12
        ),
        mainPanel(
            fluidRow(
                column(12, offset = 3, dataTableOutput("table"))
            ),
            fluidRow(
                column(12, offset = 3, plotOutput("graph", 
                                      height = "650px", width = "930px")
            
                )
            ),
            fluidRow(
                column(12, offset = 3, textOutput("warning"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df<- reactive(
        {
            req(input$file)
            ext <- tools::file_ext(input$file$name)
            if(ext != "csv"){
                validate("Can only admit .csv file")
            }
            tryCatch({
                ret <- read.csv(input$file$datapath, header = TRUE, sep=",") %>%
                    mutate(Country = as.character(Country), 
                           Percentage = as.numeric(Percentage),
                           OPEC = as.character(OPEC)) %>%
                    mutate(OPEC = str_trim(OPEC, side = "both"))
            }, warning = function(war){
                validate("Error with file read")
            }, error = function(war){
                validate("Error with file read")
            })
            
            return(ret)
        }
    )
    
    
    opc <- eventReactive( #opec non opec
        c(input$order, input$opec),
        {
            if(input$opec == "OPEC"){
                dataB <- df() %>% 
                    filter(OPEC == "Yes")
            }
            else if(input$opec == "NO OPEC"){
                dataB <- df() %>% 
                    filter(OPEC == "No")
            }
            else{
                dataB <- df()
            }
            
            if(input$order == "Ascending"){
                dataB <- dataB %>%
                    mutate(Country = fct_reorder(Country, Percentage))
            }
            else if(input$order == "Descending"){
                dataB <- dataB %>%
                    mutate(Country = fct_reorder(Country, -Percentage))
            }
            
            return(dataB)
        }
    )
    
    
    
    colourOpec <- eventReactive(
        input$opec,
        {
            if(input$opec == "ALL"){
                return("ALL")
            }
            else if(input$opec == "OPEC"){
                return("OPEC")
            }
            else if(input$opec == "NO 0PEC"){
                return("NO OPEC")
            }
            else{
                return("NONE")
            }
        }
    )
    
    tab <- eventReactive( #table or not
        input$button,
        {
            if(input$button == "Yes"){
                return(1)
            }
            else{
                return(0)
            }
        }
    )
    
    output$table <- renderDataTable(
       if(tab() == 1){
           return(opc())
       },
       options = list(pageLength = 5)
    )
    
    
    typeOfGraph <- eventReactive(
        input$types,
        {
            if(input$types == "None"){
                return(0)
            }
            if(input$types == "Vertical barplot"){
                return(1)
            }
            if(input$types == "Horizontal barplot"){
                return(2)
            }
            if(input$types == "Boxplot for statistics"){
                return(3)
            }
            if(input$types == "Pie plot"){
                return(4)
            }
            else{
                return(-1)
            }
        }
    )
    
    output$warning <- renderText(
        {
            req(input$types)
            req(input$opec)
            if(input$types != "None" && input$types != "Pie plot" && input$opec != "OPEC"){
                ret <- "(*)WARNING: Pay attention to 'Others' because this is a category that includes multiple countries. It's not possible to compare a group of countries with categories that contain information of one only State!"
            }
            else{
                ret <- NULL
            }
            return(ret)
        }
    )
    output$graph <- renderPlot(
        {
            if(typeOfGraph() == 0){
                validate("You didn't selected a type of graph")
            }
            if(all(is.na(opc()))){
                use <- df()
            }
            else{
                use <- opc()
            }
            m <- max(use$Percentage)
            if(typeOfGraph() == 1){
                if(colourOpec() == "ALL"){
                    plot <- use %>%
                        mutate(Percentage = Percentage/100) %>%
                        ggplot(aes(x = Country, y = Percentage, fill = OPEC)) +
                        geom_col(width = 0.75) +
                        scale_fill_manual(values = c("black", "gray45"), labels = c("NO OPEC ", "OPEC")) +
                        scale_y_continuous(labels = scales::percent, breaks = seq(0, m, by = 0.02)) +
                        theme(axis.text=element_text(size=12)) +
                        geom_text(aes(label = paste0(Percentage*100, "%", "\n(", BillionsBarrels, ")")), nudge_y = 0.008) +
                        xlab("") +
                        ylab("") +
                        ggtitle("Percentage (billions of barrels)") +
                        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
                        theme(legend.key.size = unit(1, "cm"), legend.title = element_blank(), legend.text = element_text(size = 12))
                }
                else{
                    if("Yes" %in% use$OPEC){
                        color <- "gray45"
                    }
                    else{
                        color <- "black"
                    }
                    plot <- use %>%
                        mutate(Percentage = Percentage/100) %>%
                        ggplot(aes(x = Country, y = Percentage, fill = OPEC)) +
                        geom_col(width = 0.75) +
                        scale_fill_manual(values = c(color)) +
                        scale_y_continuous(labels = scales::percent, breaks = seq(0, m, by = 0.02)) +
                        theme(axis.text=element_text(size=12)) +
                        geom_text(aes(label = paste0(Percentage*100, "%", "\n(", BillionsBarrels, ")")), nudge_y = 0.008) +
                        guides(fill = FALSE) +
                        xlab("") +
                        ylab("") +
                        ggtitle("Percentage (billions of barrels)") +
                        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
        
                }
            }
            if(typeOfGraph() == 2){
                if(colourOpec() == "ALL"){
                    plot <- use %>%
                        mutate(Percentage = Percentage/100) %>%
                        ggplot(aes(x = Percentage, y = Country, fill = OPEC)) +
                        geom_col() +
                        scale_fill_manual(values = c("black", "gray45"), labels = c("NO OPEC ", "OPEC")) +
                        scale_x_continuous(labels = scales::percent, breaks = seq(0, m, by = 0.02)) +
                        theme(axis.text.x=element_text(size=12)) +
                        geom_text(aes(label = paste0(Percentage*100, "%", " (", BillionsBarrels, ")")), nudge_x = 0.01) +
                        xlab("") +
                        ylab("") +
                        ggtitle("Percentage (billions of barrels)") +
                        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
                        theme(legend.key.size = unit(1, "cm"), legend.title = element_blank(), legend.text = element_text(size = 12))+
                        theme(axis.text.y = element_text(size = 13))
                }
                else{
                    if("Yes" %in% use$OPEC){
                        color <- "gray45"
                    }
                    else{
                        color <- "black"
                    }
                    plot <- use %>%
                        mutate(Percentage = Percentage/100) %>%
                        ggplot(aes(x = Percentage, y = Country, fill = OPEC)) +
                        geom_col() +
                        scale_x_continuous(labels = scales::percent, breaks = seq(0, m, by = 0.02)) +
                        scale_fill_manual(values = c(color)) +
                        theme(axis.text.x=element_text(size=12)) +
                        guides(fill = FALSE) +
                        geom_text(aes(label = paste0(Percentage*100, "%", " (", BillionsBarrels, ")")), nudge_x = 0.01) +
                        xlab("") +
                        ylab("") +
                        ggtitle("Percentage (billions of barrels)") +
                        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
                        theme(axis.text.y = element_text(size = 13))
                }
            }
            if(typeOfGraph() == 3){
                plot <- use %>%
                    mutate(Percentage = Percentage/100) %>%
                    ggplot(aes(x = 0, y = Percentage)) +
                    geom_boxplot() +
                    scale_y_continuous(labels = scales::percent, breaks = seq(0, m, by = 0.02)) +
                    xlab("") +
                    ylab("") +
                    theme(axis.text.y = element_text(size=12), plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
                    geom_text(size = 5, aes(label = Country), size = 4, position = position_jitter(seed = 0), vjust = -1) +
                    geom_jitter(position = position_jitter(seed = 0)) +
                    theme(axis.text.x = element_blank()) +
                    ggtitle("Distribution and Statistics")
                
                
            }
            if(typeOfGraph() == 4){
                df_pie <- df() %>% group_by(OPEC) %>% 
                    summarize(OilReserves=sum(Percentage), Barrels = sum(BillionsBarrels)) %>%
                    arrange(desc(OPEC)) %>%
                    mutate(ypos = cumsum(OilReserves) - 0.5*OilReserves)
                
                plot <- df_pie %>%
                    ggplot(aes(x="", y=OilReserves, fill=OPEC))+
                    geom_bar(stat="identity", width=1, color="white") +
                    coord_polar("y", start=0)+
                    scale_fill_manual(values = c("black", "grey45"), labels = c("NO OPEC ", "OPEC"))+
                    theme_void()+
                    geom_text(aes(y = ypos, label = paste0(OilReserves,  "%", "\n(", Barrels, ")")), color = "white", size=6)+
                    labs(fill={ }) +
                    ggtitle("Reserves division between OPEC and NO OPEC countries\nPercentage (billions of barrels)") +
                    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
                    theme(legend.key.size = unit(1, "cm"), legend.text = element_text(size = 12))
            }
            return(plot)
        }
    )#abbreviazioni per vertical barplot
}

# Run the application 
shinyApp(ui = ui, server = server)
