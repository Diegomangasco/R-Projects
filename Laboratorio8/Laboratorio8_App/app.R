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
library(readxl)

media_mobile <- function(v,n){
    stopifnot(length(v)>=n)
    res = rep(NA,floor((n-1)/2))
    while(length(v) >= n){
        res = c(res,mean(v[1:n]))
        v=tail(v,-1)
    }
    res = c(res,rep(NA,floor(n/2)))
    return( res )
}

ui <- fluidPage(
    useShinyFeedback(),
    fluidRow(
        column(12, offset = 4, h2("VIQ dashboard Covid-19"))
    ),
    br(),
    fluidRow(
        column(12, offset = 4, fileInput("file", "Load covid_19-iss.xlsx", accept = ".xlsx"))
    ),
    br(),
    fluidRow(
        column(4, offset = 2, checkboxGroupInput("types", 
                                                  "Types of visualization", 
                                                  c("SINTOMI", "DIAGNOSI"))),
        column(4, offset = 1, sliderInput("slider", 
                                          "Range of date:",
                                          value = c(as.Date("2020/01/15"), as.Date("2020/02/16")),
                                          min = as.Date("2020/01/01"),
                                          max = as.Date("2020/12/31")))
    ),
    fluidRow(
        column(6, plotOutput("data", 
                             brush = brushOpts(id = "sel",
                                               fill = "orange", resetOnNew = FALSE))),
        column(6, dataTableOutput("info")),
    )
)

server <- function(input, output) {

    df <- reactive({
        req(input$file)
        ext <- tools::file_ext(input$file$name)
        if(ext != "xlsx"){
           validate("Can only admit .xlsx file")
        }
    })
    
    inizio <- reactive({
        req(input$file)
        tryCatch({
            inizio <- read_xlsx(input$file$datapath, sheet = "casi_inizio_sintomi")
        }, warning = function(war){
            validate("Not preset the sheet: casi_inizio_sintomi")
        }, error = function(err){
            validate("Not preset the sheet: casi_inizio_sintomi")
        })
        return(inizio)
    })
    
    prelievo <- reactive({
        req(input$file)
        tryCatch({
            prelievo <- read_xlsx(input$file$datapath, sheet = "casi_prelievo_diagnosi")
        }, warning = function(war){
            validate("Not preset the sheet: casi_prelievo_diagnosi")
        }, error = function(err){
            validate("Not preset the sheet: casi_prelievo_diagnosi")
        })
        return(prelievo)
    })
    
    covid_casi_data <- reactive({
        covid_casi_data <- 
            inizio() %>%
            mutate(CASI = as.numeric(CASI)) %>% 
            rename(SINTOMI = CASI) %>% 
            inner_join(
                prelievo() %>% 
                    mutate(CASI = as.numeric(CASI)) %>% 
                    rename(DIAGNOSI = CASI),
                by =c("DATA_INIZIO_SINTOMI"="DATA_PRELIEVO_DIAGNOSI")
            ) %>%
            rename(Data = DATA_INIZIO_SINTOMI) %>% 
            select(- starts_with("iss_date")) %>% 
            mutate(Data = as.Date(Data,format="%d/%m/%Y")) %>% 
            filter(! is.na(Data)) %>% 
            pivot_longer(c("SINTOMI","DIAGNOSI"),names_to="Tipo",values_to="CASI") %>% 
            group_by(Tipo) %>% arrange(Data) %>% 
            mutate(CASI_MM=media_mobile(CASI,7)) %>% 
            filter(!is.na(CASI_MM))
        return(covid_casi_data)
    })
    
    action <- eventReactive(
        c(input$types, input$slider),
        {
            res <- covid_casi_data() %>%
                filter(Data <= input$slider[2] & Data >= input$slider[1])
            result <- res
            
            if(length(input$types) == 1){
                if(input$types == "SINTOMI"){
                    result <- 
                        result %>%
                        filter(Tipo == "SINTOMI") 
                }
                else if(input$types == "DIAGNOSI"){
                    result <- 
                        result %>%
                        filter(Tipo == "DIAGNOSI") 
                }
            }
            else if(length(input$types) == 0){
                validate("Insert the type of plot")
            }
            return(result)
        })
    
    output$data <- renderPlot({
        req(action())
        
        plot <- ggplot(action(), aes(x=Data,y=CASI_MM, color=Tipo, group=Tipo))+
            geom_line()+
            scale_color_brewer(type="qual",guide="none")+
            geom_point(data=~.x %>% filter(Data==max(Data)))+
            geom_text(aes(label=Tipo),data=~.x %>% filter(Data==max(Data)),
                      hjust="left",vjust="bottom",nudge_x = 3, show.legend = FALSE)+
            geom_text(aes(label=round(CASI_MM)),data=~.x %>% filter(Data==max(Data)),
                      hjust="left",vjust="top",nudge_x = 3,nudge_y=-200,size=3,color="gray20")+
            scale_x_date(expand=expansion(add=c(0,40)))+
            ylab("Casi")+
            ggtitle("Andamento dei casi","Per data inizio sintomi, media mobile a 7gg")+
            theme_minimal()
        
        
        bP <- brushedPoints(action(), input$sel)
        media_diag <- round(mean((bP %>% filter(Tipo == "DIAGNOSI"))$CASI))
        media_sint <- round(mean((bP %>% filter(Tipo == "SINTOMI"))$CASI))
        
        if(!is.nan(media_diag))
            plot <- plot + geom_hline(yintercept = media_diag, color="orange", linetype=2) +
            annotate(geom= "text", x=as.Date(bP$Data[1]), 
                     y = media_diag, label=paste("Media Diagnosi: ", media_diag), 
                     hjust = "left", family="serif", fontface="bold", colour = "orange", size = 5 )
        
        if(!is.nan(media_sint))
            plot <- plot + geom_hline(yintercept = media_sint, color="orange", linetype=2) +
            annotate(geom= "text", x=as.Date(bP$Data[1]), 
                     y = media_sint, label=paste("Media Sintomi: ", media_sint), 
                     hjust = "left", family="serif", fontface="bold", colour = "orange", size = 5 )
        
        plot
        
    })
    
    
    output$info <- renderDataTable({
        bP <- brushedPoints(action(), input$sel)
        if(nrow(bP) == 0){
            validate("You didn't select some data")
        }
        else{
            return(bP)
        }
    }, options = list(pageLength = 10))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
