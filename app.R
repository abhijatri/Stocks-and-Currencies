library(shiny)
library(quantmod)
library(shinythemes)
library(sysfonts)
library(magrittr)
library(DT)

ticker <- read.csv("http://markets.cboe.com/us/equities/market_statistics/listed_symbols/csv", stringsAsFactors = F)$Name

ui <- fluidPage(
    theme = shinytheme("flatly"),
    includeCSS("./www/bootstrap.css"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    # titlePanel("Stock Prices and Currencies"),
    
    # Sidebar with a slider input for number of bins
    navbarPage("Stock Prices and Currencies",
               collapsible = T,
               footer = tagList(
                   br(),
                   tags$a(href="mailto:abhijatridas@gmail.com",icon("envelope"),target="_blank"),
                   tags$a(href="https://www.linkedin.com/in/abhijatri-das-2800454b",icon("linkedin"),target="_blank"),
                   tags$a(href="https://github.com/abhijatri/Stocks-and-Currencies.git", icon("github"), "Source Code", target="_blank")
               ),
               
               tabPanel("Stocks", fluid = TRUE,
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("custom","Use only CBOE listed symbols?",c("Yes","No"), selected = "Yes", inline = T),
                                conditionalPanel(
                                    condition = "input.custom == 'Yes'",
                                    selectInput("ticker_cboe","CBOE Symbol", choices = ticker)
                                ),
                                conditionalPanel(
                                    condition = "input.custom == 'No'",
                                    textInput("ticker_custom","Yahoo Finance Symbol", value = "^VIX")
                                ),
                                selectInput("freq","Periodicity ",choices = c("daily","weekly","monthly")),
                                dateRangeInput("range","Period",start = "2020-01-01"),
                                radioButtons("chart_type","Chart Type",c("candlesticks", "matchsticks", "bars","line")),
                                radioButtons("log","Log Scale",c("Yes","No"), inline = T, selected = "No"),
                                downloadButton('downloadData', 'Download .csv')
                                
                            ),
                            
                            mainPanel(
                                plotOutput("plot"),
                                uiOutput("return"),
                                div(style = 'overflow-x: scroll',DT::dataTableOutput("data"))
                            )
                        )
               ),
               tabPanel("Currencies", fluid = TRUE,
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("ticker_curr1","Currency 1", choices = rownames(oanda.currencies)),
                                selectInput("ticker_curr2","Currency 2", choices = rownames(oanda.currencies)),
                                dateRangeInput("range2","Period",start = Sys.Date() - 180),
                                radioButtons("log2","Log Scale",c("Yes","No"), inline = T, selected = "No"),
                                downloadButton('downloadData2', 'Download .csv')
                                
                            ),
                            
                            mainPanel(
                                plotOutput("plot2"),
                                div(style = 'overflow-x: scroll',DT::dataTableOutput("data2"))
                            )
                        )
               )
    )
)


server <- function(input, output) {
    
    observe({
        name <- try(ifelse(input$custom == "Yes",input$ticker_cboe,input$ticker_custom))
        try(getSymbols.yahoo(name, from = input$range[1], to = input$range[2], env = parent.frame(), periodicity = input$freq))
        name2 <- try(gsub("[^[:alnum:]]", "", name))
        
        data <- try(get(name2, envir = parent.frame()))
        return <- try(round((last(data[,4])[[1]]/first(data[,1])[[1]] - 1)*100,2))
        
        dt <- try(data.frame(Day = index(data), data))
        dt <- try(dt[order(dt$Day, decreasing = T),])
        
        
        f_log <- try(ifelse(input$log == "Yes",TRUE,FALSE))
        
        output$plot <- renderPlot({
            try(chartSeries(data,
                            type = input$chart_type,
                            name = name2,
                            log.scale = f_log,
                            theme = chartTheme("white"),
                            multi.col = F,
                            minor.ticks=F
            )
            )
        })
        
        output$return <- renderUI({
            try(
                p(strong(return,"% return"))
            )
            
        })
        
        output$data <- DT::renderDataTable({
            
            DT::datatable(dt,rownames = F) %>%
                formatRound(c(2,3,4,5,7), 2) %>% 
                formatCurrency(c(6), currency = "", interval = 3, mark = ",", digits = 0)
            
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                paste(paste(name2, input$range[1], input$range[2], sep="_"),".csv", sep = "")
            },
            content = function(file) {
                write.csv(dt, file,row.names = FALSE)
            }
        )
        
    })
    
    observe({
        name <- paste(input$ticker_curr1,input$ticker_curr2, sep = "/")
        name2 <- paste(input$ticker_curr1,input$ticker_curr2, sep = "_")
        
        getSymbols(name, src = "oanda", from = input$range2[1], to = input$range2[2], env = parent.frame())
        
        data <- get(paste0(input$ticker_curr1,input$ticker_curr2), envir = parent.frame())
        
        f_log2 <- ifelse(input$log2 == "Yes",TRUE,FALSE)
        
        dt <- try(data.frame(Day = index(data), data))
        dt <- try(dt[order(dt$Day, decreasing = T),])
        
        output$plot2 <- renderPlot({
            chartSeries(data,
                        name = name,
                        type = 'line',
                        log.scale = f_log2,
                        theme = chartTheme("white"),
                        multi.col = F,
                        minor.ticks=F
            )
        })
        
        output$data2 <- DT::renderDataTable({
            
            DT::datatable(dt,rownames = F)
            datatable(dt,rownames = F) %>%
                formatRound(c(2), 2)
        })
        
        output$downloadData2 <- downloadHandler(
            filename = function() {
                paste(paste(name2, input$range2[1], input$range2[2], sep="_"),".csv", sep = "")
            },
            content = function(file) {
                write.csv(dt, file,row.names = FALSE)
            }
        )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
