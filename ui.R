library(shiny)

shinyUI(fluidPage(
    
#-------------------------------------------
    titlePanel("Spot relative to Trend"),
#-------------------------------------------
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Choose CSV File',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            tags$hr(),
            checkboxInput('header', 'Header', TRUE),
            selectInput('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ',')
        ),

        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Ranking Chart", plotOutput("plot")), 
                        tabPanel("Summary", 
                                 selectInput("variable", "Select projection type:",
                                             list("Latest 20days" = "abc", 
                                                  "40 Days Regime" = "pqr")),
                                 numericInput('days', 'Number of Days', 3),
                                 plotOutput("projection"),
                                 dataTableOutput("summary")),
                        
                        tabPanel("SpotData", tableOutput("contents"))
        )
    )
)))

