#Load packages
library(shiny)
library(DT)

#Define UI layout
fluidPage(
    
    titlePanel("Clinical Trial Dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose a CSV file",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                      ".csv")
                      ),
            # tags$hr(),
            # checkboxInput('header', 'Header', TRUE),
            checkboxGroupInput(inputId = "SelIn1", label = "Variables", choices = "..."),
            selectInput(inputId = "SelIn2", label = "Group by", choices = "..."),
            selectInput(inputId = "SelIn3", label = "Filter Categories", choices = "..."),
            uiOutput("reactUI1")
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Boxplot", plotOutput("boxchart"), 
                                 uiOutput("download1")),
                        tabPanel("Trend", plotOutput("linechart"),
                                 uiOutput("download2"))
            ),
            br(),
            tabsetPanel(type = "tabs",
                        tabPanel("Data", DT::dataTableOutput("tablechart1")),
                        tabPanel("Summary", DT::dataTableOutput("tablechart2"))
            )
        )
    )
)
