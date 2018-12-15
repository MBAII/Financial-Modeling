# Load packages ----
library(shiny)
library(quantmod)

# Source helpers ----

source("MV_Function.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Mean-Variance Portfolio"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select stocks to examine. 
        Information will be collected from Yahoo finance."),
      
      textInput("symb", "Symbols", "SPY,GOOG,KO"),
      
      textInput("rf", "Fisk Free Rate", "0.02"),
      
      dateRangeInput("dates", 
                     "Date range",
                     start = "1980-01-01", 
                     end = as.character(Sys.Date())),
      
      textInput("ret", "Desired Return", "0.1"),
      
      

      # br(),
      br(),
      
      actionButton("submit", label = "Submit/Clear")
      
    ),
    
    mainPanel(
      tableOutput("weight"),
      plotOutput("plot")
      

    )
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    is_click = input$submit
    stocks <- input$symb
    rf <- input$rf
    ret <- as.numeric(input$ret)
    start <- input$dates[1]
    end <- input$dates[2]
    message(is_click)

    if (is_click%%2 == 1){

      return(mv_model(stocks,rf,start,end,ret))
    }
  })

  output$weight <- renderTable({
    dataInput()$weight
  })
  
  output$plot <- renderPlot({
    dataInput()$plot
  })
  

}

# Run the app
shinyApp(ui, server)
