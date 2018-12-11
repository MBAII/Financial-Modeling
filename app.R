# Load packages ----
library(shiny)
library(quantmod)

# Source helpers ----
source("helpers.R")
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
      
      actionButton("submit", label = "Submit")
      
    ),
    
    mainPanel(
      tableOutput("weight"),
      plotOutput("plot")
      
      # textOutput("text")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    is_click = input$submit
    stocks <- input$symb
    rf <- input$rf
    message(is_click)
    if (is_click == 1){
      is_click = 0
      return(mv_model(stocks,rf))
    }
  })

  output$weight <- renderTable({
    dataInput()$weight
  })
  
  output$plot <- renderPlot({
    dataInput()$plot
  })
  
  
  
  # output$text <- renderPrint({ 
  #   observeEvent(input$submit, {
  #     # print(input$symb)
  #     data = input$symb
  #     data_2 = unlist(strsplit(data, ","))
  #     print(data)
  #     print(data_2)
  #     print(typeof(data_2))
  #   })
  #   if (input$submit){
  #     "submit"
  #   }else{
  #     "not submit yet"
  #   }
  #   })
}

# Run the app
shinyApp(ui, server)
