# Load packages ----
library(shiny)
library(quantmod)

# Source helpers ----
source("helpers.R")
source("MV_Function.R")

# User interface ----
ui <- fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select stocks to examine. 
        Information will be collected from Yahoo finance."),
      
      textInput("symb", "Symbols", "SPY,GOOG,KO"),
      
      # dateRangeInput("dates", 
      #                "Date range",
      #                start = "2013-01-01", 
      #                end = as.character(Sys.Date())),
      # 
      br(),
      br(),
      
      actionButton("submit", label = "Submit")
      # 
      # checkboxInput("log", "Plot y axis on log scale", 
      #               value = FALSE),
      # 
      # checkboxInput("adjust", 
      #               "Adjust prices for inflation", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("plot"),
      textOutput("text"))
  )
)

# Server logic
server <- function(input, output) {

  dataInput <- reactive({
    # getSymbols(input$symb, src = "yahoo",
    #            from = input$dates[1],
    #            to = input$dates[2],
    #            auto.assign = FALSE)
  })

  output$plot <- renderPlot({

    # chartSeries(dataInput(), theme = chartTheme("white"),
    #             type = "line", log.scale = input$log, TA = NULL)
    if (input$submit){
      mv_model(input$symb)
    }
  })
  
  output$text <- renderPrint({ 
    observeEvent(input$submit, {
      # print(input$symb)
      data = input$symb
      data_2 = unlist(strsplit(data, ","))
      print(data)
      print(data_2)
      print(typeof(data_2))
    })
    if (input$submit){
      "submit"
    }else{
      "not submit yet"
    }
    })
}

# Run the app
shinyApp(ui, server)
