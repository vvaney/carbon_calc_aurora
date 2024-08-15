
library(shiny)
library(ggplot2)
library(dplyr)

# Define the carbon credit model function
carbon_credit_model <- function(df, harvest_rate, growth_rate) {
  # (your full model code here, with growth_rate as a parameter in the function signature)
  # ...
  
  # return the result dataframe
  return(result)
}

# UI
ui <- fluidPage(
  titlePanel("Carbon Credit Model for Voluntary Carbon Markets"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("growth_rate",
                  "Timber Growth Rate:",
                  min = 0.01,
                  max = 0.10,
                  value = 0.05,
                  step = 0.005)
    ),
    
    mainPanel(
      plotOutput("removalPlot"),
      plotOutput("conservationPlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive expression to filter data and run the model
  result_data <- reactive({
    harvest_rate <- data.frame(
      harvest = c(0.15, 0.05, 0, rep(0, 17)),
      year = 2024:2043
    )
    
    
    
    voluntary_inputs <- read.csv("Inputs_Voluntary.csv")
    voluntary_inputs <- voluntary_inputs[-1, ]
    
    filtered <- voluntary_inputs |>
      filter(Group == "Kanawha River")
    
    # Run the carbon credit model with the reactive growth_rate
    result <- carbon_credit_model(filtered, harvest_rate, input$growth_rate)
    
    return(result)
  })
  
  # Render removal credits plot
  output$removalPlot <- renderPlot({
    data <- result_data()
    
    ggplot(data, aes(x = Year, y = removal_credit, color = prop)) +
      geom_line() +
      labs(title = "Removal Credits Over Time",
           x = "Year",
           y = "Removal Credits")
  })
  
  # Render conservation credits plot
  output$conservationPlot <- renderPlot({
    data <- result_data()
    
    ggplot(data, aes(x = Year, y = conservation_credit, color = prop)) +
      geom_line() +
      labs(title = "Conservation Credits Over Time",
           x = "Year",
           y = "Conservation Credits")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
