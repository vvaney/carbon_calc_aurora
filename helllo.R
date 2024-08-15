
library(shiny)
library(ggplot2)
library(dplyr)

# Define the carbon credit model function
carbon_credit_model <- function(df, harvest_rate, growth_rate) {
  
  # Extract project-specific inputs
  start_year <- as.numeric(df$Start_Year[1])
  live_stocking <- as.numeric(df$Live_Stocking[1])
  dead_stocking <- as.numeric(df$Dead_Stocking[1])
  acres <- as.numeric(df$Acres[1])
  initial_loss_rate <- as.numeric(df$Loss.Rate[1])
  loss_period <- as.numeric(df$Loss_Period[1])
  low_point <- as.numeric(df$Low_Point[1])
  
  # Initialize data structures
  years <- nrow(harvest_rate)
  year_labels <- start_year:(start_year + years - 1)
  
  # Calculate Baseline Scenario
  
  if (loss_period == 1){
    loss_factor <- data.frame(
      loss_factor = rep(1.0, years),
      year = year_labels)
    
  } else {
    loss_factor <- data.frame(
      loss_factor = c(rep(1.2, 6), rep(1, 14)),
      year = year_labels)
  }
  
  loss_factor$loss_factor <- as.numeric(loss_factor$loss_factor)
  loss_factor$year <- as.numeric(loss_factor$year)
  
  carbon_stock_baseline <- numeric(years)
  dead_carbon_per_acre_baseline <- numeric(years)
  total_carbon_baseline_per_acre <- numeric(years)
  acre_carbon_baseline <- numeric(years)
  current_loss_rate <- numeric(years)
  carbon_stock_baseline[1] <- live_stocking
  dead_carbon_per_acre_baseline[1] <- dead_stocking
  total_carbon_baseline_per_acre[1] <- live_stocking + dead_stocking
  acre_carbon_baseline[1] <- (total_carbon_baseline_per_acre[1] * acres) / 1000
  current_loss_rate[1] <- initial_loss_rate
  
  for (year in 2:years) {
    
    loss_factor_yr <- loss_factor$loss_factor[year - 1]
    if (year > loss_period) {
      current_loss_rate[year] <- growth_rate
    } else {
      current_loss_rate[year] <- initial_loss_rate
    }
    carbon_stock_baseline[year] <- carbon_stock_baseline[year - 1] + live_stocking * (growth_rate - current_loss_rate[year] * loss_factor$loss_factor[year])
    dead_carbon_per_acre_baseline[year] <- dead_carbon_per_acre_baseline[year - 1] * (carbon_stock_baseline[year] / carbon_stock_baseline[year - 1])
    total_carbon_baseline_per_acre[year] <- carbon_stock_baseline[year] + dead_carbon_per_acre_baseline[year]
    acre_carbon_baseline[year] <- (total_carbon_baseline_per_acre[year] * acres) / 1000
  }
  
  # adjustement that I made so that average would be the same as spreadsheet ####
  
  #######################################
  n_rows <- length(acre_carbon_baseline)
  
  # Set the last row to NA
  acre_carbon_baseline[n_rows] <- NA
  
  # Set the second-to-last row to 0
  acre_carbon_baseline[n_rows - 1] <- 0
  #######################################
  
  
  baseline_df <- data.frame(
    Year = year_labels,
    carbon_per_acre_baseline = carbon_stock_baseline,
    dead_carbon_per_acre_baseline = dead_carbon_per_acre_baseline,
    total_carbon_baseline_per_acre = total_carbon_baseline_per_acre,
    total_carbon_baseline = acre_carbon_baseline,
    mean_total_c = mean(acre_carbon_baseline, na.rm = TRUE),
    loss_rate = current_loss_rate,
    loss_factor = loss_factor$loss_factor
  )
  
  # Calculate Projected Carbon Stocks
  carbon_stock_projected <- numeric(years)
  carbon_stock_projected[1] <- live_stocking
  total_carbon_growth <- numeric(years)
  blended_growth <- numeric(years)
  blended_growth[1] <- growth_rate
  
  for (year in 2:years) {
    current_year <- start_year + year - 1
    harvest_rate_current_year <- harvest_rate$harvest[harvest_rate$year == current_year]
    if (length(harvest_rate_current_year) == 0) {
      harvest_rate_current_year <- 0
    }
    adjusted_growth_rate <- growth_rate * (1 - harvest_rate_current_year)
    growth <- live_stocking * adjusted_growth_rate
    carbon_stock_projected[year] <- carbon_stock_projected[year - 1] + growth
    total_carbon_growth[year] <- (growth * acres) / 1000
    blended_growth[year] <- growth / carbon_stock_projected[year]
  }
  
  projected_df <- data.frame(
    Year = year_labels,
    Carbon_Stock = carbon_stock_projected,
    Carbon_per_acre_growth = total_carbon_growth,
    total_carbon_per_acre = carbon_stock_projected + dead_stocking,
    harvest_rate = c(harvest_rate$harvest, rep(0, years - nrow(harvest_rate))),
    blended_growth = blended_growth,
    static_growth_rate = rep(growth_rate, years)
  )
  
  # Calculate Credit Issuance and Removal
  total_carbon <- numeric(years)
  change_in_C <- numeric(years - 1)
  harvested <- numeric(years)
  twenty_yr_harvest <- numeric(years)
  
  for (i in 1:years) {
    total_carbon[i] <- (acres * projected_df$total_carbon_per_acre[i]) / 1000
  }
  
  for (i in 1:(years - 1)) {
    change_in_C[i] <- total_carbon[i + 1] - total_carbon[i]
  }
  
  for (i in 1:years) {
    harvested[i] <- acres * projected_df$Carbon_Stock[i] * projected_df$harvest_rate[i] * projected_df$blended_growth[i] * (0.05 / 1000)
  }
  
  for (i in 1:years) {
    if (baseline_df$loss_rate[i] != projected_df$static_growth_rate[i]) {
      twenty_yr_harvest[i] <- live_stocking * baseline_df$loss_rate[i] * acres * (0.05 / 1000)
    } else {
      twenty_yr_harvest[i] <- low_point * baseline_df$loss_rate[i] * acres * (0.05 / 1000)
    }
  }
  
  average_twenty_baseline <- mean(twenty_yr_harvest)
  market_leakage <- df$Market_Leakage
  buffer <- 0.18
  
  issuance_df <- data.frame(
    years = year_labels,
    total_carbon = total_carbon,
    change_in_C = c(change_in_C, NA), 
    harvested = harvested,
    twenty_yr_harvest = twenty_yr_harvest,
    average_twenty_baseline = average_twenty_baseline,
    market_leakage = market_leakage,
    buffer = buffer,
    removal_credits = numeric(years)
  )
  
  for (i in 1:years) {
    issuance_df$removal_credits[i] <- (issuance_df$change_in_C[i] + issuance_df$harvested[i] - issuance_df$average_twenty_baseline) * (1 - issuance_df$market_leakage) * (1 - issuance_df$buffer)
  }
  
  # Calculate Conservation Credits
  delta_baseline <- numeric(years)
  stocking_emissions_reduction <- numeric(years)
  harvest_reduction <- numeric(years)
  gross_emission_reduction <- numeric(years)
  voluntary_buffer_credit <- numeric(years)
  annual_ert <- numeric(years)
  conservation_credit <- numeric(years)
  delta_baseline <- numeric(nrow(baseline_df) - 1)
  turning_point <- FALSE  # Initialize the turning point flag
  turning_point_position <- 0  # Counter for the turning point position
  
  for (i in 1:(nrow(baseline_df) - 1)) {
    if (baseline_df$mean_total_c[i + 1] > baseline_df$total_carbon_baseline[i]) {
      if (!turning_point && i > 1) {
        # Set the value for the position before the turning point
        delta_baseline[i - 1] <- baseline_df$mean_total_c[i - 1] - baseline_df$total_carbon_baseline[i - 1]
        delta_baseline[i] <- 0  # Set the turning point and onwards to 0
        turning_point <- TRUE
        turning_point_position <- i - 1  # Record the position of the turning point
      } else {
        delta_baseline[i] <- 0
      }
    } else {
      if (!turning_point) {
        # Normal calculation before the turning point
        delta_baseline[i] <- baseline_df$total_carbon_baseline[i + 1] - baseline_df$total_carbon_baseline[i]
      } else {
        delta_baseline[i] <- 0  # Set to 0 after the turning point
      }
    }
  }
  
  # Set the last element of delta_baseline to NA if needed
  delta_baseline[nrow(baseline_df)] <- NA
  
  # Print the turning point position
  if (turning_point_position > 0) {
    print(paste("The turning point is at position:", turning_point_position))
  } else {
    print("No turning point was found.")
  }
  
  end_year_conser <- start_year + turning_point_position
  
  for (i in 1:(years - 1)) {
    stocking_emissions_reduction[i] <- issuance_df$change_in_C[i] - delta_baseline[i]
    harvest_reduction[i] <- issuance_df$harvested[i] - issuance_df$average_twenty_baseline
    gross_emission_reduction[i] <- (harvest_reduction[i] + stocking_emissions_reduction[i]) * (1 - market_leakage)
    voluntary_buffer_credit[i] <- buffer * gross_emission_reduction[i]
    annual_ert[i] <- gross_emission_reduction[i] - voluntary_buffer_credit[i]
    if (year_labels[i] < end_year_conser) {
      conservation_credit[i] <- annual_ert[i] - issuance_df$removal_credits[i]
    } else {
      conservation_credit[i] <- 0
    }
  }
  
  prop <- df$Group
  
  result <- data.frame(
    Year = year_labels,
    prop = prop,
    removal_credit = issuance_df$removal_credits,
    conservation_credit = conservation_credit
  )
  
  return(result)
}

# UI
ui <- fluidPage(
  titlePanel("Carbon Credit Model for Voluntary Carbon Markets"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("property", 
                  "Select Property:", 
                  choices = NULL),  # Choices will be populated dynamically in server
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
server <- function(input, output, session) {
  
  # Load data once in the server function
  voluntary_inputs <- read.csv("Inputs_Voluntary.csv")
  voluntary_inputs <- voluntary_inputs[-1, ]
  
  # Update the selectInput choices dynamically based on available properties
  observe({
    property_choices <- unique(voluntary_inputs$Group)
    updateSelectInput(session, "property", choices = property_choices)
  })
  
  # Reactive expression to filter data and run the model
  result_data <- reactive({
    req(input$property)
    
    # Filter data based on the selected property
    filtered <- voluntary_inputs |>
      filter(Group == input$property)
    
    # Harvest rate stays the same, but it can also be reactive if needed
    harvest_rate <- data.frame(
      harvest = c(0.15, 0.05, 0, rep(0, 17)),
      year = 2024:2043
    )
    
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
