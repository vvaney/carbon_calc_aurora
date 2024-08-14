library(dplyr)
library(tidyverse)
library(FinancialMath)
library(ggplot2)
library(tidyr)
library(gganimate)
library(gifski)
library(truncnorm)
library(writexl)
library(openxlsx)


#ESTABLISHING GLOBAL INPUTS 
global_inputs <- data.frame(
  price_curve = 1,
  stumpage_curve = 4,
  discount_rate = 0.10,
  inflation_rate = 0.025,
  CMDA = 0.15,
  capitalized_c_price = 40,
  cap_mult = 25.625,
  exp_mult = 16
  
)

####################################################
# Credit Issuance Model for Voluntary Carbon Markets 
####################################################


carbon_credit_model <- function(df, harvest_rate) {
  
  # set to default settings but you can change it to anything
  
  # Extract project-specific inputs
  start_year <- as.numeric(df$Start_Year[1])
  live_stocking <- as.numeric(df$Live_Stocking[1])
  dead_stocking <- as.numeric(df$Dead_Stocking[1])
  growth_rate <- as.numeric(df$Timber_Growth[1])
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
  
  
  
  
  result <- data.frame(
    Year = year_labels,
    removal_credit = issuance_df$removal_credit,
    conservation_credit = conservation_credit
    
  )
  
  return(result)
}







####################################
# INPUT VARS HERE-------------------
####################################


harvest_rate <- data.frame(
  harvest = c(0.15, 0.05, 0, rep(0, 17)),
  year = 2024:2043 #### change years to fit start year
)

#voluntary_inputs <- read.csv(paste0(carbCalcDir,"Inputs_Voluntary.csv"))
voluntary_inputs <- read.csv("Inputs_Voluntary.csv")
voluntary_inputs <- voluntary_inputs[-1, ]




#### choose property of interest ###

filtered <- voluntary_inputs |>
  filter(Group == "Kanawha River")

result <- carbon_credit_model(filtered, harvest_rate)



#############
#OUTPUTS
##############

credits_generated <- result |>
  filter(Year < 2034)|> #select years 
  mutate(removal_credit = removal_credit*1000, conservation_credit = conservation_credit*1000) %>%
  rename(Removal = removal_credit,
         Conservation = conservation_credit) %>%
  pivot_longer(2:3, names_to = "Credit Type", values_to = "Credits")








#######################
# DOWNLOAD PRICE CURVES
######################

# read in prices which you can change at any point in time just create a new CSV file from excel 

removal_price <- read.csv(paste0(carbCalcDir,"removal_price.csv"))
conservation_price <- read.csv(paste0(carbCalcDir,"conservation_price.csv"))


###########################
# Select years of interest 
###########################
 
# conservation_price <- conservation_price |>
#   filter(Year >= 2024 & Year <= 2034) # sub this for time frame of interest
#   
# removal_price <- removal_price  |>
#   filter(Year >= 2024 & Year <= 2034)  # sub this for time frame of interest

###########################
# calculate Prices
###########################

# 
# Value_Estimator <- function(years, conservation_price, removal_price) {
#   
#   removal_value <- numeric(length(years))
#   conservation_value <- numeric(length(years))
#   
#   removal_credits <- credits_generated$removal_credit
#   conservation_credits <- credits_generated$conservation_credit
#   
#   for (i in 1:length(years)) {
#     removal_value[i] <- removal_credits[i] * removal_price[i]
#     conservation_value[i] <- conservation_credits[i] * conservation_price[i]
#   }
#   
#   values <- data.frame(
#     year = years,
#     removal_value = removal_value,
#     conservation_value = conservation_value
#   )
#   
#   return(values)
# }
# 
# conservation_price <- conservation_price$ConservationPrice
# removal_price <- removal_price$RemovalPrice
# years = 2024:2034 #select time frame!!!
# 
# # OUTPUT
# dollar_value <- Value_Estimator(years,conservation_price,removal_price)


prices <- full_join(removal_price, conservation_price) %>%
  rename(Removal = RemovalPrice,
         Conservation = ConservationPrice) %>%
  pivot_longer(2:3, names_to = "Credit Type", values_to = "Price")

creditValue <- left_join(credits_generated, prices) %>%
  mutate(Value = Credits*Price)

discount_rate <- .1
start_year <- year(today())-1

carbonPV <- creditValue %>%
  mutate(Period = Year - start_year) %>%
    mutate(PV = Value/(1+discount_rate)^Period) %>%
  group_by(`Credit Type`) %>%
  summarise(PV = sum(PV))



###########################
# calculate NPV
###########################

# NPV_CALC <- function(removal_NPV_input,conservation_NPV_input){
# 
# removal_NPV <- c()
# conservation_NPV <- c()
#   
# removal_NPV <- sum(removal_NPV_input/ (1 + 0.1)^(1:length(removal_NPV_input)))
# conservation_NPV <- sum(conservation_NPV_input/(1+0.1)^(1:length(1:7)))
# 
# 
# NPV <- data.frame(
#   removal_NPV = removal_NPV,
#   conservation_NPV = conservation_NPV
# )
# 
# 
# return(NPV)
# }
# 
# 
# removal_NPV_input <- dollar_value$removal_value
# conservation_NPV_input <- dollar_value$conservation_value[1:7]
# 
# 
# NPV <- NPV_CALC(removal_NPV_input,conservation_NPV_input)



###########################
#   MONTE  CARLOOOOO
###########################


mean_value <- 0.02568071
standard_deviation <- 1

# set upper and lower boundaries
lower_bound <- 0.005
upper_bound <- 0.06

# Generate the normal distribution data
set.seed(123)  # for reproducibility
number_of_samples <- 1000

normal_distribution <- rtruncnorm(number_of_samples,a = lower_bound,b = upper_bound,mean = mean_value, sd = 1)


run_simulations <- function(df, harvest_rate, normal_distribution, num_simulations = 100) {
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Ensure the number of simulations does not exceed the length of the normal_distribution vector
  num_simulations <- min(num_simulations, length(normal_distribution))
  
  # Sample growth rates from the normal distribution
  sampled_growth_rates <- sample(normal_distribution, num_simulations)
  
  simulation_results <- data.frame(
    Simulation = integer(),
    Year = integer(),
    Growth_Rate = numeric(),
    Removal_Credit = numeric(),
    Conservation_Credit = numeric()
  )
  
  for (i in 1:num_simulations) {
    
    # Set the growth rate for this simulation
    df$Timber_Growth[1] <- sampled_growth_rates[i]
    
    # Run the carbon credit model with the new growth rate
    result <- carbon_credit_model(df, harvest_rate)
    
    # Filter and calculate the credits generated
    credits_generated <- result |>
      filter(Year >= 2024 & Year <= 2034) |>
      mutate(removal_credit = removal_credit * 1000, conservation_credit = conservation_credit * 1000)
    
    # Prepare data for this simulation
    simulation_data <- data.frame(
      Simulation = rep(i, nrow(credits_generated)),
      Year = credits_generated$Year,
      Growth_Rate = rep(sampled_growth_rates[i], nrow(credits_generated)),
      Removal_Credit = credits_generated$removal_credit,
      Conservation_Credit = credits_generated$conservation_credit
    )
    
    # Append to simulation_results
    simulation_results <- rbind(simulation_results, simulation_data)
  }
  
  return(simulation_results)
}

# Run simulations
simulation_results <- run_simulations(kanawha_river, harvest_rate, normal_distribution)


growth_rate_quantiles <- quantile(simulation_results$Growth_Rate, c(0.25, 0.5, 0.75))
print(growth_rate_quantiles)  # Check the computed quantiles

# Filter simulation_results for the specified growth rates and find which simulations have closest growth rate to 10th and 90th percentiler 

filtered_results_quants <- simulation_results %>%
  filter(between(Growth_Rate, growth_rate_quantiles[1] - 0.001, growth_rate_quantiles[1] + 0.001) |
           between(Growth_Rate, growth_rate_quantiles[3] - 0.001, growth_rate_quantiles[3] + 0.001))





# for mean 

filtered_results_mean <- simulation_results %>% 
  filter(between(Growth_Rate, growth_rate_quantiles[2] - 0.0001, growth_rate_quantiles[2] + 0.0001))|>
  filter(Simulation == 16)


ribbon_data <- filtered_results_quants %>%
  group_by(Year) %>%
  summarise(
    ymin = min(Removal_Credit),
    ymax = max(Removal_Credit)
  )

# Plotting with ggplot2
ggplot() +
  geom_line(data = filtered_results_mean, mapping = aes(x = Year, y = Removal_Credit), color = "red") +
  geom_ribbon(data = ribbon_data, aes(x = Year, ymin = ymin, ymax = ymax), fill = "pink", alpha = 0.2) +
  labs(title = "Simulated Growth Rates and Removal Credits",
       x = "Year",
       y = "Removal Credit") +
  theme_minimal()


