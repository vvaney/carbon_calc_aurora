library(dplyr)
library(tidyverse)
library(FinancialMath)
library(ggplot2)
library(tidyr)
library(gganimate)
library(gifski)
library(truncnorm)

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


