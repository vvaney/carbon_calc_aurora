# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(lubridate)

# Main script starts here

# Load your voluntary inputs data
voluntary_inputs <- read.csv("Inputs_Voluntary.csv")
voluntary_inputs <- voluntary_inputs[-1, ]  # Assuming first row is not needed



# Load price data
removal_price <- read.csv("removal_price.csv")
conservation_price <- read.csv("conservation_price.csv")



# Combine prices into one dataframe
prices <- full_join(removal_price, conservation_price) %>%
  rename(Removal = RemovalPrice,
         Conservation = ConservationPrice) %>%
  pivot_longer(cols = c(Removal, Conservation), names_to = "Credit Type", values_to = "Price")



# Get unique properties
unique_properties <- unique(voluntary_inputs$Group)



# Initialize an empty list to store results
results_list <- list()



# Iterate over each property
for (property in unique_properties) {
  
  # Filter the data for the current property
  filtered <- voluntary_inputs %>%
    filter(Group == property)
  
  # Extract the start year from the filtered data
  start_year <- as.numeric(filtered$Start_Year[1])
  
 
  harvest_rate <- data.frame(
    harvest = c(0.15, 0.05, 0, rep(0, 17)),
    year = start_year:(start_year + 19)
  )
  
 
  result <- carbon_credit_model(filtered, harvest_rate)
  

  result <- result %>% mutate(Property = property)
  
 
  results_list[[property]] <- result
}


all_results <- bind_rows(lapply(names(results_list), function(property) {
  results_list[[property]]
}))

# Further analysis or plotting can be done with the `all_results` dataframe
credits_generated <- all_results %>%
  filter(Year < 2034) %>%
  #mutate(removal_credit = removal_credit * 1000, conservation_credit = conservation_credit * 1000) %>%
  rename(Removal = removal_credit, Conservation = conservation_credit) %>%
  pivot_longer(cols = c(Removal, Conservation), names_to = "Credit Type", values_to = "Credits")


creditValue <- left_join(credits_generated, prices) %>%
  mutate(Value = Credits * Price)


discount_rate <- 0.1
start_year <- year(today()) - 1

# Calculate the Present Value (PV) for each credit type and property
carbonPV <- creditValue %>%
  mutate(Period = Year - start_year) %>%
  mutate(PV = Value / (1 + discount_rate) ^ Period) %>%
  group_by(Property, `Credit Type`) %>%
  summarise(PV = sum(PV), .groups = 'drop')




reg_plt <- ggplot(carbonPV, aes(x = Property, y = PV, fill = `Credit Type`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Removal" = "#7f216f", "Conservation" = "#008098")) +  # Custom colors for Credit Types
  theme_minimal() +
  labs(title = "Present Value of Carbon Credits by Property",
       x = "Property",
       y = "Present Value (PV)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Create the plot with custom colors
p <- ggplot(carbonPV, aes(x = Property, y = PV, fill = `Credit Type`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Removal" = "#7f216f", "Conservation" = "#008098")) +  # Custom colors for Credit Types
  theme_minimal() +
  labs(title = "Present Value of Carbon Credits by Property",
       x = "Property",
       y = "Present Value (PV)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_states(Property, transition_length = 2, state_length = 1, wrap = FALSE) +
  enter_fade() +
  exit_fade() +
  ease_aes('sine-in-out') +
  shadow_mark() +  # Keep previous bars visible
  ggtitle("Present Value by Property: {closest_state}")

# Render the animation and save as a GIF
animate(p, renderer = gifski_renderer("carbonPV_by_property.gif"), width = 800, height = 600, duration = 10, fps = 2)
