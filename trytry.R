# Load necessary libraries
library(tidyverse)

# Load Data
removal_price <- read.csv("removal_price.csv")
conservation_price <- read.csv("conservation_price.csv")
pink_box_input <- read.csv("Pink_Box_Input_file.csv") %>%
  mutate(acres = as.numeric(acres)) %>%
  filter(Group == "Kanawha River")
voluntary_input <- read.csv("Inputs_Voluntary.csv") %>%
  filter(Group == "Kanawha River") %>%
  mutate(dynamic = 0.89)
expenses <- read.csv("expenses.csv") %>%
  filter(Group == "Kanawha River") %>%
  select(-Group)%>%
  slice(1)
  mutate(across(where(is.character), as.numeric),
         year = 2023)

credits_generated <- credits_generated|>
  filter(`Credit Type` == "Removal")


# Create years range
years <- 2023:2041

# Define global_inputs from previous context
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

# Present Value Estimation Function
PresentValueEst <- function(global_inputs, years, r, c, creds) {
  discount_factor <- c(1, 0.91, 0.83, 0.75)
  dynamic <- voluntary_input$dynamic[1]
  
  # Voluntary Removal
  removal_cred <- creds$Credits * 1000
  if (length(removal_cred) < length(years)) {
    removal_cred <- c(removal_cred, rep(0, length(years) - length(removal_cred)))
  }
  Revenue <- c(0, (removal_cred[1:(length(years) - 1)] * r$RemovalPrice[-1]) / 1000)
  terminal <- ifelse(years == 2026, (removal_cred[length(removal_cred)] * global_inputs$capitalized_c_price / 1000) * (1 - global_inputs$CMDA) * global_inputs$cap_mult, 0)
  VSUM <- Revenue * (1 - global_inputs$CMDA)
  PV <- sum((terminal + VSUM)[1:4] * discount_factor)
  
  # Other Revenue
  orev <- sapply(1:length(years), function(i) {
    voluntary_input$Misc_exp * (1 + global_inputs$inflation_rate)^(i-1)
  })
  other_rev <- ifelse(years == 2026, orev[4] * global_inputs$cap_mult + orev[3], orev)
  other_revs <- sum(other_rev * discount_factor)
  
  # Timber
  timber_pv <- sum(
    sapply(1:3, function(i) {
      start_cords <- pink_box_input$TotalCords * (pink_box_input$BlendedGrowth + 1)^(i-1)
      harvest <- c(0.35, 0.15, 0.05)[i] * start_cords * 1000
      timber_price <- pink_box_input$TimberPricing * (1 + global_inputs$inflation_rate)^(i-1)
      harvest * timber_price / 1000 * discount_factor[i]
    })
  )
  
  # Conservation and Removal
  conservation_revenue <- creds %>%
    mutate(
      conservation_credit = Credits * 1000,
      jj = conservation_credit * dynamic,
      reven = (jj * c$ConservationPrice / 1000) - (global_inputs$CMDA * jj * c$ConservationPrice / 1000)
    )
  conservation_NPVC <- sum(conservation_revenue$reven * discount_factor)
  
  data.frame(
    VoluntaryRemovals = PV,
    other_revs = other_revs,
    timber_pv = timber_pv,
    conservation_NPVC = conservation_NPVC,
    totalNPV = sum(PV, other_revs, timber_pv, conservation_NPVC)
  )
}

# Calculate NPV per acre
NPV_acre <- function(expenses, pink_box_input, PVofRev, years = 19) {
  inflation <- global_inputs$inflation_rate
  exp_mult <- global_inputs$exp_mult
  startacres <- pink_box_input$acres[1]
  
  # Calculate Total Operating Expenses with inflation
  expenses_list <- sapply(1:years, function(i) {
    expenses %>%
      mutate(across(-year, ~ .x * (1 + inflation)^(i-1))) %>%
      summarise(total_operating_expenses = sum(across(-year), na.rm = TRUE)) %>%
      pull(total_operating_expenses)
  })
  
  # Adjust for 2026
  expenses_list[4] <- expenses_list[5] * exp_mult + expenses_list[4]
  hello <- sum(expenses_list[1:4] * c(1, 0.91, 0.83, 0.75))
  
  # Calculate NPV per acre
  results <- PVofRev %>%
    summarise(
      q1 = ((VoluntaryRemovals / totalNPV * hello) + VoluntaryRemovals) * 1000 / startacres,
      q2 = ((other_revs / totalNPV * hello) + other_revs) * 1000 / startacres,
      q3 = ((timber_pv / totalNPV * hello) + timber_pv) * 1000 / startacres,
      q4 = ((conservation_NPVC / totalNPV * hello) + conservation_NPVC) * 1000 / startacres
    )
  
  results %>%
    mutate(total_NPV_per_acre = rowSums(across(everything()))) %>%
    select(everything())
}

# Run the functions and generate the final table
PV_summary <- PresentValueEst(global_inputs, years, removal_price, conservation_price, credits_generated)
grown_expenses <- NPV_acre(expenses, pink_box_input, PV_summary)

# Create the final table
final_table <- tibble(
  `Present Value Summary` = c("Voluntary Removals", "Other Revenue", "Timber", "Conservation NPV", "Total NPV"),
  `PV.of.Rev.` = c(PV_summary$VoluntaryRemovals, PV_summary$other_revs, PV_summary$timber_pv, PV_summary$conservation_NPVC, PV_summary$totalNPV),
  `NPV.acre` = c(grown_expenses$q1, grown_expenses$q2, grown_expenses$q3, grown_expenses$q4, grown_expenses$total_NPV_per_acre)
)

# Display the final table
print(final_table)
