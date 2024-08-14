library(purrr)
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



#########################
# Pre function processing 
#########################

# Load Data
removal_price <- read.csv("removal_price.csv")
conservation_price <- read.csv("conservation_price.csv")
pink_box_input <- read.csv("Pink_Box_Input_file.csv")
pink_box_input$acres <- as.numeric(pink_box_input$acres)
voluntary_input <- read.csv("Inputs_Voluntary.csv")

voluntary_input <- voluntary_input |>
  filter(Group == "Kanawha River")

voluntary_input$dynamic <- 0.89





# Filter and mutate credits
credits <- result |>
  filter(Year < 2043) |>
  mutate(
    removal_credit = removal_credit * 1000,
    conservation_credit = conservation_credit * 1000
  )



# add years leading up to start date 
#add_yrs <- data.frame()



#----------------------------------------------------------##-------------------------------------------------


##################
# Define Function
################


PresentValueEst <- function(pb_inputs,global_inputs, years, r, c, df) {
  removal_cred <- df$removal_credit
  r_price <- r$RemovalPrice
  CMDA <- global_inputs$CMDA
  cap <- global_inputs$capitalized_c_price
  cap_mult <- global_inputs$cap_mult
  discount_factor <- c(1,0.91,0.83,0.75,0.68,0.62,0.56,0.51,0.47,0.42,0.39,0.35,0.32,0.29,0.26,0.24,0.22,0.20,0.18)
  inflation <- global_inputs$inflation_rate
  dynamic <- voluntary_input$dynamic
  
  acres <- as.numeric(filtered$Acres)
  misc_exp <- as.numeric(voluntary_inputs$Misc_exp)
  discount_rate <- global_inputs$discount_rate
  inflation <- global_inputs$inflation_rate
  
  ######### Voluntary Removal #######
  # Ensure removal_cred has the same length as years
  if (length(removal_cred) < length(years)) {
    removal_cred <- c(removal_cred, rep(0, length(years) - length(removal_cred)))
  }
  
  # Shift by one
  Revenue <- c(0, (removal_cred[1:(length(years) - 1)] * r_price[2:length(years)]) / 1000)
  min_CMDA <- -1 * (Revenue * CMDA)
  VSUM <- min_CMDA + Revenue
  
  terminal <- ifelse(years == 2026, ((removal_cred[2:length(years)] * cap) / 1000) * (1 - CMDA) * cap_mult, 0)
  
  undiscounted_value <- terminal + VSUM
  PV <- sum(undiscounted_value[1:4] * discount_factor[1:4])
  
  ########## Other Revenue 
  orev <- numeric(length(years))
  Misc_Revenue <- numeric(length(years))
  
  orev[1] <- misc_exp
  
  for (i in 2:length(years)) {
    orev[i] <- orev[i - 1] * (1 + inflation)
    Misc_Revenue[i - 1] <- (orev[i - 1] * acres) / 1000
  }

  other_rev <- ifelse(years == 2026, 
                      Misc_Revenue[match(2027, years)] * cap_mult + Misc_Revenue[match(2026,years)], 
                      ifelse(years < 2026, 
                             Misc_Revenue, 
                             0))
  
  
  other_revs <- sum(other_rev * discount_factor[1:4])
  
  ### timber ###
  beg_chords <- numeric(length(years))
  end_chords <- numeric(length(years))
  minus_harvest <- numeric(length(years))
  price <- numeric(length(years))
  undiscounted <- numeric(length(years))

  start_chords <- pink_box_input$TotalCords
  growth <- pink_box_input$BlendedGrowth
  start_price <- pink_box_input$TimberPricing
  harvests <- c(0.35,0.15,0.05)


  beg_chords[1] <- start_chords
  price[1] <- start_price

  for (i in 2:length(years)){
    beg_chords[i] <- (beg_chords[i-1] * growth) + (beg_chords[i-1]) 
    end_chords[i-1] <- beg_chords[i-1]*growth
    minus_harvest[i-1] <- (end_chords[i-1]*harvests[i-1])*1000
    price[i] <- price[i-1]* (1 + inflation)
    undiscounted[i-1] <- (minus_harvest[i-1] * price[i-1])/1000
    
  }
  
timber_pv <- sum(discount_factor[1:3] * undiscounted[1:3])

## tying in conservation and removal



#this is a bandaid 
new_row <- data.frame(Year = 2023, 
                      conservation_credit = 0, 
                      jj = 0, 
                      reven = 0)

# Modify your existing code to add the new row at the beginning
conservation_credit <- df |>
  select(Year, conservation_credit) |>
  inner_join(c, by = "Year") |>
  mutate(jj = conservation_credit * dynamic[1]) |>
  mutate(reven = ((jj * ConservationPrice) / 1000) - (CMDA * (jj * ConservationPrice) / 1000)) |>
  bind_rows(new_row) |>
  arrange((Year))  # Optional: arrange if you want to keep it sorted by Year


reven  <- conservation_credit$reven
conservation_NPVC <- sum(reven*discount_factor)
conservation_credit$conservation_NPVC <- conservation_NPVC


  
  prelim_df <- data.frame(
    VoluntaryRemovals = PV,
    other_revs = other_revs,
    timber_pv = timber_pv,
    conservation_NPVC = conservation_NPVC,
    undiscounted = undiscounted)

final <- prelim_df[1,]    
        
    
#final step 
final$totalNPV <- sum(final$VoluntaryRemovals,final$other_revs,final$timber_pv,final$conservation_NPVC)  
    
  
  return(final)
}




#######################
#applying the function
#######################

# Example usage:
pink_box_input <- pink_box_input |>
  filter(Group == "Kanawha River") # filter for property of interest

years <- 2023:2041

PV_summary <- PresentValueEst(pink_box_input,global_inputs, years, removal_price, conservation_price, credits)

# Check result
print(PV_summary)


PVofRev <- PV_summary %>%
  slice(1)


#--------------------------------------------------------------------------------------------------------

###############################################
# growing out expenses and calculating NPV/acre
################################################

expenses <- read.csv("expenses.csv")

expenses <- expenses |>
  filter(Group == "Kanawha River")|> # choose your property one more time 
  slice(1) 

expenses$RoadsMaint <- as.numeric(expenses$RoadsMaint)
expenses$RdsHarvest <- as.numeric(expenses$RdsHarvest)
expenses$PropertyManager <- as.numeric(expenses$PropertyManager)
expenses$PropertyManagement <- as.numeric(expenses$PropertyManagement)
expenses$AdmOverhead <- as.numeric(expenses$AdmOverhead)
expenses$TaxesInsur <- as.numeric(expenses$TaxesInsur)


expenses$year <- 2023

#check to see how many acres
print(pink_box_input$acres)
#check to see start chords

print(pink_box_input$TotalCords)



##############
# the function
##############
NPV_acre <- function(expenses, pink_box_input, PVofRev, years = 19) {
  # Initialize vectors to store the values for each year
  roads_maintenance <- numeric(years)
  roads_harvest <- numeric(years)
  property_Manager <- numeric(years)
  property_Management <- numeric(years)
  adm_over <- numeric(years)
  taxes_insurance <- numeric(years)
  year <- numeric(years)
  beg_chords <- numeric(years)
  end_chords <- numeric(years)
  minus_harvest <- rep(NA, years)  # Initialize with NA
  total_operating_expenses <- numeric(years)
  
  # Set the initial values
  roads_maintenance[1] <- expenses$RoadsMaint
  roads_harvest[1] <- expenses$RdsHarvest
  property_Manager[1] <- expenses$PropertyManager
  property_Management[1] <- expenses$PropertyManagement
  adm_over[1] <- expenses$AdmOverhead
  taxes_insurance[1] <- expenses$TaxesInsur
  growth <- pink_box_input$BlendedGrowth
  acres <- pink_box_input$acres
  harvests <- c(0.35, 0.15, 0.05)
  inflation <- global_inputs$inflation_rate
  exp_mult <- global_inputs$exp_mult
  startacres <- pink_box_input$startacres

  
  year[1] <- expenses$year
  beg_chords[1] <- pink_box_input$TotalCords
  
  # Calculate expenses over the years
  for (i in 2:years) {
    # Calculate beg_chords and end_chords
    beg_chords[i] <- (beg_chords[i-1] * growth) + beg_chords[i-1]
    end_chords[i-1] <- beg_chords[i-1] * growth
    
    # Calculate minus_harvest only for the first three years
    if (i <= 4) {
      minus_harvest[i-1] <- -1 * (end_chords[i-1] * harvests[min(i-1, length(harvests))])
    }
    
    # Update expenses with inflation
    roads_maintenance[i] <- (roads_maintenance[i-1] * (1 + inflation))
    roads_harvest[i] <- (roads_harvest[i-1] * (1 + inflation))
    property_Manager[i] <- (property_Manager[i-1] * (1 + inflation))
    property_Management[i] <- (property_Management[i-1] * (1 + inflation))
    adm_over[i] <- (adm_over[i-1] * (1 + inflation))
    taxes_insurance[i] <- (taxes_insurance[i-1] * (1 + inflation)) 
    year[i] <- year[i-1] + 1
  }
  
  # Adjust expenses based on acres
  for (i in 1:years) {
    roads_maintenance[i] <- -1 * (roads_maintenance[i] * acres) / 1000
    # Only apply minus_harvest where it's not NA
    if (!is.na(minus_harvest[i])) {
      roads_harvest[i] <- roads_harvest[i] * minus_harvest[i]
    } else {
      roads_harvest[i] <- NA  # Keep NA where applicable
    }
    property_Manager[i] <- -1 * (property_Manager[i] * acres) / 1000
    property_Management[i] <- -1 * (property_Management[i] * acres) / 1000
    adm_over[i] <- -1 * (adm_over[i] * acres) / 1000
    taxes_insurance[i] <- -1 * (taxes_insurance[i] * acres) / 1000
    
    # Calculate total operating expenses
    total_operating_expenses[i] <- sum(roads_maintenance[i], roads_harvest[i], property_Manager[i], property_Management[i],
                                       adm_over[i], taxes_insurance[i], na.rm = TRUE)
  }
  
  # Adjust total operating expenses for 2026 based on 2027 value
  year_2026_index <- which(year == 2026)
  year_2027_index <- which(year == 2027)
  
  
  if (length(year_2026_index) > 0 && length(year_2027_index) > 0) {
    total_operating_expenses[year_2026_index] <- total_operating_expenses[year_2027_index] * exp_mult + total_operating_expenses[year_2026_index]
  }
  
  
  # Create a data frame with the results
  expen <- data.frame(
    Year = year,
    TotalOperatingExpenses = total_operating_expenses
  )
  
  discount_factor <- c(1,0.91,0.83,0.75)
  yay <- expen$TotalOperatingExpenses[1:4]
  hello <- sum(yay*discount_factor)
  
  
  #tying everything together 
  
  voluntary_removal <- PVofRev$VoluntaryRemovals
  orev <- PVofRev$other_revs
  timber_p <- PVofRev$timber_pv
  conservation_NPV <- PVofRev$conservation_NPV
  total_NPV <- PVofRev$totalNPV
  
  
  q1 <- ((voluntary_removal/total_NPV*hello) + voluntary_removal)*1000/startacres
  q2 <- ((orev/total_NPV*hello) + orev)*1000/startacres
  q3 <- ((timber_p/total_NPV*hello) + timber_p)*1000/startacres
  q4 <- ((conservation_NPV/total_NPV*hello) + conservation_NPV)*1000/startacres
  
  final <- data.frame(
  voluntary_removals = q1,
  other_revenue = q2,
  timber_PV = q3,
  conservation_NPV = q4,
  total_NPV_per_acre = sum(q1,q2,q3,q4)
  )
  

  
  
  return(final)
  
}

# Run the function to grow the expenses
grown_expenses <- NPV_acre(expenses, pink_box_input,PVofRev)
print(grown_expenses)




# Assuming grown_expenses and PV_summary are already created

# Extract necessary values
PV_summary_values <- c(PV_summary$VoluntaryRemovals, PV_summary$other_revs, PV_summary$timber_pv, PV_summary$conservation_NPVC, PV_summary$totalNPV)
grown_expenses_values <- c(grown_expenses$voluntary_removals, grown_expenses$other_revenue, grown_expenses$timber_PV, grown_expenses$conservation_NPV, grown_expenses$total_NPV_per_acre)

# Combine into one data frame
final_table <- data.frame(
  `Present Value Summary` = c("Voluntary Removals", "Other Revenue", "Timber", "Conservation NPV", "Total NPV"),
  `PV of Rev.` = PV_summary_values,
  `NPV/acre` = grown_expenses_values
)

# Print the final table
print(final_table)








