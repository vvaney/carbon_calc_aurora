

carb_calc <- function(df) {
  
  live_stocking <- as.numeric(df$Live_Stocking[1])
  dead_stocking <- as.numeric(df$Dead_Stocking[1])
  growth_rate <- as.numeric(df$Timber_Growth[1])
  acres <- as.numeric(df$Acres[1])
  live_baseline <- as.numeric(df$Live_Baseline[1])
  dead_baseline <- as.numeric(df$Dead_Baseline[1])
  

  start_year <- as.numeric(df$Start_Year[1]) - 1
  end_year <- start_year + 20
  years <- start_year:end_year
  
  
  harvest_rate <- data.frame(
    harvest = c(0.35, 0.15, 0.05, rep(0, 18)),
    year = start_year:end_year # adjust years here 
  )
  
  
  
  total_carb_acre_growth <- numeric(length(years))
  net_carb_acre_growth <- numeric(length(years))
  carbon_growth <- numeric(length(years))
  baseline_assumptions <- numeric(length(years))
  total_carbon <- numeric(length(years))
  baseline <- numeric(length(years))
  
  total_carb_acre_growth[1] <- live_stocking + dead_stocking
  
  for (i in 1:length(years)) {
    net_carb_acre_growth[i] <- (growth_rate * live_stocking) * (1 - harvest_rate$harvest[i])
    gross_live_C_acre <- live_stocking * growth_rate
    if (i > 1) {
      total_carb_acre_growth[i] <- total_carb_acre_growth[i - 1] + net_carb_acre_growth[i - 1]
    }
    carbon_growth[i] <- (net_carb_acre_growth[i] * acres) / 1000
    baseline_assumptions[i] <- dead_baseline + live_baseline
    
    total_carbon[i] <- (total_carb_acre_growth[i] * acres) / 1000
    baseline[i] <- (baseline_assumptions[i] * acres) / 1000
  }
  
  actual_harvested_c <- numeric(length(years))
  baseline_harvest <- numeric(length(years))
  diff_base_harvest <- numeric(length(years))
  GHG_reduction <- numeric(length(years))
  qualified_GHG <- numeric(length(years))
  secondary_GHG <- numeric(length(years))
  net <- numeric(length(years))
  comp_buffer <- numeric(length(years)) 
  comp_credits <- numeric(length(years))
  
  miaow <- data.frame(
    years = years,
    total_carbon = total_carbon,
    baseline = baseline
    
  )
    merch_timber <- 0.53
    stored_landfill <- 0.40
    market_resp <- 0.80
    secondary_impacts <- 0.20
    
  
  
  gross_live_C_acre <- live_stocking * growth_rate
  
  for (i in 1:length(years)) {
    if (i == 1) {
      qualified_GHG[i] <- miaow$total_carbon[i] - miaow$baseline[i]
    } else {
      qualified_GHG[i] <- miaow$total_carbon[i] - miaow$total_carbon[i - 1] - (miaow$baseline[i] - miaow$baseline[i - 1])
    }
    actual_harvested_c[i] <- (gross_live_C_acre - net_carb_acre_growth[i]) * acres / 1000
    baseline_harvest[i] <- (live_baseline * acres * growth_rate + ((live_stocking - live_baseline) * acres / 100 * 2)) / 1000
    diff_base_harvest[i] <- actual_harvested_c[i] - baseline_harvest[i]
    GHG_reduction[i] <- diff_base_harvest[i] * merch_timber * stored_landfill * market_resp
    secondary_GHG[i] <- diff_base_harvest[i] * secondary_impacts
    net[i] <- qualified_GHG[i] + GHG_reduction[i] + secondary_GHG[i]
    comp_buffer[i] <- net[i] * 0.176
  }
  

  resultz <- data.frame(
    years = years,
    net = net,
    comp_buffer = comp_buffer)
  
  
  for (i in 1:(length(years) - 1)) {
    comp_credits[i + 1] <- net[i + 1] - comp_buffer[i + 1]
  }

  
results <- data.frame(
  years = years,
  comp_credits = comp_credits
)

  return(results)
}

comp_inputs <- read.csv("inputs_compliance.csv")

# Example usage for Marmet
inputs_carbon_cred <- comp_inputs %>%
  filter(name == "Glacial Lakes Compliance")
  
compliance_credits <- carb_calc(inputs_carbon_cred )




################ CONSERVATION CREDIT PRICING ###########################

# read in your price curve
price_compliance <- read.csv("compliance_price_curve.csv")



###########################
# calculate Prices
###########################


Value_Estimator <- function(years, price) {
  
compliance_value <- numeric(length(years))

credits <-compliance_credits$comp_credits
  
  for (i in 1:length(years)) {
    compliance_value[i] <- (credits[i] * price[i]) *1000
  }
  
  values <- data.frame(
    year = years,
    compliance_value = compliance_value
  )
  
  return(values)
}

price <-  price_compliance$Compliance
years = 2024:2034 #select time frame!!!


# OUTPUT
dollar_value <- Value_Estimator(years,price)




################# NPV Estimator ##########


NPV_CALC <- function(compliance_NPV_input){
  
  compliance_NPV <- c()
  
  
  compliance_NPV <- sum(compliance_NPV_input/ (1 + 0.1)^(1:length(compliance_NPV_input)))

  
  
  NPV <- data.frame(
    compliance_NPV = compliance_NPV
  )
  
  
  return(NPV)
}


compliance_NPV_input <- dollar_value$compliance_value

NPV <- NPV_CALC(compliance_NPV_input)


















