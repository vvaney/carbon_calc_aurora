###############
#Required Package(s)
###############


library(data.table)
library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(tidyverse)
library(openxlsx)

###############
#Input Data
###############

treeList <- read_csv("FVS_Processing_REPO/EX.Property/Hilton/hilton_tree_list.csv")
##############################################################


###############
#Data Inputs
###############


treeList$YEAR <- 2005

treeList$VARIANT <- "NE" # Variant code

treeList$LOCATION <- 922 # Location 

treeList$add_tr <- 1     # Static value


###################
#set site index
###################

#if your tree list comes with its own site index values

treeList$SITE_INDEX <- NA
treeList$Factor <- NA


#################################################################################################
#if your data does not contain site index values specify your site index and dominant species

# treeList <- treeList |>
#   mutate(SITE_INDEX = if_else(dbh == 0, ht, SITE_INDEX))


# choose tree with highest basal area and add the site index calculated in prev script under
 # treeList <- treeList |>
 #   mutate(SITE_INDEX = ifelse(SpeciesCode == "HM", 55.38919, SITE_INDEX))



########################
# MODIFYING THE TREE LIST🌳
########################

# define tree species and convert it to FIA 2-3 digit code use 
  #use below csv file to help you convert them 

#FIA_code <- read.csv("FIATreeSpeciesCode.csv")



tree_1 <- treeList |>
  mutate(SPP = case_when(
    SpeciesCode == "BA" ~ 543,
    SpeciesCode == "BC" ~ 762,
    SpeciesCode == "BE" ~ 531,
    SpeciesCode == "BF" ~ 12,
    SpeciesCode == "BN" ~ 601,
    SpeciesCode == "BS" ~ 95,
    SpeciesCode == "BW" ~ 950,
    SpeciesCode == "CE" ~ 40,
    SpeciesCode == "GB" ~ 379,
    SpeciesCode == "HE" ~ 260,
    SpeciesCode == "HM" ~ 318,
    SpeciesCode == "NC" ~ 998,
    SpeciesCode == "OH" ~ 962,
    SpeciesCode == "OO" ~ 962,
    SpeciesCode == "OS" ~ 391,
    SpeciesCode == "PO" ~ 740,
    SpeciesCode == "QA" ~ 746,
    SpeciesCode == "RM" ~ 316,
    SpeciesCode == "RO" ~ 833,
    SpeciesCode == "RP" ~ 125,
    SpeciesCode == "SB" ~ 998,
    SpeciesCode == "SP" ~ 90,
    SpeciesCode == "TA" ~ 71,
    SpeciesCode == "WA" ~ 541,
    SpeciesCode == "WB" ~ 375,
    SpeciesCode == "WO" ~ 802,
    SpeciesCode == "WP" ~ 129,
    SpeciesCode == "YB" ~ 371,
    TRUE ~ 998  # Default case
  ))|>
  
  
  # select all bars you need
  select(PointID,TreeNumber, YEAR, SPP, VARIANT,DBH, LOCATION, add_tr,SITE_INDEX,Factor)|>
  rename(
    PLOT_ID = PointID,
    TREE_ID = TreeNumber,
    DBH = DBH,
    Factor = Factor,
    TREE_COUNT = add_tr
    
    #optional
    #HT = ht, #put NA if no data
    #LAT = `tree_PLOT::gpsY`, #put NA if no data
    #LONG = `tree_PLOT::gpsX` #put NA if no data
  )  


tree_1$STAND_ID <- tree_1$PLOT_ID




##########################
# CREATING FVS_TreeInit 
##########################

make_FVS_TreeInit <- function(){
  tibble(
    STAND_CN = NA,
    STAND_ID = NA,
    PLOT_CN = NA,
    PLOT_ID = NA,
    STANDPLOT_CN = NA,
    STANDPLOT_ID = NA,
    TREE_CN = NA,
    TREE_ID = NA,
    YEAR = NA,
    TAG_ID = NA,
    TREE_COUNT = NA,
    HISTORY = NA,
    SPECIES = NA,
    DIAMETER = NA,
    DIAMETER_HT = NA,
    DG = NA,
    HT = NA,
    HTG = NA,
    HTTOPK = NA,
    HT_TO_LIVE_CROWN = NA,
    CRRATIO = NA,
    DAMAGE1 = NA,
    SEVERITY1 = NA,
    DAMAGE2 = NA,
    SEVERITY2 = NA,
    DAMAGE3 = NA,
    SEVERITY3 = NA,
    DEFECT_CUBIC = NA,
    DEFECT_BOARD = NA,
    TREEVALUE = NA,
    PRESCRIPTION = NA,
    AGE = NA,
    SLOPE = NA,
    ASPECT = NA,
    PV_CODE = NA,
    PV_REF_CODE = NA,
    TOPOCODE = NA,
    SITEPREP = NA) 
}

#---------------------------------

FVS_TreeInit_blank <- make_FVS_TreeInit()
tree <- tree_1
#---------------------------------

TreeInit <- FVS_TreeInit_blank %>% 
  add_row(
    # Gotta have these
    STAND_CN = tree$PLOT_ID,
    STAND_ID = tree$PLOT_ID, 
    PLOT_CN = NA,
    PLOT_ID = tree$PLOT_ID,
    STANDPLOT_ID = NA,
    STANDPLOT_CN = NA,
    TREE_CN = NA,
    TREE_ID = tree$TREE_ID, 
    SPECIES = tree$SPP, 
    DIAMETER = tree$DBH,
    YEAR = tree$YEAR,
    # Good to have these
    HT = NA,
    HT_TO_LIVE_CROWN = NA,
    CRRATIO = NA,
    AGE = NA,
    # Optional
    TREE_COUNT = 1,
    HISTORY = NA,
    DIAMETER_HT = NA,
    DG = NA,
    HTG = NA,
    DAMAGE1 = NA,
    SEVERITY1 = NA,
    DAMAGE2 = NA,
    SEVERITY2 = NA,
    DAMAGE3 = NA,
    SEVERITY3 = NA,
    DEFECT_CUBIC = NA,
    DEFECT_BOARD = NA,
    TREEVALUE = NA,
    PRESCRIPTION = NA,
    SLOPE = NA,
    ASPECT = NA,
    PV_CODE = NA,
    PV_REF_CODE = NA,
    TOPOCODE = NA,
    SITEPREP = NA
  ) %>% 
  drop_na(TREE_ID)


##########################
# CREATING FVS_PlotInit 
##########################



plot <- tree%>% 
  group_by(STAND_ID,PLOT_ID,YEAR,Factor,LOCATION,SPP,VARIANT) %>% 
  summarize(num_trees = n(),
            SITE_INDEX = mean(SITE_INDEX, na.rm = TRUE)) |> 
  select(STAND_ID,PLOT_ID,YEAR,Factor,SITE_INDEX,LOCATION,SPP,VARIANT)




num_plot <- plot %>% 
  group_by(STAND_ID, YEAR) %>% 
  summarize(num_plots = n()) %>% 
  select(STAND_ID, YEAR, num_plots)
num_plot


# 
# # CREATE make_FVS_PlotInit() function---------------------------------------------------------
# 
# make_FVS_PlotInit <- function(){
#   tibble(
#     STAND_ID = NA,	
#     PLOT_ID	= NA,
#     STANDPLOT_ID = NA,	
#     VARIANT	= NA,
#     INV_YEAR = NA,	
#     GROUPS = NA,
#     ADDFILES = NA,
#     FVSKEYWORDS = NA,
#     GIS_LINK = NA,
#     PROJECT_NAME = NA,
#     LATITUDE = NA,
#     LONGITUDE = NA,
#     REGION = NA,
#     FOREST = NA,
#     DISTRICT = NA,
#     COMPARTMENT = NA,
#     LOCATION = NA,
#     ECOREGION = NA,
#     PV_CODE = NA,
#     PV_REF_CODE = NA,
#     AGE = NA,
#     ASPECT = NA,
#     SLOPE = NA,
#     ELEVATION = NA,
#     ELEVFT = NA,
#     BASAL_AREA_FACTOR = NA,
#     INV_PLOT_SIZE = NA,
#     BRK_DBH = NA,
#     NUM_PLOTS = NA,
#     NONSTK_PLOTS = NA,
#     SAM_WT = NA,
#     STK_PCNT = NA,
#     DG_TRANS = NA,
#     DG_MEASURE = NA,
#     HTG_TRANS = NA,
#     HTG_MEASURE = NA,
#     MORT_MEASURE = NA,
#     MAX_BA = NA,
#     MAX_SDI = NA,
#     SITE_SPECIES = NA,
#     SITE_INDEX = NA,
#     MODEL_TYPE = NA,
#     PHYSIO_REGION = NA,
#     FOREST_TYPE = NA,
#     STATE	= NA,
#     COUNTY = NA,
#     FUEL_MODEL = NA,
#     FUEL_0_25 = NA,
#     FUEL_25_1 = NA,
#     FUEL_1_3 = NA,
#     FUEL_3_6_H = NA,
#     FUEL_6_12_H = NA,
#     FUEL_12_20_H = NA,
#     FUEL_20_35_H = NA,
#     FUEL_35_50_H = NA,
#     FUEL_GT_50_H = NA,
#     FUEL_3_6_S = NA,
#     FUEL_6_12_S = NA,
#     FUEL_12_20_S = NA,
#     FUEL_20_35_S = NA,
#     FUEL_35_50_S = NA,
#     FUEL_GT_50_S = NA,
#     FUEL_LITTER = NA,
#     FUEL_DUFF = NA,
#     PHOTO_REF = NA,
#     PHOTO_CODE = NA)
# }
# 
# # CREATE blank template for TreeInit sheet
# 
# FVS_PlotInit_blank <- make_FVS_PlotInit()
# 
# 
# # APPLY example plot records FVS_PlotInit_blank
# 
# PlotInit <- FVS_PlotInit_blank %>% 
#   add_row(
#     # Gotta have these
#     STAND_ID = plot$PLOT_ID,	
#     PLOT_ID	= plot$PLOT_ID,
#     STANDPLOT_ID = NA,	
#     VARIANT	= plot$VARIANT,
#     INV_YEAR = plot$YEAR,	
#     GROUPS = "All_Stands",
#     ADDFILES = NA,
#     FVSKEYWORDS = NA,
#     GIS_LINK = NA,
#     PROJECT_NAME = NA,
#     LATITUDE = plot$LAT,
#     LONGITUDE = plot$LONG,
#     REGION = NA,
#     FOREST = NA,
#     DISTRICT = NA,
#     COMPARTMENT = NA,
#     LOCATION = plot$LOCATION,
#     ECOREGION = NA,
#     PV_CODE = NA,
#     PV_REF_CODE = NA,
#     AGE = NA,
#     ASPECT = NA,
#     SLOPE = NA,
#     ELEVATION = NA,
#     ELEVFT = NA,
#     BASAL_AREA_FACTOR = plot$Factor,
#     INV_PLOT_SIZE = NA,
#     BRK_DBH = NA,
#     NUM_PLOTS = as.numeric(nrow(plot)),
#     NONSTK_PLOTS = NA,
#     SAM_WT = NA,
#     STK_PCNT = NA,
#     DG_TRANS = NA,
#     DG_MEASURE = NA,
#     HTG_TRANS = NA,
#     HTG_MEASURE = NA,
#     MORT_MEASURE = NA,
#     MAX_BA = NA,
#     MAX_SDI = NA,
#     SITE_SPECIES = plot$SPP,
#     SITE_INDEX = plot$SiteIndex,
#     MODEL_TYPE = NA,
#     PHYSIO_REGION = NA,
#     FOREST_TYPE = NA,
#     STATE	= NA,
#     COUNTY = NA,
#     FUEL_MODEL = NA,
#     FUEL_0_25 = NA,
#     FUEL_25_1 = NA,
#     FUEL_1_3 = NA,
#     FUEL_3_6_H = NA,
#     FUEL_6_12_H = NA,
#     FUEL_12_20_H = NA,
#     FUEL_20_35_H = NA,
#     FUEL_35_50_H = NA,
#     FUEL_GT_50_H = NA,
#     FUEL_3_6_S = NA,
#     FUEL_6_12_S = NA,
#     FUEL_12_20_S = NA,
#     FUEL_20_35_S = NA,
#     FUEL_35_50_S = NA,
#     FUEL_GT_50_S = NA,
#     FUEL_LITTER = NA,
#     FUEL_DUFF = NA,
#     PHOTO_REF = NA,
#     PHOTO_CODE = NA) %>% 
#   drop_na(PLOT_ID)
# 
# PlotInit <- inner_join(PlotInit, num_plot)
# 
# PlotInit <- PlotInit %>% 
#   select(-c(NUM_PLOTS))
# 
# PlotInit <- PlotInit %>%
#   rename(NUM_PLOTS = num_plots) %>% 
#   relocate(NUM_PLOTS, .after = BRK_DBH)
# 

# Summarize PlotInit to create StandInit (list of stands)------------------------------------



stand <- plot %>% 
  group_by(STAND_ID,YEAR,Factor,LOCATION,SPP,VARIANT) %>% 
  summarize(num_trees = n(),
            SITE_INDEX = mean(SITE_INDEX, na.rm = TRUE)) |> 
  select(STAND_ID,YEAR,Factor,SITE_INDEX,LOCATION,SPP,VARIANT)



# CREATE make_FVS_StandInit() function-------------------------------------

make_FVS_StandInit <- function(){
  tibble(
    STAND_CN = NA,
    STAND_ID = NA,	
    VARIANT = NA,	
    INV_YEAR = NA,	
    GROUPS = NA,	
    ADDFILES = NA,	
    FVSKEYWORDS = NA,	
    GIS_LINK = NA,	
    PROJECT_NAME = NA,
    LATITUDE = NA,	
    LONGITUDE = NA,	
    REGION = NA,
    FOREST = NA,
    DISTRICT = NA,	
    COMPARTMENT = NA,
    LOCATION	= NA,
    ECOREGION = NA,
    PV_CODE = NA,	
    PV_REF_CODE = NA,
    AGE = NA,
    ASPECT = NA,	
    SLOPE	= NA,
    ELEVATION	= NA,
    ELEVFT = NA,	
    BASAL_AREA_FACTOR = NA,
    INV_PLOT_SIZE = NA,
    BRK_DBH = NA,
    NUM_PLOTS = NA,
    NONSTK_PLOTS = NA,	
    SAM_WT = NA,	
    STK_PCNT = NA,
    DG_TRANS = NA,
    DG_MEASURE = NA,
    HTG_TRANS = NA,
    HTG_MEASURE = NA,	
    MORT_MEASURE = NA,	
    MAX_BA = NA,
    MAX_SDI	= NA,
    SITE_SPECIES = NA,	
    SITE_INDEX = NA,
    MODEL_TYPE = NA,
    PHYSIO_REGION = NA,
    FOREST_TYPE = NA,	
    STATE = NA,
    COUNTY = NA,	
    FUEL_MODEL = NA,
    FUEL_0_25 = NA,	
    FUEL_25_1	= NA,
    FUEL_1_3 = NA,
    FUEL_3_6_H= NA,
    FUEL_6_12_H	= NA,
    FUEL_12_20_H= NA,
    FUEL_20_35_H= NA,
    FUEL_35_50_H= NA,
    FUEL_GT_50_H= NA,
    FUEL_3_6_S= NA,
    FUEL_6_12_S	= NA,
    FUEL_12_20_S = NA,
    FUEL_20_35_S= NA,
    FUEL_35_50_S= NA,
    FUEL_GT_50_S= NA,
    FUEL_LITTER	= NA,
    FUEL_DUFF	= NA,
    PHOTO_REF	= NA,
    PHOTO_CODE = NA)
}

# CREATE blank template for StandInit sheet

FVS_StandInit_blank <- make_FVS_StandInit()

# APPLY example stand records FVS_StandInit_blank

# Apply example stand records to FVS_StandInit_blank
StandInit <- FVS_StandInit_blank |>
  add_row(
    STAND_CN = stand$STAND_ID,
    STAND_ID = stand$STAND_ID,
    VARIANT = stand$VARIANT,
    INV_YEAR = stand$YEAR,
    GROUPS = "All_Stands",
    FOREST = stand$LOCATION,
    BASAL_AREA_FACTOR = stand$Factor,
    NUM_PLOTS = nrow(plot),
    SITE_SPECIES = stand$SPP,
    SITE_INDEX = stand$SITE_INDEX
  ) |>
  drop_na(STAND_ID)


StandInit$STAND_ID <- as.factor(StandInit$STAND_ID)


StandInit <- inner_join(StandInit, num_plot)

StandInit <- StandInit %>% 
  select(-c(NUM_PLOTS))

StandInit <- StandInit %>%
  rename(NUM_PLOTS = num_plots) %>% 
  relocate(NUM_PLOTS, .after = BRK_DBH)

# Omit YEAR from TreeInit (not needed in FVS file)

TreeInit <- TreeInit %>% 
  select(-YEAR)



#---------------------------------------------------



# Assuming StandInit data frame is already defined

# Get unique STAND_IDs
unique_stand_ids <- unique(StandInit$STAND_ID)

# Initialize an empty list to store modified data frames
modified_stand_inits <- list()

# Loop through each unique STAND_ID
for (stand_id in unique_stand_ids) {
  # Subset StandInit for the current STAND_ID
  stand_init_subset <- StandInit[StandInit$STAND_ID == stand_id, ]
  
  # Check if there are multiple species present
  if (length(unique(stand_init_subset$SITE_SPECIES)) > 1) {
    # Check if species 998 is present
    if (998 %in% stand_init_subset$SITE_SPECIES) {
      # Remove species 998
      stand_init_subset <- stand_init_subset[stand_init_subset$SITE_SPECIES != 998, ]
    }
  }
  
  # Append modified subset to the list
  modified_stand_inits[[stand_id]] <- stand_init_subset
}

modified_stand_init <- modified_stand_inits %>%
  bind_rows() %>%
  group_by(STAND_ID)






StandInit <- modified_stand_init
StandInit$NUM_PLOTS <- NA



# Export FVS files to a single Excel workbook in the FVS-ready format-------------------------------

wb <- createWorkbook() 
addWorksheet(wb = wb, sheetName = "FVS_StandInit") 
writeData(wb = wb, sheet = 1, x = StandInit)

# addWorksheet(wb = wb, sheetName = "FVS_PlotInit") 
# writeData(wb = wb, sheet = 2, x = PlotInit) 

addWorksheet(wb = wb, sheetName = "FVS_TreeInit") 
writeDataTable(wb = wb, sheet = 2, x = TreeInit) 

saveWorkbook(wb, "updated_hilton.xlsx", overwrite = TRUE)

