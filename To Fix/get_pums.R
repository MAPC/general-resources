#### Set Up ####

# Use when Rproject is open
# root <- '../../../../Data/Working/Regional_Control_Totals/'

# Base root. Toggle between S: and K: drive depending on mapped paths on your computer
base <- "K:/DataServices/Projects/Current_Projects/"

# General K drive filepath
root <-"K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Regional_Control_Totals/"

cpi <- fread(paste0(root,"cpi_deflators_by_year.csv"))

# PUMS K drive filepath
pums_path <- "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/ipums_2021/"

# Set output filepath
output_path <- "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/outputs/"

# Reweighter files path
rwt_path <- paste0(base, "Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/")

# Set knitr path
knitr::opts_knit$set(root.dir = pums_path)

set.seed(351)

source("C:/Users/ziacovino/Desktop/statewidehousingplan/PUMS_Analysis/PUMS_Helper_Functions.R")

xw <- read.csv(paste0(rwt_path, "PUMS_data/PUMA10_RPA_crosswalk.csv")) |> 
  mutate(PUMA = str_pad(PUMA, 5, "left", pad = 0))

# Load MUnicipality to PUMA Crosswalk
muni2puma<-read_csv("K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/PUMS_data/pums_muni_inter.csv")%>%
  select(TOWN_ID, PUMACE10) |> 
  dplyr::rename(muni_id = TOWN_ID)  


mapc_pums_cleaning<-function(vintage_year, addl_vars = c()){  
  
  # Set PUMS vintage final year for {tidycensus} API query.
  vintage <- vintage_year
  
  # List of PUMS variables
  #' variable_list <- c(
  #'   #'RT', 
  #'  'PUMA', 'ADJHSG',
  #'  'ADJINC',
  #'  'NP', #'TYPEHUGQ',
  #'  'BDSP','RMSP','CPLT',
  #'  'HHLDRAGEP',
  #'  'HHLDRRAC1P','HHLDRHISP', 'HHT', 'HHT2', 
  #'  'HINCP', 'HUGCL', 'HUPAC', 'HUPAOC',
  #'                    'HUPARC', 'LNGI', 'MULTG', 'NPF', 'NPP', 'NR','NRC', 'PARTNER', 'PSF','R18',
  #'                    
  #'  'AGEP',
  #'  'RELSHIPP','RAC1P','OC',
  #'  'SFN','SFR',
  #'  'TEN', "SEX", "PINCP", 
  #'  "ESR",
  #'                    "MV", 
  #'  "GRPIP", 
  #'  "OCPIP", "SCH", "SCHG")
  
  #All of the variables pre-cleaned
  if(vintage_year >= 2012){
    variable_list<-c('PUMA', 'ADJINC', 'NP', 'BDSP','HINCP', 'AGEP',
                     'SFN', 'TEN', 'ESR', 'GRPIP', 'OCPIP')%>%
      #add any optional variables
      append(addl_vars)%>%
      #de deduplicate
      unique()
    
    pums <- pums_query(var.list = variable_list, yr = vintage, srvy = "acs5") |> 
      mutate(
        PUMA = as.character(str_pad(PUMA, width = 5, side = "left", pad = 0)),
        puma_id = PUMA
      )
  }
  
  if(vintage_year <2012){
    variable_list<-c('PUMA', 'ADJINC', 'NP', 'BDS','HINCP', 'AGEP',
                     'SFN', 'TEN', 'ESR', 'GRPIP', 'OCPIP')%>%
      #add any optional variables
      append(addl_vars)%>%
      #de deduplicate
      unique()
    
    
    pums <- pums_query(var.list = variable_list, yr = vintage, srvy = "acs5") |> 
      mutate(
        PUMA = as.character(str_pad(PUMA, width = 5, side = "left", pad = 0)),
        puma_id = PUMA
      )%>%
      rename(BDSP = BDS)
  }
  
  # Load PUMS variables
  
  
  
  # General Data Cleaning
  pums_cleaned <- pums |> 
    # Create a household flag for whether there is a child in a household.
    mutate(
      # Generate five-year age groupings to match UMDI population projections data.
      ageCAT6 = cut(as.numeric(AGEP), breaks = c(-Inf, seq(4, 84, 5), Inf), labels = 1:18),
      # Generate an age category binary:
      # (1) Persons older than 60 years old
      # (0) Persons 60 years old or younger
      ageCAT2 = case_when(
        as.numeric(AGEP) <= 60 ~ "1",
        as.numeric(AGEP) < 60 ~ "0",
        .default = "0"
      ),
      # Create a household flag for whether there is a child (a person under the age of 18) in the household.
      # (1) indicates a person is under the age of 18
      # (0) indicates a person is 18 or older.
      child = case_when(
        as.numeric(AGEP) < 18 ~ 1,
        as.numeric(AGEP) >= 18 ~ 0,
        .default = 0
      ),
      # Generate ten-year age groupings
      # ageCAT4 is the 2020 decennial census age breaks for age of householder - used for 2020 base year control total reweighter targets
      ageCAT4 = case_when(
        as.numeric(AGEP) %in% 0:14 ~ 0,
        as.numeric(AGEP) %in% 15:24 ~ 1,
        as.numeric(AGEP) %in% 25:34 ~ 2,
        as.numeric(AGEP) %in% 35:44 ~ 3,
        as.numeric(AGEP) %in% 45:54 ~ 4,
        as.numeric(AGEP) %in% 55:59 ~ 5,
        as.numeric(AGEP) %in% 60:64 ~ 6,
        as.numeric(AGEP) %in% 65:74 ~ 7,
        as.numeric(AGEP) %in% 75:84 ~ 8,
        as.numeric(AGEP) >= 85 ~ 9,
        .default = 0
      ), 
      # Counts as Worker
      # NOTE: Categories are:
      # (0) under 16 or not in labor force
      # (1) in labor force or non-civilian worker
      worker = case_when(
        # (b) indicates a person under the age of 16.
        # (6) indicates someone not in the laborforce
        (ESR == "b" | ESR == 6) ~ 0,
        # (1) indicates an employed civilian at work.
        # (2) indicates an employed civilian with a job but not at work.
        # (3) indicates civilian who is unemployed.
        # (4) Indicates an employed person in the armed forces. 
        # (5) indicates an employed person in the armed forces with a job but not at work.      
        (ESR == 1 | ESR == 2 | ESR == 3 | ESR == 4 | ESR == 5) ~ 1,
        # When ESR is NA, default to not in the laborforce.
        is.na(ESR) ~ 0,
        # If no information is present default to not in the laborforce.
        .default = 0
      ),
      # Create a grouping variable for types of relationships to the householder.
      # NOTE: 
      # (0) indicates a person in a household that is related in some way to the householder.
      # (1) indicates a person in a household unrelated to the householder.
      # RELSHIPP_Group = case_when(
      #   RELSHIPP %in% c(21,22,23,24,25,26,27,28,29,30,31,32,33) ~ 0,
      #   RELSHIPP %in% c(34,35,36) ~ 1,
      #   # Set a default flag for householders.
      #   .default = 0
      # ),
      # Create a grouping variable for owner and renter households.
      TEN_Group = case_when(
        # Owned with a mortage or free and clear -> "Own"
        TEN %in% c("1","2") ~ "Own",
        # Rented or occupied without payment of rent -> "Rent"
        TEN %in% c("3","4") ~ "Rent",
        .default = "Unknown"
      ),
      # Create a "Studio/Non-Defined Bedroom" variable to denote where households do
      # not have a bedroom number.
      studio_flag = case_when(
        as.numeric(BDSP) == 0 ~ "Studio/No Defined Bedroom",
        as.numeric(BDSP) != 0 ~ "Defined Bedroom",
        .default = "Unknown"
      ),
      # Redefine BDSP to label studios/non-defined bedroom units as One-Bedrooms
      # For purpose of calculating the number of people per bedrooms.
      BDSP = if_else(as.numeric(BDSP) == 0, 1, as.numeric(BDSP)),
      # Calculate the number of people per bedroom in a housing unit.
      PPB = as.numeric(NP)/BDSP,
      # Categorize each housing unit as overcrowded or not based on PPB.
      overcrowded.ppb = case_when(
        PPB > 2.0 ~ "1",
        PPB <= 2.0 ~ "0"
      ),
      # Convert SFN into a numeric column. Introduces NAs for people not in subfamilies
      SFN = as.numeric(SFN),
      # Replace the NA for non-subfamily members with 0.
      SFN = replace_na(SFN, 0),
      # Adjust Household income (HINCP) to constant dollars (max of acs year)
      HINCP_adj = as.numeric(HINCP)*as.numeric(ADJINC),
      HINCP_2021 = round(HINCP_adj*(cpi[year==2021, index] / cpi[year==vintage_year, index]), 2),
      # Household Income Groupings consistent with UrbanSim groupings
      HH.Income_Group = case_when(
        HINCP_2021 <= 39000 ~ "<=$39,000",
        HINCP_2021 > 39000 & HINCP_2021 <= 83000 ~ "$39,001-$83,000",
        HINCP_2021 > 83000 & HINCP_2021 <= 138000 ~ "$83,001-$138,000",
        HINCP_2021 > 138000 & HINCP_2021 <= 248000 ~ "$138,001-$248,000",
        HINCP_2021 > 248000 ~ ">$248,000",
        .default = "0"
      ),
      # # Householder Race Groupings. 
      # HHLDRRACE_Group = case_when(
      #   HHLDRRAC1P == "1" ~ "White Alone",
      #   HHLDRRAC1P == "2" ~ "Black or African American Alone",
      #   HHLDRRAC1P %in% c("3", "4", "5") ~ "AIAN Alone",
      #   HHLDRRAC1P == "6" ~ "Asian Alone",
      #   HHLDRRAC1P == "7" ~ "NHPI Alone",
      #   HHLDRRAC1P == "8" ~ "SOR Alone",
      #   HHLDRRAC1P == "9" ~ "Two or More Races Alone",
      #   .default = "Unknown"
      # ),
      # # Householder Latine Groupings
      # HHLDRHISP_Group = case_when(
      #   HHLDRHISP == "01" ~ "Non-Hispanic",
      #   HHLDRHISP != "01" ~ "Hispanic",
      #   .default = "Unknown"
      # ),
      # # Create a flag for whether a person in a household is a roommate or housemate
      # roommate_flag = case_when(
      #   RELSHIPP == 34 ~ 1,
      #   RELSHIPP != 34 ~ 0,
      #   .default = 0
      # ),
      # Replace any NAs (for households that own and so dont have a GRPIP and vice versa)
      GRPIP = replace_na(GRPIP, 0),
      OCPIP = replace_na(OCPIP, 0),
      # Define a "cost burdened" indicator.
      # (1) A household paying more than 30% of its income in housing costs (GRPIP or OCPIP) is considered "cost burdened"
      # (0) A household paying less than or equal to 30% of its income in housing costs is NOT considered "cost burdened"
      CostBurdened = case_when(
        GRPIP > 30 | OCPIP > 30 ~ "1",
        GRPIP <= 30 | OCPIP <= 30 ~ "0",
        .default = "0"
      )
    ) |>
    # Creating household flags for person variables.
    group_by(SERIALNO) |> 
    mutate(
      # Create a variable which is the sum of the number of children in a household
      Child_Total = sum(child),
      # Sum the total number of workers in the household
      Worker_Total = sum(worker),
      # Create a variable which is the sum of the number of non-relatives in the household
      # Non.Relative_Total = sum(RELSHIPP_Group),
      # # Create a variable that is the sum of roommates in a household
      # Roommate_Total = sum(roommate_flag),
      # # Create a column which denotes the presence of subfamilies for a household.
      SFN_Total = sum(SFN),
    ) |> 
    ungroup() |> 
    mutate(
      # Create a variable that indicates whether or not a household has children. 
      child_flag = case_when(
        Child_Total > 0 ~ "1",
        Child_Total == 0 ~ "0",
        .default = "0"
      ),
      # Create a variable that indicates whether or not a household has roommates.
      # roommate_flag = case_when(
      #   Roommate_Total > 0 ~ "1",
      #   Roommate_Total == 0 ~ "0",
      #   .default = "0"
      # ),
      # Create a household flag for whether or not the household contains a subfamily.
      SFN.flag = if_else(SFN_Total > 0, 1, 0),
      # Create a household flag for presence of a non-relative in the household.
      # NONRELATIVE.flag = if_else(Non.Relative_Total > 0, 1, 0),
      # Categorizing worker categories:
      # (0) no workers
      # (1) one worker
      # (2) two workers
      # (3) three or more workers
      WRKHH = case_when(
        Worker_Total == 0 ~ 0,
        Worker_Total == 1 ~ 1,
        Worker_Total == 2 ~ 2,
        Worker_Total >= 3 ~ 3
      ),
      # Determine if person is not hhder vs hhder of:
      # (0) No Workers
      # (1) One Worker
      # (2) Two Workers
      # (3) Three or more workers
      HHder = case_when(
        SPORDER == 1 & WRKHH == 0 ~ 0,
        SPORDER == 1 & WRKHH == 1 ~ 1,
        SPORDER == 1 & WRKHH == 2 ~ 2,
        SPORDER == 1 & WRKHH == 3 ~ 3,
        SPORDER > 1 ~ 99
      ), 
      # Determine what type of household the person is heading or not heading:
      # (1) Householder of Household with Children
      # (2) Non-Householder in a Household with Children 
      # (3) Householder of a Household with Multiple Adults and no children 
      # (4) Non-Householder in a Household with no children
      # (5) Householder of a Single Person Household
      HHtype = case_when(
        Child_Total >= 1 & NP > 1 & HHder != 99 ~ 1,
        Child_Total >= 1 & NP > 1 & HHder == 99 ~ 2,
        Child_Total == 0 & NP > 1 & HHder != 99 ~ 3,
        Child_Total == 0 & NP > 1 & HHder == 99 ~ 4,
        NP == 1 & HHder != 99 ~ 5
      )
    )
  return(pums_cleaned)
  
}
