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
 
mapc_pums_cleaning<-function(vintage_year, addl_vars = c()){  
  
  # Set PUMS vintage final year for {tidycensus} API query.
  vintage <- vintage_year
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
      # # Person Race Groupings. 
       RACE_Group = case_when(
         RAC1P == "1" ~ "White Alone",
         RAC1P == "2" ~ "Black or African American Alone",
         RAC1P %in% c("3", "4", "5") ~ "AIAN Alone",
         RAC1P == "6" ~ "Asian Alone",
         RAC1P == "7" ~ "NHPI Alone",
         RAC1P == "8" ~ "SOR Alone",
         RAC1P == "9" ~ "Two or More Races Alone",
         .default = "Unknown"
       ),
      # # Person Latine Groupings
       HISP_Group = case_when(
         HISP == "01" ~ "Non-Hispanic",
         HISP != "01" ~ "Hispanic",
         .default = "Unknown"
       ),
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
    ) 
  
  return(pums_cleaned)
  
}

#' PUMS Margin of Error (MOE) Calculator
#'
#' @returns
#' @export
#'
#' @examples
pums_moe <- function(df, weight, repweight_list, grp_var, totals_cols, median_cols){
  
  tmp_srvy.obj <- as_survey_rep(
    df,
    weight = {{weight}},
    repweights = matches(repweight_list),
    type = "ACS",
    mse = TRUE
  )
  
  srvy.tbl <- tmp_srvy.obj |> 
    group_by({{grp_var}}) |> 
    summarize(
      across(.cols = totals_cols, ~survey_total),
      across(.cols = median_cols, ~survey_median),
    )

  return()  
  
}
