# Script to create shapefiles from census tables

## this is not finished!!!

library(tidyverse)
library(mapcdatakeys)
library(httr2)
library(sf)
library(jsonlite)

# function to make a shapefile from a DataCommon table

#' Query DataCommon Database
#'
#' @param db.table_name as found after tabular.
#' @param year year range 
#' @param vars list of variables from table or * for the whole table
#' @param geography_column municipal or ct20_id
#' @param schema tabular. or metadata.
#'
#' @return from mapc database
#' @export
#'
#' @examples 

https://staging.datacommon-react.mapc.org/api/export?
  token=datacommon&database=ds&
  schema=tabular&
  table=b03002_race_ethnicity_acs_ct&format=csv&years=2020-24&useMetadataColumns=false

dc_acs_pull <- function(table_name, format=NULL, years, metadata=NULL, geography){
  # pull shapefile for geography from input
  if(geography == 'muni') {
    shp <- muni_sf(2020) |> 
      select(muni_name, geometry)
  } else if (geography == 'ct') {
    shp <- tract_sf(2020) |> 
      select(ct20_id, muni_name, geometry) |> 
      mutate(ct20_id = as.character(ct20_id))
  } else if(geography == 'bg'){
    shp <- block_sf(2020)
  }
  
  #define variables that won't change
  token = 'datacommon'
  database = 'ds'
  schema = 'tabular'
  
  #define default variables
  if (missing(format)) {format = 'csv'}
  if (missing(metadata)) {metadata = 'false'}
  
  #build api path
  base_url = "https://staging.datacommon-react.mapc.org/api/export?"
  endpoint_url = paste0(
              "token=", token,
              "&database=",database, 
              "&schema=", schema,
              "&table=",table_name,
              "&format=",format,
              "&years=",year,
              "&useMetadataColumns=",metadata
               ) 
  
  #get table
  table = read_csv(paste0(base_url, endpoint_url)) |> 
    mutate(ct20_id = as.character(ct20_id))
  
  #join table and shapefile
  table_shp <- left_join(table, shp) 
  return(table_shp)
}

#Race
race <- dc_acs_pull('b03002_race_ethnicity_acs_ct', '2020-24', geography = 'ct', format='csv', metadata = 'false')
test <- read.csv("https://datacommon.mapc.org/api?token=datacommon&database=ds&schema=tabular&table=b19301_per_capita_income_acs_m")
test<- 

# what works for now
race <- read_csv("https://staging.datacommon-react.mapc.org/api/export?token=datacommon&database=ds&schema=tabular&table=b03002_race_ethnicity_acs_ct&format=csv&useMetadataColumns=false")

age <- read_csv("https://staging.datacommon-react.mapc.org/api/export?token=datacommon&database=ds&schema=tabular&table=b15001_educational_attainment_by_age_acs_ct&format=csv&years=2020-24&useMetadataColumns=false")

data <- read_csv("https://datacommon.mapc.org/api/export?token=datacommon&database=ds&schema=tabular&table=b17001_poverty_by_population_acs_ct&format=csv&years=2020-24&useMetadataColumns=false")

race_clean <- race |> 
  filter(acs_year == "2020-24") |> 
  select(ct20_id, acs_year, totpop, totpop_me, nhwhi, nhwhi_me, nhwhi_p, nhwhi_mep) |> 
  mutate(poc = totpop-nhwhi,
         poc_p = poc/totpop)

age_clean <- age |> 
  filter(acs_year == "2020-24") |> 
  select(ct20_id, acs_year, pop18o, pop18o_me, pop65o, pop65ome, pop65o_p, pop65omep)

data_clean <- data |> 
  select(-c(seq_id, ct10_id, geoid, logrecno)) 

ct_shp <- tract_sf(2020) |> 
  select(ct20_id, muni_name, geometry) |> 
  mutate(ct20_id = as.double(ct20_id))

table_shp <- left_join(ct_shp, data_clean, by = "ct20_id")

st_write(table_shp, "K:/DataServices/Projects/Current_Projects/PublicHealth/Food_Systems/Online_map/spatial/pov_p.shp")
