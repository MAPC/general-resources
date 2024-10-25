## Milford CE Ethnicity Data Request

source("libraries.r")
       
xw<-mapcdatakeys::census_muni_keys%>%select(muni_id, muni_name, cosub_5y21)

acs22<- load_variables(2022, "acs5", cache = FALSE)%>%
  separate(label, into = paste0("label", 1:9), sep = "!!", fill = "right", remove = FALSE)

#Make sure that options(tigris_use_cache = TRUE) is called in library
#Change county and geography and most recent year for ACS
acs21_geom<- get_acs(state = "MA", county = "Suffolk", geography = "tract",
                     variable = "B01001A_001", geometry = TRUE, year = 2021)%>%
  #remove everything but the GEOID and geometry
  subset(select = -c(variable, estimate, moe))%>%
  #renames GEOID to work with DataCommon Datasets
  rename(ct20_id = GEOID)%>%
  #Spatial Transformation
  st_transform(mass_mainland, crs = set_crs())
    
    # For Non-Spatial Data
    # Finding Relevant Tables and Available Geography: 
    
v21 <- load_variables(2021, "acs5", cache = FALSE)%>%
      #Formats table to be more legible
   separate(label, into = paste0("label", 1:9), sep = "!!", fill = "right", remove = FALSE)
    
    
    # All Table Codes
hisp_vars<- c("B03001_001", "B03001_002", "B03001_003", "B03001_004", "B03001_005",
         "B03001_006", "B03001_007", "B03001_008", "B03001_009", "B03001_010",
         "B03001_011", "B03001_012", "B03001_013", "B03001_014", "B03001_015",
         "B03001_016", "B03001_017", "B03001_018", "B03001_019", "B03001_020",
         "B03001_021", "B03001_022", "B03001_023", "B03001_024", "B03001_025",
         "B03001_026")
        #Language
lang_vars<- c( "B16001_001", "B16001_003", "B16001_006", "B16001_009", "B16001_012",
  "B16001_015", "B16001_018", "B16001_021", "B16001_024", "B16001_027",
  "B16001_030", "B16001_033", "B16001_036", "B16001_039", "B16001_042",
  "B16001_045", "B16001_048", "B16001_051", "B16001_054", "B16001_057",
  "B16001_060", "B16001_063", "B16001_066", "B16001_069", "B16001_072",
  "B16001_075", "B16001_078", "B16001_081", "B16001_084", "B16001_087",
  "B16001_090", "B16001_093", "B16001_096", "B16001_099", "B16001_102",
  "B16001_105","B16001_108")
        #Asian Origin
asia_vars<- c("B02015_001", "B02015_002", "B02015_003", "B02015_004","B02015_005",
  "B02015_006", "B02015_007", "B02015_008", "B02015_009", "B02015_010",
  "B02015_011", "B02015_012", "B02015_013", "B02015_014","B02015_015",
  "B02015_016", "B02015_017", "B02015_018", "B02015_019", "B02015_020",
  "B02015_021", "B02015_022", "B02015_023", "B02015_024","B02015_025",
  "B02015_026", "B02015_027", "B02015_028", "B02015_029", "B02015_030")
    
hisp_data<- get_acs(state = "MA", geography = "county subdivision", 
                   variables = hisp_vars, 
                   year = 2022)%>%
      left_join(., acs22, by = c("variable" = "name"))%>%
      select(GEOID, label, estimate, moe)%>%
          #(if you don't want the MOE)
          #subset(select = -moe)%>% 
      pivot_wider(names_from = "label",
                  values_from = c("estimate", "moe"))%>%
      rowwise()%>%
      rename_all(~stringr::str_replace_all(.,"Estimate!!", ""))%>%
      rename_all(~stringr::str_replace_all(., "Total:!!Hispanic or Latino:!!",""))%>%
      rename_all(~stringr::str_replace_all(., "Central American:!!", ""))%>%
      rename_all(~stringr::str_replace_all(., "South American:!!", ""))%>%
      rename_all(~stringr::str_replace_all(., ":", ""))%>%
      rename_all(~stringr::str_replace_all(., "!!", "_"))%>%
      rename_all(~stringr::str_replace_all(., " ", "_"))%>%
  left_join(., xw, by = c("GEOID" = "cosub_5y21"))%>%
  filter(muni_name == "Milford")
  
      #   #mutate([whatever you want to do])%>%
        
        
asia_data<- get_acs(state = "MA", geography = "county subdivision", 
                    variables = asia_vars, 
                    year = 2022)%>%
  left_join(., acs22, by = c("variable" = "name"))%>%
  select(GEOID, label, estimate, moe)%>%
  #(if you don't want the MOE)
  #subset(select = -moe)%>% 
  pivot_wider(names_from = "label",
              values_from = c("estimate", "moe"))%>%
  rowwise()%>%
  rename_all(~stringr::str_replace_all(.,"Estimate!!", ""))%>%
  rename_all(~stringr::str_replace_all(., "Total:!!",""))%>%
  rename_all(~stringr::str_replace_all(., "East Asian:!!", ""))%>%
  rename_all(~stringr::str_replace_all(., "Southeast Asian:!!", ""))%>%
  rename_all(~stringr::str_replace_all(., "South Asian:!!", ""))%>%
  rename_all(~stringr::str_replace_all(., "Central Asian:!!", ""))%>%
  rename_all(~stringr::str_replace_all(., ", except Taiwanese", ""))%>%
  rename_all(~stringr::str_replace_all(., ":", ""))%>%
  rename_all(~stringr::str_replace_all(., "!!", "_"))%>%
  left_join(., xw, by = c("GEOID" = "cosub_5y21"))%>%
  filter(muni_name == "Milford")



lang_data<- get_acs(state = "MA", geography = "county subdivision", 
                    table = "C16001", 
                    year = 2022)%>%
  left_join(., acs22, by = c("variable" = "name"))%>%
  select(GEOID, label, estimate, moe)%>%
  #(if you don't want the MOE)
  #subset(select = -moe)%>% 
  pivot_wider(names_from = "label",
              values_from = c("estimate", "moe"))%>%
  rowwise()%>%
  rename_all(~stringr::str_replace_all(.,"Estimate!!", ""))%>%
  rename_all(~stringr::str_replace_all(., ":", ""))%>%
  rename_all(~stringr::str_replace_all(., "!!", "_"))%>%
  rename_all(~stringr::str_replace_all(., " ", "_"))%>%
  rename_all(~stringr::str_replace_all(., "Total_", ""))%>%
  left_join(., xw, by = c("GEOID" = "cosub_5y21"))%>%
  filter(muni_name == "Milford")
  


require(openxlsx)
output_list <- list("Hispanic Origin" = hisp_data, "Asian Origin" = asia_data,
                 "Language and Ability" = lang_data)

#write.xlsx(output_list, "Milford_CE_req.xlsx")
        
