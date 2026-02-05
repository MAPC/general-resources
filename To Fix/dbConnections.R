#Setting variables names for database connection. Information held remotely.
#source(file = "C:/Project_Work/Local_Data/General/census.data_connection.R")

#Connecting to the database to pull various datasets 
#Set the driver
drv = dbDriver("PostgreSQL")
#Prompt database connection to rental-listings-aggregator
db_connection <- dbConnect(drv, host = Sys.getenv("host"), port = Sys.getenv("port"), dbname = Sys.getenv("dbname"), 
                           user = Sys.getenv("user"), password = Sys.getenv("password"))

# Figure out how to program the connection pieces
# dbDisconnect(db_connection)
#Database Request Function 

#' Query MAPC Database
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
get_mapc_database <- function(db.table_name, year = NULL, vars, geography_column = NULL,
                              schema = c("tabular.", "metadata.")){
  # vars = * for whole table
  #db.table name 
  #year = year range desired, leave blank if no year
  
  #Pulls the year variable for the requested table
  year_var<- dbGetQuery(db_connection, paste0("SELECT yearcolumn 
                                              FROM mapc._data_browser 
                                              WHERE table_name = '",db.table_name,"'"))
  
  if (vars == "*" && missing(year)){ #All Columns all years
    
    db_query<-paste0("SELECT ", vars, 
                     "FROM ",schema, db.table_name)
    
  } else if (vars == "*" && !missing(year)){ #All Columns, specific year
    db_query<-paste0("SELECT ", vars, 
                     "FROM ", schema,db.table_name,
                     " WHERE ",year_var$yearcolumn[1]," = '",year,"'")
  } else if (is.na(year)){ #Certain columns, all years
    db_query<-paste0("SELECT ", geography_column, ", ", vars, 
                     "FROM ", schema, db.table_name)
  }else { #Certain Columns Certain years
    db_query<- paste0("SELECT ", geography_column,", ", year_var$yearcolumn[1],", ", vars, 
                      " FROM ", schema, db.table_name,
                      " WHERE ",year_var$yearcolumn[1]," = '",year,"'")
  }
  
  
  
  
  #Queries the database
  db_data <- dbGetQuery(db_connection, db_query)  
  
}

## HNA Database Query Function

#' Query MAPC Database
#'
#' @param dr_table data frame matching the thing
#' @param study_area the municipality or geography all tables will have
#' @param context a list, municipalities or geopgraphies all tables checked will have 
#'
#' @return a list of tables
#' @export
#'
#' @examples 


#### Large Database Query
get_large_database_query<-function(dr_table, study_area, context){
  
  clean_table<-NULL
  
  clean_table[['Table of Contents']]<-dr_table
  
  dr_table<-data.table(dr_table)%>%
    filter(site == "Data Common")%>%
    separate_rows(years, sep = ",")%>%
    mutate(years = str_trim(years, side = "left"))
  
  # Input Test
  # Field Names Required
  # data_common_table
  # cc
  # Data
  # year
  
  school_xwalk<-read_xlsx("K:/DataServices/Datasets/Data Keys/schools/towns_districts.xlsx")%>%
    mutate(municipal = str_to_title(town_name))%>%
    select(distcode8, municipal)
  
  for (dc_table in unique(dr_table$data_common_table)){
    
    #object for when years need to loop
    year_table<-NULL
    
    #make a list of each year requested for that table
    year_list <- dr_table%>%filter(data_common_table == dc_table)
    
    
    #create the name for the worksheet
    #[1] in case the same table was requested for multiple things, cut to the max length for an excel sheet
    data_name<- str_trunc(unique(year_list$data)[1], 30)
    
    #then for each year in that list of years
    for(tab_year in year_list$years){
      
      print(paste(dc_table, tab_year))
      
      # Special table types and then everyone else
      
      if(dc_table == "census2020_pl94_hu_occ_m") { 
        year <-NULL
        table<- get_mapc_database(db.table_name = dc_table,
                                  vars = "*",
                                  schema = "tabular.",
                                  geography_column = "municipal")
        
        print(paste(dc_table, "Loaded Special"))
      }else if(dc_table == "educ_enrollment_by_year_districts"){
        table<- get_mapc_database(db.table_name = dc_table,
                                  year = tab_year,
                                  vars = "*",
                                  schema = "tabular.",
                                  geography_column = "municipal")%>%
          full_join(., school_xwalk, by = c("districtid" = "distcode8"))
        print(paste(dc_table, "Loaded and Crosswalked"))
        print(head(table))
      }else{
        (print("regular table load"))
        table<- get_mapc_database(db.table_name = dc_table,
                                  year = tab_year,
                                  vars = "*",
                                  schema = "tabular.",
                                  geography_column = "municipal")
        print(paste(dc_table, "Loaded"))
      }
      
      #Filter by Main Community or Context Communities
      if (is.na(year_list$cc[1])){
        table<-table%>%
          filter(municipal %in% study_area)
      } 
      else {
        table<-table%>%
          filter(municipal %in% c(study_area, context))
      }
      
      year_table[[tab_year]]<-table
      
    }
    
    
    #rbind all of the tables
    table_raw<-rbindlist(year_table)
    
    #load the metadata for the table
    metadata<-get_mapc_database(db.table_name = dc_table,
                                vars = "*",
                                schema = "metadata.",
                                geography_column = "municipal")%>%
      #filter the meta data to what fields are in the table  
      filter(name %in% names(table_raw))%>%
      mutate(alias = ifelse(name == "municipal", "Municipality", alias))
    
    #make sure the table only has the columns in the metadata
    table_raw<-table_raw%>%select(metadata$name)
    
    print("Metadata Loaded")
    
    #if the metadata glitches somehow
    if (length(names(table_raw)) != length(unique(metadata$alias))){
      #skip adding clean names
      clean_table[[data_name]]<- table_raw
      
      print(paste(dc_table, "MetaData Issue"))
    }
    else{
      
      #rename the table with the alias from the metadata
      clean_table[[data_name]]<-table_raw%>%
        rename_at(vars(metadata$name), ~ metadata$alias)
    }
    
    print(paste(dc_table, "Clean Names Complete"))
  }
  
  return(clean_table)
  
}

get_region_list<-function(region){
  
  muni_list<-mapcdatakeys::all_muni_data_keys%>%
    select(muni_name, subrg_acr, rpa_acr)%>%
    filter(subrg_acr == region | rpa_acr == region)
  
  return(list(muni_list$muni_name))
  
}


