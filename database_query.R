#Setting variables names for database connection. Information held remotely.
source(file = "C:/Project_Work/Local_Data/General/census.data_connection.R")

#Connecting to the database to pull various datasets relevant to the context communities
#analysis
#Set the driver
drv = dbDriver("PostgreSQL")
#Prompt database connection to rental-listings-aggregator
db_connection <- dbConnect(drv, host = host, port = port, dbname = dbname, user = user, password = password)

#Create a function for recurring database requests
db_call <- function(db.table_name, year){
  #
  if(db.table_name == "hous_submarkets_ct"){
    
    #Retrieve data from the database  
    db_data <- dbGetQuery(db_connection, paste0("SELECT * FROM tabular.", db.table_name)) #Loads full dataset
    
  }
  #
  else if (db.table_name == "educ_enrollment_by_year_districts"){
    
    #Retrieve data from the database  
    db_data <- dbGetQuery(db_connection, paste0("SELECT * FROM tabular.",db.table_name," WHERE schoolyear = '",year,"'"))
  }
  #
  else if (db.table_name == "hous_shi_m"){
    
    #Retrieve data from the database  
    db_data <- dbGetQuery(db_connection, paste0("SELECT * FROM tabular.",db.table_name," WHERE hu_year = '",year,"'"))
  }
  #
  else if (db.table_name == "hous_res_sales_by_type_value_m"){
    
    #Retrieve data from the database  
    db_data <- dbGetQuery(db_connection, paste0("SELECT * FROM tabular.",db.table_name," WHERE sale_year = '",year,"'"))
    
  }
  #
  else if (db.table_name == "hous_hh_income_by_cb_chas_m"){
    
    #Retrieve data from the database  
    db_data <- dbGetQuery(db_connection, paste0("SELECT * FROM tabular.",db.table_name," WHERE acs_year = '",year,"'"))
    
  }
}