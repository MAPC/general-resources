#### functions ####

#Functions (By Alexa)

make_spatial <- function(points, lat, long) {
  print(paste0("There are ", nrow(filter(points, !complete.cases(lat, long))), " rows with incomplete coordinates"))
  #lat_lon_CRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  return(points_shp <- points |> 
    rename( lat = lat,
            long = long)|>
    #select(unique_id, lat, long) |> 
    st_as_sf(coords = c('long', 'lat'),
             crs = st_crs(lat_lon_CRS),
             remove = FALSE)) 
}

geolocate <- function(points_sf, polygons, polygon_field) {
  #mass_mainland<-"+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  tmp <- points_sf |> 
    #setting crs to match polygon shapefile
    st_transform(mass_mainland, crs = st_crs(polygons)) |> 
    #polygon_field can be one field you want to add or a list of fields
    st_join(select(polygons, polygon_field, geometry)) 
  return(tmp)
}

# function to find the coordinates of any records missing coordinate data
geocoder <- function(df) {
  tmp1 <- df |> 
    filter(is.na(latitude)) |> 
    geocode(geo_addr, method = 'arcgis', lat = lat, long = long)
  tmp2 <- df |> 
    filter(!is.na(latitude)) |> 
    mutate(
      lat = latitude,
      long = longitude
    )
  geocoded <- bind_rows(tmp1, tmp2) |> 
    select(-c(latitude, longitude))
}

# Function to load spatial parcel data and join to pdb on K
get_full_parcel_data<-function (muni_name){
  # Because the Boston data is somewhere else
  if (toupper(muni_name) == "BOSTON"){
    par_geom<- arc_read("https://gisportal.boston.gov/arcgis/rest/services/Assessing/ASG_PROPERTY_ASSESSMENT_PARCEL_JOIN_FY24/FeatureServer/0")
  } else{
  #ID the town to download
  muni_load<-toupper(muni_name)
  #Load in the MassGIS Excel Sheet of Links
  parcel_links<- rio::import("https://www.mass.gov/doc/massgis-parcel-data-download-links-table/download")%>%
    mutate(tax_par = paste0(str_sub(`File GDB Download URL`, -20, -17), "TaxPar"), #Name of the TaxPar feature class
           gdb_file = paste0(str_sub(`File GDB Download URL`, -20, -9)), #Name of the GDB folder name when unzipped
           shp_file = str_sub(`Shapefile Download URL`, 63, -9))
  
  #Filter the Excel Sheet of Links to the muni we are loading
  download_inputs <- parcel_links%>%
    filter(`Town Name` == muni_load)
  
  # Function to download, unzip, and read a gdb
  read_gdb_url <- function(URL, gdb_file, tax_par){
    
    #create a tempfile to dowloand
    dl_tempfile <- tempfile()
    #download to a temp file
    download.file(url = URL, destfile = dl_tempfile)
    # specificy a real folder to unzip to
    out_directory <- getwd()
    #unzip to a real folder
    unzip(dl_tempfile, exdir = out_directory)
    #check that the gdb is in the right place
    real_folder<- paste0(out_directory, "/", list.files(out_directory, pattern = gdb_file))
    print(real_folder)
    # open the thing
    par_sf<-arc.data2sf(arc.select(arc.open(paste0(real_folder, "/", tax_par))))
    
    #delete the real folder
    unlink(real_folder)
    
    return(par_sf)
    }
  
  #read in the thing as arc.open
  par_geom<-read_gdb_url(URL = download_inputs$`File GDB Download URL`, 
                         gdb_file = download_inputs$gdb_file,
                         tax_par = download_inputs$tax_par)
  }

  # Joining to the pdb
  pdb_lookup<- str_to_title(muni_load)

  print(paste(muni_load, pdb_lookup))

  pdb<- read_csv(paste0("K:/DataServices/Datasets/Parcel_DB/Data/LPDB_Municipal_Data/current/LPDB_DRAFT_", pdb_lookup,
                        "_12.18.23.csv"))

  full_parcels<- full_join(select(par_geom, LOC_ID, geom), pdb, by = "LOC_ID")

  return(full_parcels)
}
