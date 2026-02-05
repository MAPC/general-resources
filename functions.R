#### functions ####

#Functions (By Alexa)

# function to find the coordinates of any records missing coordinate data 
# add lat/long == 0 to first tmp
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

# add an if else so that if there is missing lat long it doesn't geocode or a y/n do you want to run anyways?
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


# can you add multiple fields - we think yes
geolocate <- function(points_sf, polygons, polygon_field) {
  #mass_mainland<-"+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  tmp <- points_sf |> 
    #setting crs to match polygon shapefile
    st_transform(mass_mainland, crs = st_crs(polygons)) |> 
    #polygon_field can be one field you want to add or a list of fields
    st_join(select(polygons, polygon_field, geometry)) 
  return(tmp)
}


