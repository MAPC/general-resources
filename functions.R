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
    st_join(select(polygons, polygon_field, geometry)) 
  return(tmp)
}
