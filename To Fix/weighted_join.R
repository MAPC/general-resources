weighted_join<-function(target_layer, target_id, sum_layer, sum_data, join_field){
  
  # ensure the inputs have the same crs
  target_layer<-st_transform(target_layer, crs = st_crs(sum_layer))
  
  # ensure the target layer has one polygon for unique target id
  target_layer<- target_layer|>
    group_by({{target_id}})|>
    summarize()
  
  #create a field for the area of the geography that will be summed to a larger geography 
  sum_layer$area_m2 <- as.numeric(st_area(sum_layer))
  
  # join the census data to the published geography
  data<-sum_layer|>
    right_join(sum_data, by = join_field)|>
    #show the estimates as values per m2
    mutate(across(c(where(is.numeric), -area_m2), function(x){x/area_m2}))
  
  # hh test
  print(sum(data$area_m2))
  
  
  
  # intersect the enriched published geometry with the target layer
  intersect_stage<-st_intersection(data, target_layer)
  
  
  #calculate the area of the target geography 
  intersect_stage$int_area<- as.numeric(st_area(intersect_stage))
  
  # show the estimates per m2 inflated to the area of the intersected geometry
  intersect_stage<-intersect_stage|>
    mutate(across(c(where(is.numeric), -int_area), function(x){x * int_area}))
  
  print(sum(intersect_stage$int_area, na.rm = T))
  
  # consolidate the intersected geometry to the target layer geometry
  target_layer_sum<-intersect_stage |>
    group_by({{target_id}})|>
    summarize(across(where(is.numeric), ~sum(., na.rm = T)))
  
  return(target_layer_sum)                
}
