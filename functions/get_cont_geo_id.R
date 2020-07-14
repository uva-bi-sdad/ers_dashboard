get_cont_geo_id <- function(map_level_choice, map_county_choice) {
  print('get_cont_geo_id inputs')
  print(map_level_choice)
  print(map_county_choice)
  if (map_level_choice == "County") {
    cont_geo_id = "51"
  } else if (map_level_choice == "Census Block Group") {
    cont_geo_id = map_county_choice
  } else {
    cont_geo_id = "51"
  }
  print(sprintf('get_cont_geo_id returns: %s', cont_geo_id))
  return(cont_geo_id)
}
