# Create Map

get_name_text <- function(name) {
  stringr::str_replace_all(name, ',', '<br/>')
}

get_stat_text <- function(stat_value, stat) {
  stat_text <- sprintf('%s: %s', stat, stat_value)
  return(stat_text)
}

map_label <- function(map_data, map_poly_data_choice_params, stat='count'){

  stat_value <- map_data@data[, eval(map_poly_data_choice_params[3])]
  stat_text <- get_stat_text(stat_value, map_poly_data_choice_params[12])

  name_value <- map_data@data[, c("NAME")]
  name_text <- get_name_text(name_value)

  # print(name_text)

  html_text <- sprintf(
    "
  <div style='word-wrap:break-word;'>
    <strong>%s</strong>
    <br/>
    %s
  </div>
  ",
    stat_text,
    name_text
  )
  return(html_text)
}

createBaseMap <- function(map_data, map_poly_data_choice_params) {
  #map_data <- map_values$data
  map_data@data[, eval(map_poly_data_choice_params[3])] <- as.numeric(map_data@data[, eval(map_poly_data_choice_params[3])])

  labels <- map_label(map_data, map_poly_data_choice_params) %>%
    lapply(htmltools::HTML)

  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = map_data@data[, eval(map_poly_data_choice_params[3])])

  legend_title <- substr(map_poly_data_choice_params[10], regexpr("olumn: ",  map_poly_data_choice_params[10]) + 7, nchar(map_poly_data_choice_params[10]))
  legend_title <- paste(strwrap(legend_title,16), collapse="<br />") %>%
    htmltools::HTML()

  output$mymap <- renderLeaflet({
    leaflet(map_data) %>%
      # setView(
      #         lng = map_poly_data_choice_params[6],
      #         lat = map_poly_data_choice_params[5],
      #         zoom = map_poly_data_choice_params[7]
      #       ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 1,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(get(map_poly_data_choice_params[3])),
                  #fillColor = ~colorQuantile("YlOrRd", get(map_poly_data_choice_params[3]), n=2)(get(map_poly_data_choice_params[3])),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      sendToBack = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend("topleft", pal = pal, values = ~get(map_poly_data_choice_params[3]),
                title = legend_title,
                opacity = 1
      )
  })
}

# createProxyMap <- function(legendTitle, mapTitle, palette, spdf, column, layerid_col, label_col) {
#   output$mapTitle <- renderText(mapTitle)
#   leafletProxy('mymap', data = spdf) %>%
#     addPolygons(stroke=TRUE,
#                 weight=2,
#                 smoothFactor = 0.2,
#                 fillOpacity = .75,
#                 color= ~palette(column),
#                 group=mapTitle,
#                 layerId=as.vector(paste0(layerid_col, "a")),
#                 label=paste0(label_col, ": ", round(as.numeric(as.character(column, 2))), if (values$percentage){"%"} else {""}),
#                 labelOptions= labelOptions(direction = 'auto'),
#                 highlightOptions = highlightOptions(
#                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
#                   bringToFront = FALSE, sendToBack = FALSE)
#     ) %>%
#     clearControls() %>%
#     addLegend(pal = palette,
#               values = ~column,
#               opacity = 1,
#               position = "topleft",
#               title = legendTitle
#     )
# }

createProxyMapPoints <- function(pointData, pointNameCol, pointAddr, pointCity, pointColor) {
  print("PXMP DAT")
  ptdat <- get(pointData)
  # print(head(ptdat))
  # print(names(ptdat))
  # print(class(ptdat))

  labels <- paste(ptdat@data[, pointNameCol],
                  ptdat@data[, pointAddr],
                  ptdat@data[, pointCity],
                  sep = '<br/>') %>%
    lapply(htmltools::HTML)

  print("LEAFLET PROXY")
  leafletProxy('mymap', data = ptdat) %>%
    # clearShapes() %>%
    addCircleMarkers(radius = 5,
                     color = pointColor,
                     stroke = FALSE,
                     fillOpacity = 0.5,
                     #layerId=as.vector(paste0(get(pointData)[,pointAddr], pointColor)),
                     label = labels
                     #clusterOptions = markerClusterOptions()
    ) %>%
    highlightOptions(bringToFront = TRUE)
}

# add leading zeros to fips codes
padFIPS <- function(fips_code = "1", level = "State") {
  if (toupper(level) == "STATE")
    n = 2
  else if (toupper(level) == "COUNTY")
    n = 3
  else if (toupper(level) == "LEA")
    n = 5
  else if (toupper(level) == "TRACT")
    n = 6
  stringr::str_pad(fips_code, n, "0", side = "left")
}

# combine state and sub-state fips codes
stateSubFIPS <- function(state_fips, sub_fips, sub_fips_type) {
  paste0(padFIPS(state_fips, "State"), padFIPS(sub_fips, level = sub_fips_type))
}

# block group FIPS
blockGroupFIPS <- function(state_fips, county_fips, tract_fips, block_group_fips) {
  paste0(padFIPS(state_fips, "State"), padFIPS(county_fips, "County"), padFIPS(tract_fips, "Tract"), block_group_fips)
}
