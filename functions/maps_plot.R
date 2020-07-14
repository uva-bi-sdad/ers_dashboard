get_map_plot_name_text <- function(name) {
  stringr::str_replace_all(name, ',', '\n')
}

create_label <- function(map_values, map_poly_data_choice_params, col_name){
  sprintf(
"%s: %s\n%s",
    map_poly_data_choice_params[12],
    as.numeric(map_values$data@data[, col_name]),
    get_map_plot_name_text(as.character(map_values$data@data$NAME))
  )
}

createMapBarplot <- function(map_values, map_poly_data_choice_params, col_name) {
  print('Creating map bar plot')
  # print('map_values')
  # print(map_values)

  # print('map_poly_data_choice_params:')
  # print(map_poly_data_choice_params)
  #
  # print('col_name:')
  # print(col_name)

  label <- create_label(map_values, map_poly_data_choice_params, col_name)
  #print('label:')
  #print(label)

  output$map_hist_plotly <- renderPlotly({
    plot_ly(
      x = as.character(map_values$data@data$NAME),
      y = as.numeric(map_values$data@data[, col_name]),
      name = "NAME",
      type = "bar",
      text = label
    ) %>%
      layout(
        # xaxis = list(
        #   dtick = 1
        # ),
        yaxis = list(
          fixedrange = TRUE
        ),
        margin = list(
          b = 100
        )
      )
  })
}
