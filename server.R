# server.R
library(readr)

options(shiny.reactlog = TRUE)

# LOAD SPDFs -----------------------------------------

# us_states_spdf <- readRDS("data/CENSUS/us_states_spdf.RDS")
va_counties_spdf <- readRDS("data/CENSUS/va_counties_simplified_spdf.RDS")
va_blockgroups_spdf <- readRDS("data/CENSUS/va_blockgroups_simplified_spdf.RDS")
va_grocery_stores_spdf <- readRDS("data/GMAPS/va_grocery_stores_spdf.RDS")
va_places_of_worship_spdf <- readRDS("data/GMAPS/va_places_of_worship_spdf.RDS")
va_hospitals_spdf <- readRDS("data/ARCGIS/va_hospitals_spdf.RDS")
va_schools_spdf <- readRDS("data/ARCGIS/va_schools_spdf.RDS")
va_mobile_home_parks_spdf <- readRDS("data/ARCGIS/va_mobile_home_parks_spdf.RDS")
va_snap_retailers_spdf <- readRDS("data/USDA/snap_retail_spdf.RDS")
va_libraries_spdf <- readRDS("data/GMAPS/va_libraries_spdf.RDS")
# va_fast_food_spdf <- readRDS("data/GMAPS/fast_food_by_area_Fairfax_County_and_City_va_spdf.RDS")

rwjfs <- readRDS('data/timeseries_temp/rwjf_ts_data.RDS')
rwjfs$year_dt <- sprintf('%s-01-01', rwjfs$year)
rwjfs$year_dt <- as.Date(rwjfs$year_dt)
va_rwjfs_ts <- rwjfs[tolower(rwjfs$GEO_IDS__STATE) == 'virginia', ]


# RUN SERVER
shinyServer(function(input, output, session) {

  # LOAD FUNCTIONS -----------------------------------------

  source("functions/reactive.R", local = TRUE)
  source("functions/menus.R", local = TRUE)
  source("functions/dataset.R", local = TRUE)
  source("functions/maps.R", local = TRUE)
  source("functions/maps_plot.R", local = TRUE)
  source("functions/graphs.R", local = TRUE)
  source("functions/get_cont_geo_id.R", local = TRUE)

  # CREATE REACTIVE VALUES AND MENUS -----------------------------------------

  map_values <- setMapReactives(map_level_choice = "County")
  graph_values <- setGraphReactives()
  # createMenus()

  # RENDER MENU TITLES -----------------------------------------

  output$tit0 <-
    renderUI(HTML('<div class="menuTitle">Map Level</div>'))

  # map level data title
  output$tit1 <-
    renderUI(HTML(
      '<div class="menuTitle" style="margin-top:5px;">Map Level Data</div>'
    ))

  # RENDER MENU INPUTS ----------------------------------------------

  # input.map_level_choice
  output$map_level <- renderUI({
    div(HTML(createSelectionMenuMapLevel()))
  })

  # input.map_state_choice & input.map_county_choice
  output$countySelect <- renderUI({
    div(
      HTML(createSelectionMenuStates()),
      conditionalPanel(condition = "input.map_level_choice == 'Census Block Group'",
                       HTML(createSelectionMenuStateCounties()))
    )
  })

  # input.map_polygon_data_choice
  output$areaDataMenu <- renderUI({

    cont_geo_id <- get_cont_geo_id(map_values$map_level_choice, map_values$map_county_choice)
    # if (map_values$map_level_choice == "County") {
    #   cont_geo_id = "51"
    # } else if (map_values$map_level_choice == "Census Block Group") {
    #   cont_geo_id = map_values$map_county_choice
    # } else {
    #   cont_geo_id = "51"
    # }

    div(class = "areaDataDiv",
        HTML(
          createSelectionMenuData(
            containing_geo_id = cont_geo_id,
            geo_data_level = map_values$map_level_choice
          )
        ))
  })

  # location/point data selection
  output$locationDataMenu <- renderUI({

    div( class = "menuTitle",
    if (map_values$map_poly_data_choice != "") {
        HTML(selectionMenuDataPoints())
      } else {
        HTML(selectionMenuDataPointsDisabled())
      }
)
  })

  # analysis correlation  selections ----
  output$cor1 <- renderUI({
    vars <- names(va_rwjfs_ts)
    variable_choices <- vars[!vars %in% c("GEO_IDS__FIPS", "GEO_IDS__STATE", "GEO_IDS__COUNTY")]

    div(
      div(
        selectizeInput(
          "corr1year",
          label = "Year",
          choices = unique(rwjfs$year)
        )),
      div(
          selectizeInput(
            "corrVar1",
            label = "V1",
            choices = variable_choices
          )),
      div(
          selectizeInput(
            "corrVar2",
            label = "V2",
            choices = variable_choices
          ))
    )
  })

  # analysis correlation  plot ----
  output$corr1_plot <- renderPlotly({
    # checks for valid set of correlations
    # this block is code is copied from output$corr1_coeff
    corr_df <- rwjfs[rwjfs$year == input$corr1year, ]
    xvar <- as.numeric(corr_df[, input$corrVar1])
    yvar <- as.numeric(corr_df[, input$corrVar2])

    corr_calc_df <- data.frame(xvar, yvar)
    corr_calc_df <- na.omit(corr_calc_df)
    # corr_coef <- cor(na.omit(xvar), na.omit(yvar))
    corr_coef <- cor(corr_calc_df$xvar, corr_calc_df$yvar)

    #xreg <- xvar[is.na(xvar==FALSE) & is.na(yvar==FALSE)]
    #yreg <- yvar[is.na(xvar==FALSE) & is.na(yvar==FALSE)]
    cor_reg <- lm(corr_calc_df$yvar ~ corr_calc_df$xvar)
    require(broom)
    if (is.na(corr_coef)) {
      #plot_ly(x = xvar, y = yvar)
      plot_ly(x=corr_calc_df$xvar)%>%
        add_markers(y=corr_calc_df$yvar,showlegend=FALSE)%>%
        add_lines(x = corr_calc_df$xvar, y=fitted(cor_reg),showlegend=FALSE)%>%
        add_ribbons(data = augment(cor_reg),
                    ymin = ~.fitted - 1.96 * .se.fit,
                    ymax = ~.fitted + 1.96 * .se.fit,
                    line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)',
                    name = "Standard Error")
    } else {
      #plot_ly(x = xvar, y = yvar)
      plot_ly(x=corr_calc_df$xvar)%>%
        add_markers(y=corr_calc_df$yvar,showlegend=FALSE)%>%
        add_lines(x = corr_calc_df$xvar, y=fitted(cor_reg),showlegend=FALSE)%>%
        add_ribbons(data = augment(cor_reg),
                    ymin = ~.fitted - 1.96 * .se.fit,
                    ymax = ~.fitted + 1.96 * .se.fit,
                    line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)',
                    name = "Standard Error")
    }
  })

  output$analysis_corr_coef <- renderText({
    # checks for valid set of correlations
    corr_df <- rwjfs[rwjfs$year == input$corr1year, ]
    xvar <- as.numeric(corr_df[, input$corrVar1])
    yvar <- as.numeric(corr_df[, input$corrVar2])

    corr_calc_df <- data.frame(xvar, yvar)
    corr_calc_df <- na.omit(corr_calc_df)
    print(head(corr_calc_df))
    # corr_coef <- cor(na.omit(xvar), na.omit(yvar))
    corr_coef <- cor(corr_calc_df$xvar, corr_calc_df$yvar)
    print(corr_coef)

    # if the correlation is NA, then there are no values to plot
    if (is.na(corr_coef)) {
      return(sprintf('Unable to calculate correlation between selected variables. Possibly due to lack of data.'))
    } else {
      return(sprintf('Correlation coefficient: %0.4f', corr_coef))
    }
  })


  # timeseries selections ----
  output$ts1 <- renderUI({
    vars <- names(va_rwjfs_ts)
    variable_choices <- vars[!vars %in% c("GEO_IDS__FIPS", "GEO_IDS__STATE", "GEO_IDS__COUNTY")]
    cty_choices <- unique(c(va_rwjfs_ts$GEO_IDS__COUNTY))
    div(
      div(
        selectizeInput(
          "ts1_county",
          multiple = TRUE,
          label = "County",
          choices = cty_choices
        )
      ),
      div(
        selectizeInput("ts1_variable", label = "Variable", multiple = TRUE, choices = variable_choices)
      )
    )
  })

  # timeseries plot ----

  output$ts1_plotly <- renderPlotly({
    plot_df <- rwjfs[rwjfs$GEO_IDS__COUNTY %in% input$ts1_county &
                     rwjfs$GEO_IDS__STATE == 'Virginia',
                     c('year', 'year_dt', 'GEO_IDS__STATE', 'GEO_IDS__COUNTY', input$ts1_variable)]

    if (length(input$ts1_county) == 0 | length(input$ts1_variable) == 0) {
      return(NULL)
    } else {
      bp <- plot_ly(x = unique(plot_df$year_dt))
      for (cty in input$ts1_county) {
        for (col in input$ts1_variable) {
          bp <- add_trace(bp, y = as.numeric(plot_df[plot_df$GEO_IDS__COUNTY == cty, col]), name = sprintf('%s: %s', cty, col), mode = 'lines+markers')
        }
      }
      return(bp)
    }
  })

  # SET REACTIVE VALUES FROM HTML MENU CHOICES -----------------------------------------

  # set map_level_choice
  observeEvent(input$map_level_choice, {
    map_values$map_level_choice <- input$map_level_choice
    print(paste("map_values$map_level_choice = ", map_values$map_level_choice))
  })
  # set map_state_choice
  observeEvent(input$map_state_choice, {
    map_values$map_state_choice <- input$map_state_choice
    print(paste("map_values$map_state_choice = ", map_values$map_state_choice))
  })
  # set map_county_choice
  observeEvent(input$map_county_choice, {
    map_values$map_county_choice <- input$map_county_choice
    print(paste("map_values$map_county_choice = ", map_values$map_county_choice))
  })
  #set map_poly_data_choice
  observeEvent(input$map_poly_data_choice, {
    map_values$map_poly_data_choice <- input$map_poly_data_choice
    print(paste("map_values$map_poly_data_choice = ", map_values$map_poly_data_choice))
  })



  # RENDER MAP STUFF  ------------------------------------------------------------------------

  observeEvent(map_values$map_poly_data_choice, {
    if (map_values$map_poly_data_choice != "") {
      print("Unlisting Parameters...")
      map_poly_data_choice_params <-
        unlist(strsplit(map_values$map_poly_data_choice, ";"))
      # make a reactive to be used in other reactives
      map_values$map_poly_data_choice_params <- map_poly_data_choice_params

      print("Building Query...")
      if (map_values$map_level_choice == 'County') {
        sql <- paste0(
          "select \"",
          map_poly_data_choice_params[3],
          "\", \"ST_SUBST_FIPS\" from ",
          map_poly_data_choice_params[9],
          " where LEFT(\"ST_SUBST_FIPS\", 2) = '",
          map_poly_data_choice_params[4],
          "'"
        )
      } else if (map_values$map_level_choice == 'Census Block Group') {
        sql <- paste0(
          "select \"",
          map_poly_data_choice_params[3],
          "\", \"GEOID\", \"NAME\" from ",
          map_poly_data_choice_params[9],
          " where \"ST_SUBST_FIPS\" = '",
          map_poly_data_choice_params[4],
          "'"
        )
      }

      print("Getting Data from DB...")
      col_data <- data.table::setDT(dbGetQuery(
        con,
        sql
      ))

      print("Combining Data with Geography...")
      if (map_values$map_level_choice == 'County') {
        data_spdf <- tmaptools::append_data(va_counties_spdf, col_data, "GEOID", "ST_SUBST_FIPS")
        # map_data <- data_spdf
        map_values$data <- data_spdf
      } else if (map_values$map_level_choice == 'Census Block Group') {
        data_spdf <- va_blockgroups_spdf[substr(va_blockgroups_spdf@data$GEOID,1,5) == map_poly_data_choice_params[4],]
        data_spdf <- tmaptools::append_data(data_spdf, col_data, "GEOID", "GEOID")
        # map_data <- data_spdf
        map_values$data <- data_spdf
      }
      print("Over coverage results")
      print(tmaptools::over_coverage())
      print("over coverage results end")

      # reorder columns
      refcols <- c("NAME", map_values$map_poly_data_choice_params[3])
      map_values$data@data <- map_values$data@data[, c(refcols, setdiff(names(map_values$data@data), refcols))]


      print("Creating Map")
      createBaseMap(map_values$data, map_poly_data_choice_params)

    }
  })

  # RENDER MAP PLOT STUFF ----

  observeEvent(map_values$map_poly_data_choice, {
    req(map_values$data)
    col_name <- map_values$map_poly_data_choice_params[3]
    print(sprintf('Column to plot histogram: %s', col_name))
    if (!is.null(col_name)) {
      createMapBarplot(map_values, map_values$map_poly_data_choice_params, col_name)
    } else {
      print('Null values to plot map barchart, probably nothing selected')
    }
  })

  observeEvent(input$locationData, {
    leafletProxy('mymap') %>% clearMarkers()
    for (d in as.list(input$locationData)) {
      vals <- unlist(strsplit(d, ";"))
      createProxyMapPoints(vals[1], vals[2], vals[3], vals[4], vals[5])
    }
  }, ignoreNULL = FALSE)

  # RENDER ANALYSIS ----

  # observeEvent(map_values$map_poly_data_choice, {
  #   createCorrelationDropDown(
  #     get_cont_geo_id((map_values$map_level_choice),
  #
  #                     (map_values$map_county_choice)),
  #     (map_values$map_level_choice))
  # })

  # RENDER DT ------------------------------------------------------------------------
  output$map_data_dt = DT::renderDataTable({
    map_values$data@data
  },
  filter = 'top',
  extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'RowReorder'),
  options = list(
    dom = 'Bfrtipl',
    buttons = c('copy', 'csv', 'excel'),
    colReorder = TRUE,
    fixedHeader = TRUE,
    rowReorder = TRUE,
    scrollX = TRUE
  ))

  # Box Labels ----

  output$map_title <- renderUI({
    sprintf('%s - %s',
            map_values$map_poly_data_choice_params[11],
            map_values$map_poly_data_choice_params[10])

  })

  output$map_bar_title <- renderUI({
    sprintf('%s - %s',
            map_values$map_poly_data_choice_params[11],
            map_values$map_poly_data_choice_params[10])

  })

})
