# server.R

library(magrittr)
library(rgdal)
library(tmap)
library(scales)
library(sqldf)
library(xts)
library(rio)

shinyServer(function(input, output, session) {
  # data and functions
  source("spatial_data.R", local = TRUE)
  source("create_map.R", local = TRUE)
  source("plots.R", local = TRUE)
  source("selection_menus.R", local = TRUE)
  
  values <- reactiveValues(
    plotColumn = "",
    percentage = TRUE,
    corrCoefficient = 0,
    selected_county_fips = "",
    spdf = "va_counties_spdf"
  )
  
  # ui render
  output$tit0 <- mapLevelTitle <- renderUI({
    div("Map Level", class = "menuTitle")
  })
  
  output$rbg0 <- mapLevel <- renderUI({
    div(radioButtons(
      inputId = "mapLevel",
      label = NULL,
      choices = c("State Counties", "County Block Groups")
    ),
    style = "padding-bottom:10px")
  })
  
  output$countySelect <- renderUI({
    div(conditionalPanel(condition = "input.mapLevel == 'County Block Groups'",
                         HTML(
                           read_lines("selection_menu_county_blockgroups.html")
                         )))
  })
  
  output$tit1 <- mapLevelDataTitle <- renderUI({
    div("Map Level Data", class = "menuTitle")
  })
  
  output$areaDataMenu <- renderUI({
    div(
      class = "areaDataDiv",
      conditionalPanel(condition = "input.mapLevel == 'County Block Groups'",
                       HTML(
                         read_lines("selection_menu_blockgroups_data.html")
                       )),
      conditionalPanel(condition = "input.mapLevel == 'State Counties'",
                       HTML(
                         read_lines("selection_menu_data_county_level.html")
                       ))
    )
  })
  
  output$locationDataMenu <- renderUI({
    HTML(read_lines("selection_menu_data_point_level.html"))
  })
  
  output$cor1 <- renderUI({
    div(
      div(
        style = "display:inline-block",
        selectizeInput(
          "corrVar1",
          label = "V1",
          choices = c(
            "Population Low Access %" = "PCT_LACCESS_POP10",
            "Low Income Low Access %" = "PCT_LACCESS_LOWI10",
            "Child Low Access %" = "PCT_LACCESS_CHILD10",
            "Seniors, Low Store Access %" = "PCT_LACCESS_SENIORS10",
            "Household No Car, Low Store Access %" = "PCT_LACCESS_HHNV10",
            "Students Eligible Reduced Price Lunch %" = "PCT_REDUCED_LUNCH10",
            "Students Eligible Free Lunch %" = "PCT_FREE_LUNCH10",
            "Obesity 20yrs+ %" = "SE_T012_003",
            "Adult Diabetics %" = "SE_T009_001",
            "Population 2013" = "TotalPopulation",
            "Median Age 2013" = "MedianAge",
            "Median Household Income 2013" = "MedianHouseholdIncome",
            "HS Graduation Rate" = "Graduation Rate",
            "Household SNAP Percent" = "HouseholdSNAPPercent"
          )
        )
      ),
      div(
        style = "display:inline-block",
        selectizeInput(
          "corrVar2",
          label = "V2",
          choices = c(
            "Population Low Access %" = "PCT_LACCESS_POP10",
            "Low Income Low Access %" = "PCT_LACCESS_LOWI10",
            "Child Low Access %" = "PCT_LACCESS_CHILD10",
            "Seniors, Low Store Access %" = "PCT_LACCESS_SENIORS10",
            "Household No Car, Low Store Access %" = "PCT_LACCESS_HHNV10",
            "Students Eligible Reduced Price Lunch %" = "PCT_REDUCED_LUNCH10",
            "Students Eligible Free Lunch %" = "PCT_FREE_LUNCH10",
            "Obesity 20yrs+ %" = "SE_T012_003",
            "Adult Diabetics %" = "SE_T009_001",
            "Population 2013" = "TotalPopulation",
            "Median Age 2013" = "MedianAge",
            "Median Household Income 2013" = "MedianHouseholdIncome",
            "HS Graduation Rate" = "Graduation Rate",
            "Household SNAP Percent" = "HouseholdSNAPPercent"
          )
        )
      ),
      actionButton("corr1Button", "Go!")
    )
  })
  
  output$ts1 <- renderUI({
    tsd <-
      read.csv("data/countyObesityAndTeenBirths20112016.csv",
               stringsAsFactors = FALSE)
    vaCounties <- unique(tsd[, c("County")])
    div(
      div(
        style = "display:inline-block",
        selectizeInput(
          "tsItem",
          label = "Item",
          multiple = TRUE,
          choices = c(
            "Adult Obesity %" = "Pct.Obese",
            "Teen Birth Rate per 1000" = "Teen.Birth.Rate",
            "Corn Acreage" = "Corn.Acreage",
            "Soybean Acreage" = "Soybean.Acreage",
            "Hops Acreage" = "Hops.Acreage"
          )
        )
      ),
      div(
        style = "display:inline-block",
        selectizeInput("tsCounty", label = "County", choices = vaCounties)
      ),
      actionButton("ts1Button", "Go!")
    )
  })
  
  # menu interactions
  # basemap render
  observeEvent(input$mapLevel, {
    if (input$mapLevel == "State Counties") {
      values$spdf <- "va_counties_spdf"
      spdf <- get(values$spdf)
      createBaseMap(spdf, c("Virginia", "", -79.6569, 38.8816, 7))
    }
  })
  
  observeEvent(input$selected_county_fips, {
    if (input$mapLevel == "State Counties") {
      values$spdf <- "va_counties_spdf"
      spdf <- get(values$spdf)
      createBaseMap(spdf, c("Virginia", "", -79.6569, 38.8816, 7))
      
    }
    else if (input$mapLevel == "County Block Groups") {
      values$selected_county_fips <- input$selected_county_fips
      values$spdf <- "va_blockgroups_spdf"
      spdf <- get(values$spdf)
      createBaseMap(spdf, unlist(strsplit(values$selected_county_fips, ";")))
    }
  })
  
  observeEvent(input$mapLevelData, {
    vals <- unlist(strsplit(input$mapLevelData, ";"))
    spdf <- get(values$spdf)
    values$plotColumn <- vals[3]
    if (!vals[3] %in% c("TotalPopulation", "MedianAge", "MedianHouseholdIncome")) {
      values$percentage <- TRUE
      createProxyMap(
        vals[1],
        vals[2],
        colorBin(
          "YlGnBu",
          c(
            min(as.numeric(as.character(spdf@data[, vals[3]])), na.rm = TRUE),
            max(as.numeric(as.character(spdf@data[, vals[3]])), na.rm = TRUE)
          ),
          bins = 6,
          pretty = TRUE
        ),
        spdf,
        as.numeric(as.character(spdf@data[, vals[3]])),
        spdf@data$GEOID,
        "GEOID"
      )
    }
    else {
      values$percentage <- FALSE
      createProxyMap(
        vals[1],
        vals[2],
        colorNumeric("Blues", spdf@data[, vals[3]]),
        spdf,
        spdf@data[, vals[3]],
        spdf@data$GEOID,
        "GEOID"
      )
    }
    
    if (input$mapLevel == "State Counties") {
      col_index <- which(colnames(spdf@data) == vals[3])
      col_index_data <-
        spdf@data[!is.na(spdf@data[, col_index]), c(1:5, col_index)]
      output$data1 <-
        renderDataTable(spdf@data[!is.na(spdf@data[, col_index]), c(1:5, col_index)], options = list(pageLength = 25))
      makeHistogram(vals[1], values$plotColumn)
      output$plotCol <- renderText(values$plotColumn)
    }
  })
  
  observeEvent(input$locationData, {
    leafletProxy('mymap') %>% clearMarkers()
    for (d in as.list(input$locationData)) {
      vals <- unlist(strsplit(d, ";"))
      createProxyMapPoints(vals[1], vals[2], vals[3], vals[4], vals[5])
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$corr1Button, {
    makeCorrelation(input$corrVar1, input$corrVar2)
    output$tabset2title <-
      renderText(paste("RSQ:", format(
        round(values$corrCoefficient ^ 2, 2), nsmall = 2
      )))
  })
  
  observeEvent(input$ts1Button, {
    myData <-
      read.csv("data/timeSeriesData.csv", stringsAsFactors = FALSE)
    county <- input$tsCounty
    item <- input$tsItem
    v1 <-
      as.data.frame(myData[which(myData$County == county), c(item, "Year")])
    v1$Year <- strptime(v1$Year, "%Y")
    names(v1) <- c(item, "Year")
    v1_xts <- xts(v1[, c(item)], order.by = v1[, "Year"])
    names(v1_xts) <- c(item)
    output$dygraph1 <- renderDygraph(dygraph(v1_xts))
    output$tabset2title <- renderText("")
  })
  
})
