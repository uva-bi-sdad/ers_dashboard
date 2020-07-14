library(shiny)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(rgdal)
library(tmap)
library(scales)
library(ggvis)

sidebar <- dashboardSidebar(
  includeCSS("styles.css"),
  radioButtons(inputId="mapLevel", label="Map Level", 
               choices=c("County","Block Group", "School District")),
  tags$hr(style = "border-top: 2px solid #8c8b8b;"),
  div(style = "max-height:250px; overflow-y: auto;",
  conditionalPanel(
    condition = "input.mapLevel == 'County'",
    radioButtons(
      inputId = "foodAccessData", label = "Low Access to Grocery Store", selected=character(0),
      choices = c("Population %", "Low Income %", "Child %", "Senior %", "Household No Car %")
    ),
    radioButtons(
      inputId = "foodSubsidyData", label = "Food Subsidy", selected=character(0),
      choices = c("Free Lunch %", "Reduced Lunch %")
    )
  )),
  tags$hr(style = "border-top: 3px double #8c8b8b;"),
  radioButtons(
    inputId = "locationData", label = "Location Data",
    choices = c("Snap Merchants","Grocery Stores", "Places of Worship")
  )
)
body <- dashboardBody(
  fluidRow(
    tabBox(
      title = textOutput("mapTitle"),
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", width = NULL,
      tabPanel("Map", leafletOutput("mymap")),
      tabPanel("Data", textOutput("text1"))
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Layout"),
  sidebar,
  body
)

server <- function(input, output, session) {
  output$text1 <- renderText(paste("hello:", length(input$foodSubsidyData)))
  
  createProxyMap <- function(legendTitle, mapTitle, palette, column) {
    output$mapTitle <- renderText(mapTitle)
    leafletProxy('mymap', data = vamap) %>%
      addPolygons(stroke=TRUE,
                  weight=2,
                  smoothFactor = 0.2, 
                  fillOpacity = .75,
                  color= ~palette(column),
                  group=mapTitle,
                  layerId=geoID1,
                  label=vaPCT_LACCESS_POP10popup,
                  labelOptions= labelOptions(direction = 'auto')
      ) %>%
      clearControls() %>%
      addLegend(pal = palette,
                values = ~column,
                opacity = 1,
                position = "topleft",
                title = legendTitle
      )
  }
  
  # Read in the shapefile for US states and counties:
  usshapefile <- "data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp"
  usgeo <- read_shape(file=usshapefile)
  
  # Virginia shapefile:
  vafipscode <- "51"
  vageo <- usgeo[usgeo@data$STATEFP==vafipscode,]
  
  # Food ACCESS data
  usdatafile <- "data/FoodEnvironment.xlsx"
  usdataACCESS <- rio::import(usdatafile, which = "ACCESS")
  vadataACCESS <- usdataACCESS[usdataACCESS$State=="VA" & usdataACCESS$FIPS < 51899,]
  vadataACCESS <- subset(vadataACCESS, !(vadataACCESS$FIPS %in% c("51515", "51560", "51780")))
  vadataACCESS$County_FIPS <- substr(vadataACCESS$FIPS, nchar(vadataACCESS$FIPS)-3+1, nchar(vadataACCESS$FIPS))
  vamap <- append_data(vageo, vadataACCESS, key.shp = "COUNTYFP", key.data = "County_FIPS")
  
  # Food ASSISTANCE data
  usdatafile <- "data/FoodEnvironment.xlsx"
  usdataASSISTANCE <- rio::import(usdatafile, which = "ASSISTANCE")
  vadataASSISTANCE <- usdataASSISTANCE[usdataASSISTANCE$State=="VA" & usdataASSISTANCE$FIPS < 51899,]
  vadataASSISTANCE <- subset(vadataASSISTANCE, !(vadataASSISTANCE$FIPS %in% c("51515", "51560", "51780")))
  vadataASSISTANCE$County_FIPS <- substr(vadataASSISTANCE$FIPS, nchar(vadataASSISTANCE$FIPS)-3+1, nchar(vadataASSISTANCE$FIPS))
  vamap <- append_data(vamap, vadataASSISTANCE, key.shp = "COUNTYFP", key.data = "County_FIPS")
  
  # LayerIds
  geoID1 <- as.vector(paste0(vamap$COUNTYFP, "a"))
  
  # Color Palettes
  vaPCT_LACCESS_POP10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_POP10, n = 4)
  vaPCT_LACCESS_LOWI10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_LOWI10, n = 4)
  vaPCT_LACCESS_CHILD10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_CHILD10, n = 4)
  vaPCT_LACCESS_SENIORS10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_SENIORS10, n = 4)
  vaPCT_LACCESS_HHNV10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_HHNV10, n = 4)
  
  # Pop-ups:
  vaPCT_LACCESS_POP10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_POP10, 2), "%")
  vaPCT_LACCESS_LOWI10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_LOWI10, 2), "%")
  vaPCT_LACCESS_CHILD10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_CHILD10, 2), "%")
  vaPCT_LACCESS_SENIORS10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_SENIORS10, 2), "%")
  vaPCT_LACCESS_HHNV10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_HHNV10, 2), "%")
  
  
  # generate basemap
  output$mymap <- renderLeaflet({
    output$mapTitle <- renderText("Virginia")
    leaflet(vamap) %>%
      setView(lat = 37.9316, lng = -79.6569, zoom = 7) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke=TRUE,
                    weight=2,
                    smoothFactor = 0.2, 
                    fillOpacity = .1
        )
  })
  
   #reactive({
     
  #   if (length(input$foodAccessData) > 0) input$foodSubsidyData = character(0)
  #   else if (length(input$foodSubsidyData) > 0) input$foodAccessData = character(0)
  # })
  
  # generate map layers
  observe({
    
    if (length(input$foodAccessData) > 0) {
      
      if (input$foodAccessData == "Population %") {
        
        updateRadioButtons(session, "foodSubsidyData",
                           selected = character(0)
        )
        
        createProxyMap("Population Low Access %",
                       "Population, low access to store (%), 2010",
                       colorQuantile("YlGnBu", vamap@data$PCT_LACCESS_POP10, n = 4),
                       vamap@data$PCT_LACCESS_POP10)
        
      } else if (input$foodAccessData == "Low Income %") {
        createProxyMap("Low Income Low Access %",
                       "Low Income, low access to store (%), 2010",
                       colorQuantile("YlGnBu", vamap@data$PCT_LACCESS_LOWI10, n = 4),
                       vamap@data$PCT_LACCESS_LOWI10)
        
      } else if (input$foodAccessData == "Child %") {
        createProxyMap("Child Low Access %",
                       "Child, low access to store (%), 2010",
                       colorQuantile("YlGnBu", vamap@data$PCT_LACCESS_CHILD10, n = 4),
                       vamap@data$PCT_LACCESS_CHILD10)
        
      } else if (input$foodAccessData == "Senior %") {
        createProxyMap("Seniors, Low Store Access %",
                       "Seniors, low access to store (%), 2010",
                       colorQuantile("YlGnBu", vamap@data$PCT_LACCESS_SENIORS10, n = 4),
                       vamap@data$PCT_LACCESS_SENIORS10)
        
      } else if (input$foodAccessData == "Household No Car %") {
        createProxyMap("Household No Car, Low Store Access %",
                       "Households, no car & low access to store (%), 2010",
                       colorQuantile("YlGnBu", vamap@data$PCT_LACCESS_HHNV10, n = 4),
                       vamap@data$PCT_LACCESS_HHNV10)
        
      }
    }
    
    if (length(input$foodSubsidyData) > 0) {
      
      if (input$foodSubsidyData == "Reduced Lunch %") {
        createProxyMap("Students Eligible Reduced-Price Lunch %",
                       "Students eligible for reduced-price lunch (%), 2010",
                       colorQuantile("YlGnBu", vamap@data$PCT_REDUCED_LUNCH10, n = 4),
                       vamap@data$PCT_REDUCED_LUNCH10)
        
      } else if (input$foodSubsidyData == "Free Lunch %") {
        createProxyMap("Students eligible for free lunch (%), 2010",
                       "Students Eligible Free Lunch %",
                       colorQuantile("YlGnBu", vamap@data$PCT_FREE_LUNCH10, n = 4),
                       vamap@data$PCT_FREE_LUNCH10)
      }
    }
  })
}

shinyApp(ui, server)
