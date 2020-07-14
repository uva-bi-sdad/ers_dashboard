library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(magrittr)
library(rgdal)
library(tmap)
library(scales)
library(ggvis)
library(DT)

source("sidebar.R")
source("body.R")

ui <- dashboardPage(
  dashboardHeader(title = "Column layout"),
  sidebar,
  body
)

server <- function(input, output) {
  
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
  
  # # Check if county names are in the same format in both files
  # str(vageo@data$NAME)
  # str(vadataACCESS$County)
  # # Change the county names to plain characters in scgeo:
  # vageo@data$NAME <- as.character(vageo@data$NAME)
  # # Order each data set by county name
  # vageo <- vageo[order(vageo@data$NAME),]
  # vadataACCESS <- vadataACCESS[order(vadataACCESS$County),]
  # # Are the two county columns identical now? They should be:
  # identical(vageo@data$NAME,vadataACCESS$County )
  # Add data to map
  vamap <- append_data(vageo, vadataACCESS, key.shp = "COUNTYFP", key.data = "County_FIPS")
  
  # Food ASSISTANCE data
  usdatafile <- "data/FoodEnvironment.xlsx"
  usdataASSISTANCE <- rio::import(usdatafile, which = "ASSISTANCE")
  vadataASSISTANCE <- usdataASSISTANCE[usdataASSISTANCE$State=="VA" & usdataASSISTANCE$FIPS < 51899,]
  vadataASSISTANCE <- subset(vadataASSISTANCE, !(vadataASSISTANCE$FIPS %in% c("51515", "51560", "51780")))
  vadataASSISTANCE$County_FIPS <- substr(vadataASSISTANCE$FIPS, nchar(vadataASSISTANCE$FIPS)-3+1, nchar(vadataASSISTANCE$FIPS))
  vamap <- append_data(vamap, vadataASSISTANCE, key.shp = "COUNTYFP", key.data = "County_FIPS")
  
  # Food STORES data
  usdatafile <- "data/FoodEnvironment.xlsx"
  usdataSTORES <- rio::import(usdatafile, which = "STORES")
  vadataSTORES <- usdataSTORES[usdataSTORES$State=="VA" & usdataSTORES$FIPS < 51899,]
  vadataSTORES <- subset(vadataSTORES, !(vadataSTORES$FIPS %in% c("51515", "51560", "51780")))
  vadataSTORES$County_FIPS <- substr(vadataSTORES$FIPS, nchar(vadataSTORES$FIPS)-3+1, nchar(vadataSTORES$FIPS))
  vamap <- append_data(vamap, vadataSTORES, key.shp = "COUNTYFP", key.data = "County_FIPS")
  
  # Food HEALTH data
  usdatafile <- "data/FoodEnvironment.xlsx"
  usdataHEALTH <- rio::import(usdatafile, which = "HEALTH")
  vadataHEALTH <- usdataHEALTH[usdataHEALTH$State=="VA" & usdataHEALTH$FIPS < 51899,]
  vadataHEALTH <- subset(vadataHEALTH, !(vadataHEALTH$FIPS %in% c("51515", "51560", "51780")))
  vadataHEALTH$County_FIPS <- substr(vadataHEALTH$FIPS, nchar(vadataHEALTH$FIPS)-3+1, nchar(vadataHEALTH$FIPS))
  vamap <- append_data(vamap, vadataHEALTH, key.shp = "COUNTYFP", key.data = "County_FIPS")
  
  # COUNTY HEALTH data
  vadatafile <- "data/county_health_2016/R11241936_SL050.csv"
  vadataCOUNTYHEALTH <- rio::import(vadatafile)
  vadataCOUNTYHEALTH <- subset(vadataCOUNTYHEALTH, !(vadataCOUNTYHEALTH$Geo_FIPS %in% c("51515", "51560", "51780")))
  vadataCOUNTYHEALTH$Geo_FIPS_Co <- substr(vadataCOUNTYHEALTH$Geo_FIPS, 3, 5)
  vamap <- append_data(vamap, vadataCOUNTYHEALTH, key.shp = "COUNTYFP", key.data = "Geo_FIPS_Co")
  
  # Population Data
  uspop <- read.csv("data/CO-EST2015-alldata.csv")
  vapop <- uspop[uspop$STATE=="51" & uspop$COUNTY!="0",]
  vapop <- vapop[order(vapop$COUNTY),]
  vapop$COUNTY_PAD <- stringr::str_pad(vapop$COUNTY, 3, pad = 0)
  vamap <- append_data(vamap, vapop, key.shp = "COUNTYFP", key.data = "COUNTY_PAD")
  
  # Snap Retailer Locations
  snapRetail <- read.csv("data/VASnapRetailers.csv")
  
  # LayerIds
  geoID1 <- as.vector(paste0(vamap$COUNTYFP, "a"))
  geoID2 <- as.vector(paste0(vamap$COUNTYFP, "b"))
  geoID3 <- as.vector(paste0(vamap$COUNTYFP, "c"))
  geoID4 <- as.vector(paste0(vamap$COUNTYFP, "d"))
  geoID5 <- as.vector(paste0(vamap$COUNTYFP, "e"))
  
  # Color Palettes
  vaPCT_LACCESS_POP10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_POP10, n = 4)
  vaPCT_LACCESS_LOWI10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_LOWI10, n = 4)
  vaPCT_LACCESS_CHILD10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_CHILD10, n = 4)
  vaPCT_LACCESS_SENIORS10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_SENIORS10, n = 4)
  vaPCT_LACCESS_HHNV10Palette <- colorQuantile("YlGnBu", vamap$PCT_LACCESS_HHNV10, n = 4)
  vaPCT_REDUCED_LUNCH10Palette <- colorQuantile("YlGnBu", vamap$PCT_REDUCED_LUNCH10, n = 4)
  vaPCT_FREE_LUNCH10Palette <- colorQuantile("YlGnBu", vamap$PCT_FREE_LUNCH10, n = 4)
  vaGROCPTH12Palette <- colorBin("YlGnBu", vamap$GROCPTH12, bins = 4)
  vaCONVSPTH12Palette <- colorBin("YlGnBu", vamap$CONVSPTH12, bins = 4)
  vaSE_NV008_001Palette <- colorBin("YlGnBu", vamap$SE_NV008_001, bins = 4)
  vaSE_NV008_002Palette <- colorBin("YlGnBu", vamap$SE_NV008_002, bins = 4)
  vaSE_NV008_003Palette <- colorBin("YlGnBu", vamap$SE_NV008_003, bins = 4)
  
  # Pop-ups:
  vaPCT_LACCESS_POP10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_POP10, 2), "%")
  vaPCT_LACCESS_LOWI10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_LOWI10, 2), "%")
  vaPCT_LACCESS_CHILD10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_CHILD10, 2), "%")
  vaPCT_LACCESS_SENIORS10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_SENIORS10, 2), "%")
  vaPCT_LACCESS_HHNV10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_LACCESS_HHNV10, 2), "%")
  vaPCT_REDUCED_LUNCH10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_REDUCED_LUNCH10, 2), "%")
  vaPCT_FREE_LUNCH10popup <- paste0(vamap@data$County, ": ", round(vamap@data$PCT_FREE_LUNCH10, 2), "%")
  vaGROCPTH12popup <- paste0(vamap@data$County, ": ", round(vamap@data$GROCPTH12, 2))
  vaCONVSPTH12popup <- paste0(vamap@data$County, ": ", round(vamap@data$CONVSPTH12,2))
  vaSE_NV008_001popup <- paste0(vamap@data$County, ": ", round(vamap@data$SE_NV008_001,2))
  vaSE_NV008_002popup <- paste0(vamap@data$County, ": ", round(vamap@data$SE_NV008_002,2))
  vaSE_NV008_003popup <- paste0(vamap@data$County, ": ", round(vamap@data$SE_NV008_003,2))
  
  
  # render map
  output$mymap <- renderLeaflet({
    if (as.integer(input$areaData) == 1) {
      leaflet(vamap) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke=TRUE,
                    weight=2,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_POP10popup, 
                    color= ~vaPCT_LACCESS_POP10Palette(vamap@data$PCT_LACCESS_POP10),
                    group="Population, low access to store (%), 2010",
                    layerId=geoID1,
                    label=vaPCT_LACCESS_POP10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_LOWI10popup, 
                    color= ~vaPCT_LACCESS_LOWI10Palette(vamap@data$PCT_LACCESS_LOWI10),
                    group="Low Income, low access to store (%), 2010",
                    layerId=geoID2,
                    label=vaPCT_LACCESS_LOWI10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_CHILD10popup, 
                    color= ~vaPCT_LACCESS_CHILD10Palette(vamap@data$PCT_LACCESS_CHILD10),
                    group="Child, low access to store (%), 2010",
                    layerId=geoID3,
                    label=vaPCT_LACCESS_CHILD10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_SENIORS10popup, 
                    color= ~vaPCT_LACCESS_SENIORS10Palette(vamap@data$PCT_LACCESS_SENIORS10),
                    group="Seniors, low access to store (%), 2010",
                    layerId=geoID4,
                    label=vaPCT_LACCESS_SENIORS10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_HHNV10popup, 
                    color= ~vaPCT_LACCESS_HHNV10Palette(vamap@data$PCT_LACCESS_HHNV10),
                    group="Households, no car & low access to store (%), 2010",
                    layerId=geoID5,
                    label=vaPCT_LACCESS_HHNV10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addLayersControl(
          baseGroups=c("Population, low access to store (%), 2010", 
                       "Low Income, low access to store (%), 2010", 
                       "Child, low access to store (%), 2010", 
                       "Seniors, low access to store (%), 2010", 
                       "Households, no car & low access to store (%), 2010"),
          position = "topleft",
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        
        htmlwidgets::onRender(
          "function(el, t) {
          var defaultStyle = {
          color: '#000000',
          opacity:0.5,
          weight: 1,
          fillOpacity: 0.7,
          };
          var highlightStyle = {
          color: '#ff0000',
          opacity:1,
          weight: 3,
          fillOpacity: 1,
          };
          
          var myMap = this;
          
          // query all polygons
          var layers = myMap.layerManager._byCategory.shape;
          for(var i in layers) {
          var layer = layers[i];
          // safety check; only apply the effects on polygons w/ labels
          if(layer.label) {
          layer.on('mouseover',
          function(e) {
          this.setStyle(highlightStyle);
          this.bringToFront();
          });
          layer.on('mouseout',
          function(e) {
          this.setStyle(defaultStyle);
          this.bringToBack();
          });
          }
          }
    }")
      
} 
    else if (as.integer(input$areaData) == 2) {
      leaflet(vamap) %>%
        addProviderTiles("CartoDB.Positron") %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_REDUCED_LUNCH10popup, 
                    color= ~vaPCT_REDUCED_LUNCH10Palette(vamap@data$PCT_REDUCED_LUNCH10),
                    group="Students eligible for reduced-price lunch (%), 2010",
                    layerId=geoID1,
                    label=vaPCT_REDUCED_LUNCH10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_FREE_LUNCH10popup, 
                    color= ~vaPCT_FREE_LUNCH10Palette(vamap@data$PCT_FREE_LUNCH10),
                    group="Students eligible for free lunch (%), 2010",
                    layerId=geoID2,
                    label=vaPCT_FREE_LUNCH10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addLayersControl(
          baseGroups=c("Students eligible for reduced-price lunch (%), 2010", 
                       "Students eligible for free lunch (%), 2010"),
          position = "topleft",
          options = layersControlOptions(collapsed = FALSE)
        ) 
      
    } 
    else if (as.integer(input$select) == 3) {
      leaflet(vamap) %>%
        addProviderTiles("CartoDB.Positron") %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaGROCPTH12popup, 
                    color= ~vaGROCPTH12Palette(vamap@data$GROCPTH12),
                    group="Grocery stores/1,000 pop, 2012",
                    layerId=geoID1,
                    label=vaGROCPTH12popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaCONVSPTH12popup, 
                    color= ~vaCONVSPTH12Palette(vamap@data$CONVSPTH12),
                    group="Convenience stores/1,000 pop, 2012",
                    layerId=geoID2,
                    label=vaCONVSPTH12popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addLayersControl(
          baseGroups=c("Grocery stores/1,000 pop, 2012",
                       "Convenience stores/1,000 pop, 2012"),
          position = "topleft",
          options = layersControlOptions(collapsed = FALSE)
        ) 
    }
    else if (as.integer(input$select) == 4) {
      leaflet(vamap) %>%
        addProviderTiles("CartoDB.Positron") %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaSE_NV008_001popup, 
                    color= ~vaSE_NV008_001Palette(vamap@data$SE_NV008_001),
                    group="Teen Births Rate per 100,000 Population (Fem 15-19 Yrs), 2016",
                    layerId=geoID1,
                    label=vaSE_NV008_001popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaSE_NV008_002popup, 
                    color= ~vaSE_NV008_002Palette(vamap@data$SE_NV008_002),
                    group="Chlamydia Cases Rate per 100,000 Population, 2016",
                    layerId=geoID2,
                    label=vaSE_NV008_002popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaSE_NV008_003popup, 
                    color= ~vaSE_NV008_003Palette(vamap@data$SE_NV008_003),
                    group="HIV Prevalence Rate per 100,000 Population, 2016",
                    layerId=geoID3,
                    label=vaSE_NV008_003popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addLayersControl(
          baseGroups=c("Teen Births Rate per 100,000 Population (Fem 15-19 Yrs), 2016", 
                       "Chlamydia Cases Rate per 100,000 Population, 2016", 
                       "HIV Prevalence Rate per 100,000 Population, 2016"),
          position = "topleft",
          options = layersControlOptions(collapsed = FALSE)
        ) 
    }
    else {
      leaflet(vamap) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke=TRUE,
                    weight=2,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_POP10popup, 
                    color= ~vaPCT_LACCESS_POP10Palette(vamap@data$PCT_LACCESS_POP10),
                    group="Population, low access to store (%), 2010",
                    layerId=geoID1,
                    label=vaPCT_LACCESS_POP10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_LOWI10popup, 
                    color= ~vaPCT_LACCESS_LOWI10Palette(vamap@data$PCT_LACCESS_LOWI10),
                    group="Low Income, low access to store (%), 2010",
                    layerId=geoID2,
                    label=vaPCT_LACCESS_LOWI10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_CHILD10popup, 
                    color= ~vaPCT_LACCESS_CHILD10Palette(vamap@data$PCT_LACCESS_CHILD10),
                    group="Child, low access to store (%), 2010",
                    layerId=geoID3,
                    label=vaPCT_LACCESS_CHILD10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_SENIORS10popup, 
                    color= ~vaPCT_LACCESS_SENIORS10Palette(vamap@data$PCT_LACCESS_SENIORS10),
                    group="Seniors, low access to store (%), 2010",
                    layerId=geoID4,
                    label=vaPCT_LACCESS_SENIORS10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addPolygons(stroke=TRUE,
                    weight=1,
                    smoothFactor = 0.2, 
                    fillOpacity = .75, 
                    #popup=vaPCT_LACCESS_HHNV10popup, 
                    color= ~vaPCT_LACCESS_HHNV10Palette(vamap@data$PCT_LACCESS_HHNV10),
                    group="Households, no car & low access to store (%), 2010",
                    layerId=geoID5,
                    label=vaPCT_LACCESS_HHNV10popup,
                    labelOptions= labelOptions(direction = 'auto')
        ) %>%
        
        addLayersControl(
          baseGroups=c("Population, low access to store (%), 2010", 
                       "Low Income, low access to store (%), 2010", 
                       "Child, low access to store (%), 2010", 
                       "Seniors, low access to store (%), 2010", 
                       "Households, no car & low access to store (%), 2010"),
          position = "topleft",
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        
        htmlwidgets::onRender(
          "function(el, t) {
          var defaultStyle = {
          color: '#000000',
          opacity:0.5,
          weight: 1,
          fillOpacity: 0.7,
          };
          var highlightStyle = {
          color: '#ff0000',
          opacity:1,
          weight: 3,
          fillOpacity: 1,
          };
          
          var myMap = this;
          
          // query all polygons
          var layers = myMap.layerManager._byCategory.shape;
          for(var i in layers) {
          var layer = layers[i];
          // safety check; only apply the effects on polygons w/ labels
          if(layer.label) {
          layer.on('mouseover',
          function(e) {
          this.setStyle(highlightStyle);
          this.bringToFront();
          });
          layer.on('mouseout',
          function(e) {
          this.setStyle(defaultStyle);
          this.bringToBack();
          });
          }
          }
    }")
    }
})
  
  # change left hand info on map click
  observe({
    county_fips <- renderText({ print(input$mymap_shape_mouseover$id) })
    output$mymap_shape_click <- infoFunc(county_fips)
  })
  
  county_names <- function(x) {
    if(is.null(x)) return(NULL)
    county_fips <- paste0(format(x[1]))
    county_data <- ""
    if (county_fips != "") {
      county_data <- vamap[vamap$COUNTY_PAD==substr(county_fips,1,3),]
      county_data <- as.data.frame(county_data[1,])
    }
    else {
      county_data <- vamap[vamap$COUNTY_PAD=="001",]
      county_data <- as.data.frame(county_data[1,])
    }
    countyName <- renderText({ print(paste("<b>County:</b> ",
                                           county_data[,c("NAME")])) })
    print(countyName())
  }
  
  # change things on map group change 
  observeEvent(input$mymap_groups,{
    pltdat <- ""
    if (input$mymap_groups == "Population, low access to store (%), 2010") {
      pltdat <- "PCT_LACCESS_POP10"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaPCT_LACCESS_POP10Palette,
                  values = ~PCT_LACCESS_POP10,
                  opacity = 1,
                  title = "Population, low access to store (%)"
        )
    } else if (input$mymap_groups == "Low Income, low access to store (%), 2010") {
      pltdat <- "PCT_LACCESS_LOWI10"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaPCT_LACCESS_LOWI10Palette,
                  values = ~PCT_LACCESS_LOWI10,
                  opacity = 1,
                  title = "Population, low access to store (%)"
        )
    } else if (input$mymap_groups == "Child, low access to store (%), 2010") {
      pltdat <- "PCT_LACCESS_CHILD10"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaPCT_LACCESS_CHILD10Palette,
                  values = ~PCT_LACCESS_CHILD10,
                  opacity = 1,
                  title = "Population, low access to store (%)"
        )
    } else if (input$mymap_groups == "Seniors, low access to store (%), 2010") {
      pltdat <- "PCT_LACCESS_SENIORS10"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaPCT_LACCESS_SENIORS10Palette,
                  values = ~PCT_LACCESS_SENIORS10,
                  opacity = 1,
                  title = "Population, low access to store (%)"
        )
    } else if (input$mymap_groups == "Households, no car & low access to store (%), 2010") {
      pltdat <- "PCT_LACCESS_HHNV10"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaPCT_LACCESS_HHNV10Palette,
                  values = ~PCT_LACCESS_HHNV10,
                  opacity = 1,
                  title = "Population, low access to store (%)"
        )
    } else if (input$mymap_groups == "Students eligible for reduced-price lunch (%), 2010") {
      pltdat <- "PCT_REDUCED_LUNCH10"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaPCT_REDUCED_LUNCH10Palette,
                  values = ~PCT_REDUCED_LUNCH10,
                  opacity = 1,
                  title = "Population, Food Assistance (%)"
        )
    } else if (input$mymap_groups == "Students eligible for free lunch (%), 2010") {
      pltdat <- "PCT_FREE_LUNCH10"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaPCT_FREE_LUNCH10Palette,
                  values = ~PCT_FREE_LUNCH10,
                  opacity = 1,
                  title = "Population, Food Assistance (%)"
        )
    } else if (input$mymap_groups == "Grocery stores/1,000 pop, 2012") {
      pltdat <- "GROCPTH12"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaGROCPTH12Palette, 
                  values = ~GROCPTH12, 
                  opacity = 1, 
                  title = "Stores per 1,000 Population"
        )
    } else if (input$mymap_groups == "Convenience stores/1,000 pop, 2012") {
      pltdat <- "CONVSPTH12"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaCONVSPTH12Palette, 
                  values = ~CONVSPTH12, 
                  opacity = 1, 
                  title = "Stores per 1,000 Population"
        )
    } else if (input$mymap_groups == "Teen Births Rate per 100,000 Population (Fem 15-19 Yrs), 2016") {
      pltdat <- "SE_NV008_001"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>% 
        addLegend(pal = vaSE_NV008_001Palette, 
                  values = ~SE_NV008_001, 
                  opacity = 1, 
                  title = "Cases per 100,000 Population"
        )
    } else if (input$mymap_groups == "Chlamydia Cases Rate per 100,000 Population, 2016") {
      pltdat <- "SE_NV008_002"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>% 
        addLegend(pal = vaSE_NV008_002Palette, 
                  values = ~SE_NV008_002, 
                  opacity = 1, 
                  title = "Cases per 100,000 Population"
        )
    } else if (input$mymap_groups == "HIV Prevalence Rate per 100,000 Population, 2016") {
      pltdat <- "SE_NV008_003"
      leafletProxy('mymap', data = vamap) %>%
        clearControls() %>%
        addLegend(pal = vaSE_NV008_003Palette, 
                  values = ~SE_NV008_003, 
                  opacity = 1, 
                  title = "Cases per 100,000 Population"
        )
    }
    
    
    plot_data <- as.data.frame(vamap[c("COUNTY_PAD", pltdat)])
    #plot_data_ordered <- plot_data[order(plot_data$PCT_LACCESS_POP10),]
    
    # Lables for axes
    xvar_name <- "County FIPS Code"
    yvar_name <- input$mymap_groups
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    #xvar <- prop("x", as.symbol(pltdat))
    yvar <- prop("y", as.symbol(pltdat))
    
    plot_data %>% 
      ggvis(~COUNTY_PAD, yvar, stroke.hover := 200) %>% 
      layer_bars(fill.hover := "red") %>%
      set_options(width = "auto") %>% 
      add_axis("x", title = "County FIPS Code", tick_padding = 15, properties = axis_props(
        #axis = list(stroke = "red", strokeWidth = 5),
        grid = list(stroke = "blue"),
        ticks = list(stroke = "blue", strokeWidth = 2),
        labels = list(angle = 270, align = "left", fontSize = 6))) %>% 
      add_axis("y", title = yvar_name) %>% 
      #scale_ordinal('x', domain=c('001','003','005','007','009','011')) %>% 
      add_tooltip(county_names, "hover") %>% 
      handle_hover(clickFunc) %>%
      bind_shiny("plot1")
    
    corrdata <- as.data.frame(vamap)
    corrdata %>% 
      ggvis(~corrdata$PCT_LACCESS_CHILD10, ~corrdata$PCT_OBESE_CHILD11) %>%
      set_options(width = 355, height = 390) %>%
      layer_points() %>% 
      add_axis("x", title = "% Children w/ Low Store Access, 2010", tick_padding = 15, properties = axis_props(
        #axis = list(stroke = "red", strokeWidth = 5),
        grid = list(stroke = "blue"),
        ticks = list(stroke = "blue", strokeWidth = 2),
        labels = list(angle = 270, align = "left", fontSize = 10))) %>%
      add_axis("y", title = "% Obese Children, 2011") %>% 
      bind_shiny("correlated")
    
    output$tbl <- DT::renderDataTable(
      plot_data, options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  
  
  # create info for left hand info pane
  infoFunc <- function(fips) {
    county_fips <- fips
    county_data <- ""
    if (county_fips() != "") {
      county_data <- vamap[vamap$COUNTY_PAD==substr(county_fips(),1,3),]
      county_data <- as.data.frame(county_data[1,])
    }
    else {
      county_data <- vamap[vamap$COUNTY_PAD=="001",]
      county_data <- as.data.frame(county_data[1,])
    }
    renderText({ print(
      paste0("<table>",
             "<tr><td><b>County</b></td><td width='100px'>",county_data[,c("NAME")],"</td></tr>",
             "<tr><td><b>Population 2015</b></td><td>",county_data[,c("POPESTIMATE2015")],"</td></tr>",
             "<tr><td><b>Land Area</b></td><td>",paste(round(county_data[,c("ALAND")]*3.86102e-7, 2), "sq mi" ),"</td></tr>",
             "<tr><td><b>Population Density, 2015</b></td><td>",round(county_data[,c("POPESTIMATE2015")]/(county_data[,c("ALAND")]*3.86102e-7), 2),"</td></tr>",
             if (as.integer(input$select) == 1) {
               paste0("<tr><td><b>Pop % low store access, 2010:</b></td><td>",round(county_data[,c("PCT_LACCESS_POP10")], 2),"%</td></tr>",
                      "<tr><td><b>Low Income % low store access, 2010:</b></td><td>",round(county_data[,c("PCT_LACCESS_LOWI10")], 2),"%</td></tr>",
                      "<tr><td><b>Children % low store access, 2010:</b></td><td>",round(county_data[,c("PCT_LACCESS_CHILD10")], 2),"%</td></tr>",
                      "<tr><td><b>Seniors % low store access, 2010:</b></td><td>",round(county_data[,c("PCT_LACCESS_SENIORS10")], 2),"%</td></tr>",
                      "<tr><td><b>Households % no car, low store access, 2010:</b></td><td>",round(county_data[,c("PCT_LACCESS_HHNV10")], 2),"%</td></tr>")
             },
             if (as.integer(input$select) == 2) {
               paste0("<tr><td><b>Students % eligible reduced lunch, 2010:</b></td><td>",round(county_data[,c("PCT_REDUCED_LUNCH10")], 2),"%</td></tr>",
                      "<tr><td><b>Students % eligible free lunch, 2010:</b></td><td>",round(county_data[,c("PCT_FREE_LUNCH10")], 2),"%</td></tr>")
             },
             if (as.integer(input$select) == 3) {
               paste0("<tr><td><b>Grocery stores/1,000 pop, 2012:</b></td><td>",round(county_data[,c("GROCPTH12")], 2),"</td></tr>",
                      "<tr><td><b>Convenience stores/1,000 pop, 2012:</b></td><td>",round(county_data[,c("CONVSPTH12")], 2),"</td></tr>")
             },
             if (as.integer(input$select) == 4) {
               paste0("<tr><td><b>Teen Births Rate/100,000 pop, 2016:</b></td><td>",round(county_data[,c("SE_NV008_001")], 2),"</td></tr>",
                      "<tr><td><b>Chlamydia Cases/100,000 pop, 2016:</b></td><td>",round(county_data[,c("SE_NV008_002")], 2),"</td></tr>",
                      "<tr><td><b>HIV Prevalence/100,000 pop, 2016:</b></td><td>",round(county_data[,c("SE_NV008_003")], 2),"</td></tr>")
             },
             "</table>")
    )})
  }
  
  # change things on map_shape_click
  clickFunc <- function(data, location, session) {
    countyInfo <- infoFunc(renderText({ print(data$x_) }))
    output$mymap_shape_click <- countyInfo
  }
  
  # Observer to update map with dots
  observeEvent(input$showSnap, {
    vaSNAPSpopup <- paste0(snapRetail$Store_Name, ", ", snapRetail$Address, ", ", snapRetail$City)
    if (input$showSnap == TRUE) {
      leafletProxy('mymap', data = snapRetail) %>%
        addCircleMarkers(radius = 2, color = "black", fillColor = "black", popup = vaSNAPSpopup, clusterOptions = markerClusterOptions())
    }
    else {
      leafletProxy('mymap', data = snapRetail) %>%
        clearMarkers()
    }
  })
}

shinyApp(ui = ui, server = server)