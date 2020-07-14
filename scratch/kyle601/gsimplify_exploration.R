## This is code to test whether using gSimplify() is effective in plotting leaflets faster.
## It appears that gsimplify() speeds things up by about 1.5 seconds, but returns
library(DBI)
library(maptools)
library(tigris)
library(rgeos)
# connect to sdal database
con <- sdalr::con_db(dbname = "dashboard", user = "kyle601")

# get spatialdataframe from database
va_co <- rpostgis::pgGetGeom(con, query = "select * from sp_census_counties_va")

# get some basic population data to put on the leaflet
library(acs)
acs::api.key.install(key = "9b8bb3e163b764eba6cda4fb1dccdd6d7adaabbc")
county_pop <- geo.make(state = "VA", county = "*")
Population_Total <- acs.fetch(geo=county_pop, endyear = 2015,  table.number="B01003")

## Attach data to the counties shapefile
counties_geo_data <- data.frame(paste0(Population_Total@geography$state,
                            Population_Total@geography$county),
                            Population_Total@estimate)
colnames(counties_geo_data) <- c("GEOID", "Population_Total")
va_merged <- geo_join(va_co, counties_geo_data, "GEOID", "GEOID")



### Unsimplified Leaflet
library(leaflet)

## Higlight from here until end of leaflet to calculate run time
ptm <- proc.time()
pal_pop <- colorNumeric("Blues", NULL, n = 6)

popup_pop <- paste0("Population: ", as.character(va_merged$Population_Total))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = va_merged,
              fillColor = ~pal_pop(va_merged$Population_Total), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_pop) %>%
  addLegend(pal = pal_pop, 
            values = va_merged$Population_Total, 
            position = "bottomright", 
            title = "Total Population")
print(proc.time()-ptm)



### Leaflet with rgeos::gSimplify()
## simplify the polygons
########## Adjust the "tol" value below to select how well polygons are preserved!
va_gsimp1 <- rgeos::gSimplify(va_co, tol = 100, topologyPreserve = TRUE)
## re-attach the data (gSimplify removes the data and returns a SpatialPolygons
## object instead of a SpatialPolygonsDataFrame)
va_gsimp_final <- SpatialPolygonsDataFrame(va_gsimp1, data = va_merged@data)

## Higlight from here until end of leaflet to calculate run time
ptm <- proc.time()

pal_pop <- colorNumeric("Blues", NULL, n = 6)

popup_pop <- paste0("Population: ", as.character(va_gsimp_final$Population_Total))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = va_gsimp_final,
              fillColor = ~pal_pop(va_gsimp_final$Population_Total), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_pop) %>%
  addLegend(pal = pal_pop, 
            values = va_gsimp_final$Population_Total, 
            position = "bottomright", 
            title = "Total Population")
print(proc.time()-ptm)









######### ms_simplify() is producing errors with the leaflet, I'd say we stick with gsimplify() for now
### Leaflet with rmapshaper::ms_simplify()
va_merged_ms_simplify <- rmapshaper::ms_simplify(va_co, keep=0.2)

pal_pop <- colorNumeric("Blues", NULL, n = 6)

popup_pop <- paste0("Population: ", as.character(va_merged_ms_simplify$Population_Total))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = va_merged_ms_simplify,
              fillColor = ~pal_pop(va_merged_ms_simplify$Population_Total), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_pop) %>%
  addLegend(pal = pal_pop, 
            values = va_merged_ms_simplify$Population_Total, 
            position = "bottomright", 
            title = "Total Population")


