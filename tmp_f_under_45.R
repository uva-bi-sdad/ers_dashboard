FU45 <- data.table::fread("data/F_under_45_central_dist.csv", check.names = T)
FU45[, GEOID := substr(GeoID, 10, 21)]

VABG <- readRDS("data/CENSUS/va_blockgroups_simplified_spdf.RDS")
VABG_dt <- data.table::setDT(VABG@data)

setkey(FU45, GEOID)
setkey(VABG_dt, GEOID)
new_dt <- FU45[VABG_dt][, !"GeoID"]

VABG@data <- new_dt
plot(VABG)

new_spdf <- tmaptools::append_data(VABG, FU45, key.shp = "GEOID", key.data = "GEOID")

new_new_spdf <- new_spdf[!is.na(new_spdf$Female.Under.45),]

sp::spplot(new_new_spdf, "Female.Under.45")


pal <- leaflet::colorNumeric(
  palette = "RdYlBu",
  domain = new_new_spdf$Female.Under.45)

binpal <- leaflet::colorBin("RdYlBu", new_new_spdf$Female.Under.45, 10, pretty = TRUE)

labels <- sprintf("County: %s, %s: %s", new_new_spdf$COUNTYFP, new_new_spdf$NAMELSAD, new_new_spdf$Female.Under.45)

leaflet::leaflet(new_new_spdf) %>%
  leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addPolygons(smoothFactor = 0.2,
                       weight = 1,
                       fillOpacity = 0.7,
                       color = "white",
                       dashArray = "3",
                       fillColor = ~binpal(Female.Under.45),
                       highlight = leaflet::highlightOptions(
                         weight = 5,
                         color = "#666",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = labels
  ) %>%
  leaflet::addLegend("topright", pal = binpal, values = ~Female.Under.45,
            title = "Female < 45 (2016)",
            opacity = 1
  )
