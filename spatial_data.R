# spatial_data.R
library(tmaptools)




# All U.S. Counties
#us_counties_spdf <- readRDS("data/us_counties.RDS")




# All County Level Data
va_counties_spdf <- readRDS(paste0(getwd(),"/data/va_counties_spdf.RDS"))

# Virginia Snap Retailer Locations
snap_retail_spdf <- readRDS(paste0(getwd(),"/data/snap_retail_spdf.RDS"))

# Virginia Grocery Stores
grocery_stores_spdf <- readRDS(paste0(getwd(),"/data/grocery_stores_spdf.RDS"))

# Virginia Places of Worship
va_places_of_worship_spdf <- readRDS(paste0(getwd(),"/data/va_places_of_worship_spdf.RDS"))

# Time Series Data
# tsd <- read.csv("/data/countyObesityAndTeenBirths20112016.csv", stringsAsFactors = FALSE)
tsd <- readRDS(paste0(getwd(),"/data/timeSeriesData.RDS"))

# VA Counties for Time Series
vaCounties <- unique(tsd[,c("County")])

# VA Blockgroups with selected CENSUS data
va_blockgroups_spdf <- readRDS(paste0(getwd(),"/data/va_blockgroups_spdf.RDS"))

# arl_bg <- va_blockgroups_spdf[va_blockgroups_spdf$COUNTYFP=="013",]
# arl_bg$TOT_POP <- as.integer(arl_bg$TOT)
# 
# spplot(arl_bg, "TOT_POP" , main = "Main Title", sub = "Subtitle")
# 
# pal <- colorNumeric("YlGnBu", as.numeric(arl_bg@data[, c("TOT_POP")]))
# 
# leaflet(arl_bg) %>%
#   setView(lat = 38.878337,
#           lng = -77.100703,
#           zoom = 13) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(
#     stroke = TRUE,
#     weight = 2,
#     color = pal,
#     smoothFactor = 0.2,
#     fillOpacity = .1,
#     label= paste0("label")
#   )

# con <- con_db(dbname = "nass_quick_stats", user = "aschroed")
# gzf <- gzfile("/home/aschroed/sdal/projects/community_indicators/NASS_Quick_Stats/qs.demographics_20170808.txt.gz", "rt")
# df <- read.delim(gzf)
# DBI::dbWriteTable(con, "qs_demographics_20170808", df)

