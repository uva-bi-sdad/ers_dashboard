library("rgdal")

# connect to sdal database
con <- sdalr::con_db(dbname = "dashboard", user = "aschroed")

# get spatialdataframe from database
va_co <- rpostgis::pgGetGeom(con, query = "select * from us_counties where \"STATEFP\" = '51'")
plot(va_co)

# get dataset from database
va_hlth <- DBI::dbGetQuery(con, "select * from us_county_health_rankings_data_2017 where \"GEO_IDS__STATE\" = 'Virginia'")
head(va_hlth)

# append dataset to spatialdataframe
va_co_hlth <- tmaptools::append_data(va_co, va_hlth, key.shp = "GEOID", key.data = "ST_SUBST_FIPS")

# make spatial plot using a specific column from dataset (must be numeric) 
va_co_hlth$N_PREM_DTH__NUM_DTHS <- as.numeric(va_co_hlth$PREM_DTH__NUM_DTHS)
sp::spplot(va_co_hlth, zcol = "N_PREM_DTH__NUM_DTHS")