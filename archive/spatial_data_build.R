# spatial_data.R

# # Read in the shapefile for US states and counties:
# usshapefile <- "data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp"
# 
# # Virginia shapefile:
# vafipscode <- "51"
# vageo <- usgeo[usgeo@data$STATEFP==vafipscode,]
# 
# # Food ACCESS data
# usdatafile <- "data/FoodEnvironment.xlsx"
# usdataACCESS <- rio::import(usdatafile, which = "ACCESS")
# vadataACCESS <- usdataACCESS[usdataACCESS$State=="VA" & usdataACCESS$FIPS < 51899,]
# vadataACCESS <- subset(vadataACCESS, !(vadataACCESS$FIPS %in% c("51515", "51560", "51780")))
# vadataACCESS$County_FIPS <- substr(vadataACCESS$FIPS, nchar(vadataACCESS$FIPS)-3+1, nchar(vadataACCESS$FIPS))
# vamap <- append_data(vageo, vadataACCESS, key.shp = "COUNTYFP", key.data = "County_FIPS")
# 
# # Food ASSISTANCE data
# usdatafile <- "data/FoodEnvironment.xlsx"
# usdataASSISTANCE <- rio::import(usdatafile, which = "ASSISTANCE")
# vadataASSISTANCE <- usdataASSISTANCE[usdataASSISTANCE$State=="VA" & usdataASSISTANCE$FIPS < 51899,]
# vadataASSISTANCE <- subset(vadataASSISTANCE, !(vadataASSISTANCE$FIPS %in% c("51515", "51560", "51780")))
# vadataASSISTANCE$County_FIPS <- substr(vadataASSISTANCE$FIPS, nchar(vadataASSISTANCE$FIPS)-3+1, nchar(vadataASSISTANCE$FIPS))
# vamap <- append_data(vamap, vadataASSISTANCE, key.shp = "COUNTYFP", key.data = "County_FIPS")
# 
# # Food STORES data
# usdatafile <- "data/FoodEnvironment.xlsx"
# usdataSTORES <- rio::import(usdatafile, which = "STORES")
# vadataSTORES <- usdataSTORES[usdataSTORES$State=="VA" & usdataSTORES$FIPS < 51899,]
# vadataSTORES <- subset(vadataSTORES, !(vadataSTORES$FIPS %in% c("51515", "51560", "51780")))
# vadataSTORES$County_FIPS <- substr(vadataSTORES$FIPS, nchar(vadataSTORES$FIPS)-3+1, nchar(vadataSTORES$FIPS))
# vamap <- append_data(vamap, vadataSTORES, key.shp = "COUNTYFP", key.data = "County_FIPS")
# 
# # Food HEALTH data
# usdatafile <- "data/FoodEnvironment.xlsx"
# usdataHEALTH <- rio::import(usdatafile, which = "HEALTH")
# vadataHEALTH <- usdataHEALTH[usdataHEALTH$State=="VA" & usdataHEALTH$FIPS < 51899,]
# vadataHEALTH <- subset(vadataHEALTH, !(vadataHEALTH$FIPS %in% c("51515", "51560", "51780")))
# vadataHEALTH$County_FIPS <- substr(vadataHEALTH$FIPS, nchar(vadataHEALTH$FIPS)-3+1, nchar(vadataHEALTH$FIPS))
# vamap <- append_data(vamap, vadataHEALTH, key.shp = "COUNTYFP", key.data = "County_FIPS")
# 
# # COUNTY HEALTH data
# vadatafile <- "data/county_health_2016/R11241936_SL050.csv"
# vadataCOUNTYHEALTH <- rio::import(vadatafile)
# vadataCOUNTYHEALTH <- subset(vadataCOUNTYHEALTH, !(vadataCOUNTYHEALTH$Geo_FIPS %in% c("51515", "51560", "51780")))
# vadataCOUNTYHEALTH$Geo_FIPS_Co <- substr(vadataCOUNTYHEALTH$Geo_FIPS, 3, 5)
# vamap <- append_data(vamap, vadataCOUNTYHEALTH, key.shp = "COUNTYFP", key.data = "Geo_FIPS_Co")
# 
# # Population Data
# uspop <- read.csv("data/CO-EST2015-alldata.csv")
# vapop <- uspop[uspop$STATE=="51" & uspop$COUNTY!="0",]
# vapop <- vapop[order(vapop$COUNTY),]
# vapop$COUNTY_PAD <- sprintf("%03d", vapop$COUNTY)
# vapop$COUNTYPAD <- sprintf("%03d", vapop$COUNTY)
# vapop <- vapop[,c("CENSUS2010POP","COUNTYPAD","COUNTY_PAD")]
# vamap <- append_data(vamap, vapop, key.shp = "COUNTYFP", key.data = "COUNTYPAD" )
# 
# # Education Data
# vaHSGrad <- readr::read_csv("data/countyGraduationRate2016.csv")
# vamap <- append_data(vamap, vaHSGrad, key.shp = "COUNTYFP", key.data = "County_FIPS")
# # Demographic Data
# vadataDEMOGRAPHICS <- readr::read_csv("data/CLD3Data.csv")
# vadataDEMOGRAPHICS <- subset(vadataDEMOGRAPHICS, !(vadataDEMOGRAPHICS$Id2 %in% c("51515", "51560", "51780")))
# vadataDEMOGRAPHICS$HouseholdSNAPPercent <- (vadataDEMOGRAPHICS$HouseholdSNAP/vadataDEMOGRAPHICS$TotalHouseholds)*100
# vadataDEMOGRAPHICS$County_FIPS <- substr(vadataDEMOGRAPHICS$Id2, 3, 5)
# vamap <- append_data(vamap, vadataDEMOGRAPHICS, key.shp = "COUNTYFP", key.data = "County_FIPS")
#
# saveRDS(vamap, "data/va_counties_spdf.RDS")
va_counties_spdf <- readRDS("data/va_counties_spdf.RDS")

# Virginia Snap Retailer Locations
# snap_retail <- read.csv("data/VASnapRetailers.csv")
# snap_retail_spdf <- SpatialPointsDataFrame(coords = snapRetail[,c("Longitude", "Latitude")], 
#                                            data = snapRetail[,c("Store_Name", "Address", "Address.Line..2", "City", "State", "Zip5", "Zip4", "County")])
# saveRDS(snap_retail_spdf, "data/snap_retail_spdf.RDS")
snap_retail_spdf <- readRDS("data/snap_retail_spdf.RDS")

# Virginia Grocery Stores
# grocery_stores <- read.csv("data/grocery_stores.csv")
# grocery_stores_spdf <- SpatialPointsDataFrame(coords = grocery_stores[,c("lon", "lat")],
#                                               data = grocery_stores[,c("name", "address", "url", "website", "locality", "county")])
# saveRDS(grocery_stores_spdf, "data/grocery_stores_spdf.RDS")
grocery_stores_spdf <- readRDS("data/grocery_stores_spdf.RDS")

# Virginia Places of Worship
# va_places_of_worship <- read.csv("data/va_places_of_worship.csv")
# va_places_of_worship_spdf <- SpatialPointsDataFrame(coords = grocery_stores[,c("lon", "lat")],
#                                                     data = grocery_stores[,c("name", "address", "url", "website", "locality", "county")])
# saveRDS(va_places_of_worship_spdf, "data/va_places_of_worship_spdf.RDS")
va_places_of_worship_spdf <- readRDS("data/va_places_of_worship_spdf.RDS")

# Time Series Data
tsd <- read.csv("data/countyObesityAndTeenBirths20112016.csv", stringsAsFactors = FALSE)
# VA Counties for Time Series
vaCounties <- unique(tsd[,c("County")])

# library(tmaptools)
# va_bg_shapefile <- "data/tl_2016_51_bg/tl_2016_51_bg.shp"
# va_blockgroups_spdf <- read_shape(file=va_bg_shapefile)
# saveRDS(va_blockgroups_spdf, "data/va_blockgroups_spdf.RDS)
va_blockgroups_spdf <- readRDS("data/va_blockgroups_spdf.RDS")
acs_15_5yr_B01002_age <- readRDS("data/ACS_15_5YR_B01002_with_ann.RDS")
va_blockgroups_spdf <- append_data(va_blockgroups_spdf, acs_15_5yr_B01002_age, "GEOID", "GEO.id2")

# arl_bg_geo <- va_bg_geo[va_bg_geo$COUNTYFP=="013",]

library(tmaptools)
arl_blockgroups <- va_blockgroups_spdf[va_blockgroups_spdf$COUNTYFP=="013",]
arl_blockgroups<-append_data(arl_blockgroups, acs_15_5yr_B01002_age, key.shp = "GEOID", key.data = "GEO.id2")

ACS_15_5YR_B01003_with_ann<-fread("data/CENSUS/ACS_15_5YR_B01003_with_ann.csv")
colnames(ACS_15_5YR_B01003_with_ann) <- census_column_rename(ACS_15_5YR_B01003_with_ann)
DEC_10_PL_P1_with_ann<-fread("data/CENSUS/DEC_10_PL_P1_with_ann.csv")
colnames(DEC_10_PL_P1_with_ann) <- census_column_rename(DEC_10_PL_P1_with_ann)
DEC_10_SF1_P12_with_ann<-fread("data/CENSUS/DEC_10_SF1_P12_with_ann.csv")
colnames(DEC_10_SF1_P12_with_ann) <- census_column_rename(DEC_10_SF1_P12_with_ann)
DEC_10_SF1_P13_with_ann<-fread("data/CENSUS/DEC_10_SF1_P13_with_ann.csv")
colnames(DEC_10_SF1_P13_with_ann) <- census_column_rename(DEC_10_SF1_P13_with_ann)


DEC_10_ACS_15_5YR <- tmaptools::append_data(va_blockgroups_spdf, DEC_10_PL_P1_with_ann, "GEOID", "ID2")
DEC_10_ACS_15_5YR <- tmaptools::append_data(DEC_10_ACS_15_5YR, DEC_10_SF1_P12_with_ann, "GEOID", "ID2")
DEC_10_ACS_15_5YR <- tmaptools::append_data(DEC_10_ACS_15_5YR, DEC_10_SF1_P13_with_ann, "GEOID", "ID2")
DEC_10_ACS_15_5YR <- tmaptools::append_data(DEC_10_ACS_15_5YR, ACS_15_5YR_B01003_with_ann, "GEOID", "ID2")
saveRDS(DEC_10_ACS_15_5YR, "data/va_blockgroups_spdf.RDS")

length(names(DEC_10_ACS_15_5YR))
length(unique(names(DEC_10_ACS_15_5YR)))
