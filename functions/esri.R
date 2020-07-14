# Read Features from an ESRI Geodatabase ----------------------------------------------

require(rgdal)

# The input file geodatabase
fgdb <- "data/CENSUS/ACS_2015_5YR_BG_51_VIRGINIA.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="X02_RACE")

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)


res <- readOGR(dsn = "data/CENSUS/ACS_2015_5YR_BG_51_VIRGINIA.gdb", layer = "X02_RACE", dropNULLGeometries = FALSE)


# Load module to get readOGR
require('rgdal');

# Load module to get read.dbf
require('foreign');

# goto the directory with the GDB stuff
setwd('~/git/dashboard/data/CENSUS/')

# Conversation to a shapefile container 
system("ogr2ogr -f 'ESRI SHAPEFILE' test ACS_2015_5YR_BG_51_VIRGINIA.gdb") 

# read the dbf
dbf <- read.dbf('test/ACS_2015_5YR_BG_51_VIRGINIA.dbf')

# read the geometries
geom <- readOGR('test/ACS_2015_5YR_BG_51_VIRGINIA.shp','ACS_2015_5YR_BG_51_VIRGINIA')
