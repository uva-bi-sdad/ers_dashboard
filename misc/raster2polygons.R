# library(sdalr)
# library(rpostgis)
# 
# con <- con_db('arlington', 'aschroed')
# 
# my_spdf<-pgGetGeom(con, name=c("shapefiles","arl_schools"), geom = "geom")
# plot(my_spdf)
# 
# library(raster)
# library(sp)
# library(rgdal)
# 
# gt <- raster("/home/aschroed/Export (1).tif")
# plot(gt)
# gt
# poly <- rasterToPolygons(gt)
