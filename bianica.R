my_spdf <- readRDS(file = "~/git/dashboard/data/houston_pts_orig_spdf.RDS")
my_spdf <- readRDS(file = "~/git/dashboard/data/houston_pts_spdf.RDS")

my_data <- as.data.frame(my_spdf@data)

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('blue','yellow','red'), bias = 1, alpha = TRUE)

#This adds a column of color values
# based on the y values
my_data$Col <- rbPal(10)[as.numeric(cut(my_data$exposure_ppb,breaks = 8))]
my_spdf@data <- my_data

plot(geoZips)
plot(my_spdf, pch = 15, cex = .3, col = scales::alpha(my_spdf@data$Col, .2), add = TRUE)







# ORIGINAL RESULTS

tmp_exp_hour <- data.table::setDT(readRDS(file = "~/git/dashboard/data/tmp_exp_hour.RDS"))
persons_fix <- data.table::setDT(readRDS(file = "~/git/dashboard/data/persons_fix.RDS"))
persons_fix[, pid := as.integer(pid)]
data.table::setkey(tmp_exp_hour, PID)
data.table::setkey(persons_fix, pid)
join <- persons_fix[tmp_exp_hour, nomatch=0]
tmp_exp_hour2 <- join[, .(pid, hid, homex, homey, home_zipcode, homeloc, pexposure, exposure_ppb = pexposure/(24*60))]
exp_pid <- tmp_exp_hour2

# get exposure for each home address
exp<-dplyr::group_by(exp_pid,home_zipcode,homex,homey) %>%
  summarise(exposure_ppb=mean(exposure_ppb))

# subset for zips in harris county
harris_zips<-DBI::dbGetQuery(con, "select * from zips_harris_county")

exp_hh<-filter(exp,home_zipcode %in% harris_zips$zips)

houston_orig_coords <- as.data.frame(cbind(exp_hh$homex, exp_hh$homey))
houston_orig_data <- as.data.frame(exp_hh[, c("home_zipcode", "exposure_ppb")])
houston_pts_orig_spdf <- sp::SpatialPointsDataFrame(houston_orig_coords, houston_orig_data)
saveRDS(houston_pts_orig_spdf, "~/git/dashboard/data/houston_pts_orig_spdf.RDS")


geodat<-read_shape("~/sdal/projects/houston/Shapefiles/HarrisCounty.shp")
geoZips<-read_shape("~/sdal/projects/houston/Shapefiles/HarrisZips.shp")
