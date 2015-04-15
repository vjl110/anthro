setwd("~/GitHub/anthro/womenmob/data")

trvl <- read.csv("womMOB_event_verbose.csv")
	trvl$location <- tolower(trvl$location)
rep <- read.csv("repro.csv")
coord <- read.csv("res_coord.csv")
	coord$location <- tolower(coord$location)
inf <- read.csv("tweinfo.csv")
	inf$location <- tolower(inf$location)
	inf <- merge(inf, coord, by = "location", all.x = T)[c(2, 9:10)]


d <- merge(trvl, rep, by = "ID", all.x = T)
	d <- merge(d, coord, by = "location", all.x = T)
		d <- d[c(1:2, 26, 33:34)]
			d <- merge(d, inf, by = "ID", all.x = T)
d <- na.omit(d)
# Haversine formula
  R <- 6371 # Earth mean radius [km]
  delta.long <- (d$lon.y - d$lon.x)
  delta.lat <- (d$lat.y - d$lat.x)
  a <- sin(delta.lat/2)^2 + cos(d$lat.x) * cos(d$lat.y) * sin(delta.long/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d$dist = (R * c)/10

