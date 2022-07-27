source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

library(hrbrthemes)
library(mapview) 
library(sf)


# load datasets -----------------------------------------------------------

atln<-  readRDS("./data/atln_pirata_raw.rds")


# pirata_atlantic ---------------------------------------------------------

atln
atln_complete <- atln[complete.cases(atln), ]

# transform the longitude from 0 - +360 to -180 to +180
atln_complete <- atln_complete[lon > 180, lon := (-360 + lon)]

atln_complete[, `:=`(sname =  gsub(" ", "", paste(lat, "_", lon)))]

# transform the precipitation rate from mm/hr to mm
atln_complete <- atln_complete[, .(lon, lat, date, rf = (rf * 24), qrn, srn, sname)]

ggplot(atln_complete, aes(date, rf)) + 
  geom_line()  + 
  facet_wrap(~sname)

atln_complete_uniq <- unique(atln_complete, by = c("lon", "lat"))

station_coords_sf <- st_as_sf(atln_complete, 
                              coords = c("lon", "lat"), 
                              crs = 4326)
mapview(station_coords_sf, map.types = 'Stamen.TerrainBackground')


atln_complete_uniq <- unique(atln_complete, by = c("lat", "lon"))


station_time <- atln_complete[, .(start = min(year(date)), 
                               end = max(year(date))), 
                           by = sname]

table(station_time$end)
table(station_time$start)

ggplot(station_time) + 
  geom_segment(aes(x = sname, xend = sname, y = start, yend = end), color="grey") + 
  geom_point( aes(x = sname, y = start)) + 
  geom_point( aes(x = sname, y = end)) + 
  labs(x = "Buoys", y = "Time") + 
  theme_small + 
  coord_flip()

ggsave("results/figures/atln_pirata_data_availab.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)

max_year <- 2019
min_year <- 2000

atln_selected <- station_time[start <= 2005 & end >= 2019]

saveRDS(atln_complete, "./data/atln_pirata.rds")
saveRDS(atln_selected, "./data/atln_pirata_2005_19.rds")
saveRDS(atln_complete_uniq, "./data/atln_pirata_stations.rds")

###########################################################