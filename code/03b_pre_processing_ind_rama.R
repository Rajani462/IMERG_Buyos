source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

library(hrbrthemes)
library(mapview) 
library(sf)


# load datasets -----------------------------------------------------------

rama <- readRDS("./data/ind_rama_raw.rds")

# RAMA Indian_ocean -------------------------------------------------------

rama
summary(rama)

# add a new column 'sname' based on lat long

rama[, `:=`(sname =  gsub(" ", "", paste(lat, "_", lon)))]
rama_complete <- rama[complete.cases(rama), ]
summary(rama_complete)

# transform the precipitation rate from mm/hr to mm/day

rama_complete <- rama_complete[, .(lon, lat, date, rf = (rf * 24), qrn, srn, sname)]

ggplot(rama_complete, aes(date, rf)) + 
  geom_line()  + 
  facet_wrap(~sname)


rama_complete_uniq <- unique(rama_complete, by = c("lon", "lat"))

station_coords_sf <- st_as_sf(rama_complete_uniq, 
                              coords = c("lon", "lat"), 
                              crs = 4326)
mapview(station_coords_sf, map.types = 'Stamen.TerrainBackground')


station_time <- rama_complete[, .(start = min(year(date)), 
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

ggsave("results/figures/ind_rama_data_availab.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)

max_year <- 2020
min_year <- 2010

rama_stations <- station_time[start <= 2009 & end >= 2018]
rama_selected <- rama_complete[rama_stations, on = 'sname']


saveRDS(rama_complete, "./data/ind_rama.rds")
saveRDS(rama_selected, "./data/ind_rama_2009_18.rds")
saveRDS(rama_complete_uniq, "./data/ind_rama_stations.rds")

#####################################################################