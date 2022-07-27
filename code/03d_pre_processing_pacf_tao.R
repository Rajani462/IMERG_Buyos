source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

library(hrbrthemes)
library(mapview) 
library(sf)


# load datasets -----------------------------------------------------------

pacf <- readRDS("./data/pacf_tao_raw.rds")

# TAO Pacific -------------------------------------------------------------

pacf_complete <- pacf[complete.cases(pacf), ]
summary(pacf_complete)

# transform the longitude from 0 - +360 to -180 to +180

pacf_complete <- pacf_complete[lon > 180, lon := (-360 + lon)]

pacf_complete[, `:=`(sname =  gsub(" ", "", paste(lat, "_", lon)))]

# transform the precipitation rate from mm/hr to mm

pacf_complete <- pacf_complete[, .(lon, lat, date, rf = (rf * 24), qrn, srn, sname)]

ggplot(pacf_complete, aes(date, rf)) + 
  geom_line()  + 
  facet_wrap(~sname)


station_time <- pacf_complete[, .(start = min(year(date)), 
                                  end = max(year(date))), 
                              by = sname]


station_coords_sf <- st_as_sf(pacf_complete, 
                              coords = c("lon", "lat"), 
                              crs = 4326)
mapview(station_coords_sf, map.types = 'Stamen.TerrainBackground')

pacf_complete_uniq <- unique(pacf_complete, by = c("lon", "lat"))


table(station_time$end)
table(station_time$start)


ggplot(station_time) + 
  geom_segment(aes(x = sname, xend = sname, y = start, yend = end), color="grey") + 
  geom_point( aes(x = sname, y = start)) + 
  geom_point( aes(x = sname, y = end)) + 
  labs(x = "Buoys", y = "Time") + 
  theme_small + 
  coord_flip()

ggsave("results/figures/pacf_tao_data_availab.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


max_year <- 2010
min_year <- 2000

pacf_selected <- station_time[start <= 2000 & end >= 2010]

saveRDS(pacf_complete, "./data/pacf_tao.rds")
saveRDS(pacf_selected, "./data/pacf_tao_2000_2010.rds")
saveRDS(pacf_complete_uniq, "./data/pacf_tao_stations.rds")


############################################################


# mapview of the all the stations across the three oceans -----------------


rama_atln <- rbind(rama_complete_uniq, atln_complete_uniq_trans)
rama_atln_pacf <- rbind(rama_atln, pacf_complete_uniq)

station_coords_sf <- st_as_sf(rama_atln_pacf, 
                              coords = c("lon", "lat"), 
                              crs = 4326)
mapview(station_coords_sf, map.types = 'Stamen.TerrainBackground')


saveRDS(rama_atln_pacf, "./data/rama_atln_pacf_latlon.rds")

# load library
library(rworldmap)
# get map
worldmap <- getMap(resolution = "coarse")
# plot world map
plot(worldmap, col = "lightgrey", 
     fill = T, border = "darkgray",
     xlim = c(-180, 180), ylim = c(-90, 90),
     bg = "aliceblue",
     asp = 1, wrap=c(-180,180))

# plot data on world map
plot(worldmap, xlim = c(-80, 160), ylim = c(-50, 100), 
     asp = 1, bg = "lightblue", col = "black", fill = T)
# add points
points(rama_atln_pacf$lon, rama_atln_pacf$lat, 
       col = "red", cex = 0.7, pch = 20)
############

library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

global_dist <- world_map <- ggplot(data = world) + 
  geom_sf(fill = "white") + 
  coord_sf(xlim = c(-180, 170), ylim = c(-40, 40)) + 
  geom_point(data = rama_atln_pacf, aes(lon, lat)) + 
  #scale_color_manual(values = mycol_continent5) + 
  theme_generic + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

