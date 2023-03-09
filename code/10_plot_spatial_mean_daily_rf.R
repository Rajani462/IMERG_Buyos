source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

library(viridis)
library(rnaturalearth)
library(rgdal)

##########

#mean_2001_20 <- readRDS("./data/imerg_spat_mean_25ns_2001_20.rds")
mean_2001_20_ocn <- readRDS("./data/ocn_mean_2001_20_dt.rds")

ind_rama_imrg <- readRDS("./data/ind_rama_imrg.rds")
atln_pirata_imrg <- readRDS("./data/atln_pirata_imrg.rds")
pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")

###############

ind <- ind_rama_imrg[, .(buoys_rf = mean(ind_rama, na.rm = TRUE)), by = .(lat, lon, sname)]
atln <- atln_pirata_imrg[, .(buoys_rf = mean(atln_pirata, na.rm = TRUE)), by = .(lat, lon, sname)]
pacf <- pacf_tao_imrg[, .(buoys_rf = mean(pacf_tao, na.rm = TRUE)), by = .(lat, lon, sname)]
ind_atln_pacf <- rbind(ind, atln, pacf)
#rama_atln_pacf <- readRDS("./data/rama_atln_pacf_latlon.rds")

#############

name_shp <- readOGR("C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/ne_110m_admin_0_countries",
layer="ne_110m_admin_0_countries")

# name_shp2 <- readOGR("C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/ne_10m_ocean",
#                      layer="ne_10m_ocean")
# 
# mean_2001_20_df <- as.data.frame(mean_2001_20)
# 
# coordinates(mean_2001_20_df) <- ~ lon + lat
# proj4string(mean_2001_20_df) <- proj4string(name_shp2)

#absh <- SpatialPolygonsDataFrame(cz_union)

# memory.limit()
# memory.limit(size=90000)
# 
# sf_type <- st_intersects(mean_2001_20_df, name_shp2)
# 
# ocn_mean_2001_20 <- mean_2001_20_df[!is.na(over(mean_2001_20_df, as(name_shp2, "SpatialPolygons"))), ]


ggplot(mean_2001_20_ocn) + 
#ggplot(mean_2001_20) +
geom_tile(aes(lon, lat, fill = mean_rf), alpha = 0.7) +
#coord_fixed(ratio = 1) +
scale_fill_viridis(direction = -1) +
coord_sf(ylim = c(-25, 25), expand = c(0, 0)) +
labs(x = "", y = "", fill = "IMERG (mm/day)") +
geom_polygon(data = name_shp,
aes(x = long, y = lat, group = group), fill="white", color="black") +
#scale_color_gradient(low = "gray", high = "blue") +
#geom_point(data = rama_atln_pacf, aes(lon, lat), colour = "red", size = 1) +
geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), shape = 21, size = 2.0) +
geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), color = "black", shape = 21, size = 2.5) +
  
geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), shape = 19, size = 1.5) + 
  
  scale_color_viridis(direction = -1) + 
#scale_color_gradientn(colours = c("grey","orange", "red", "black"), limits = c(0, 11)) +
#scale_color_gradient(low = "gray", high = "red") +
# guides(limits = c(1,4), fill = guide_colorbar(reverse = TRUE)) +
# scale_fill_gradient(limits = c(1,4), breaks=c(1,2,3,4))
labs(color ="Buoys (mm/day)") +
theme_generic +
theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"),
legend.key.height = unit(0.5, 'cm'))

ggsave("results/paper_fig/spatil_mean_dist.png",width = 8.2, height = 3.3, 
       units = "in", dpi = 600)


# revising based on reviwer_3 comment (use only a single color bar)

ggplot(mean_2001_20_ocn) + 
  geom_tile(aes(lon, lat, fill = mean_rf)) + 
  scale_fill_gradientn(colours = c("yellow", "green", "blue")) +
  coord_sf(ylim = c(-25, 25), expand = c(0, 0)) +
  labs(x = "", y = "", fill = "IMERG (mm/day)") +
  geom_polygon(data = name_shp,
               aes(x = long, y = lat, group = group), fill="white", color="black") + 
  labs(color ="Buoys (mm/day)") +
  theme_generic +
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"),
        legend.key.height = unit(0.5, 'cm')) + 
  geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), shape = 21, size = 2.0) +
  geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), color = "black", shape = 21, size = 2.5) +
  geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), shape = 19, size = 1.5) + 
  scale_colour_gradientn(colours = c("yellow", "green")) + 
labs(color ="Precipitation (mm/day)") +
  theme_generic +
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"),
        legend.key.height = unit(0.5, 'cm'))

###################################################################################

