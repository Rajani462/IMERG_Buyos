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


# revising based on reviwer_3 comment (use only a single color bar)

ggplot(mean_2001_20_ocn) + 
  geom_tile(aes(lon, lat, fill = mean_rf)) + 
  #scale_fill_viridis(direction = -1, limits = c(0, 16), breaks = c(5, 10, 15)) + 
  scale_fill_binned(type = "viridis", direction = -1, limits = c(0, 25), breaks = c(2, 4, 6, 8, 10, 12), show.limits = TRUE) +
  # scale_fill_viridis_c(direction = -1, limits = c(0, 25), oob = scales::squish) +
  # geom_text(aes(label = round(mean_rf, 2)), size = 2) + 
  #scale_fill_gradientn(colours = c("yellow", "green", "blue"), breaks = c(5, 10, 15)) +
  coord_sf(ylim = c(-25, 25), expand = c(0, 0)) +
  labs(x = "", y = "", fill = "Precipitation (mm/day)") +
  geom_polygon(data = name_shp,
               aes(x = long, y = lat, group = group), fill="white", color="black") + 
  labs(color ="Buoys (mm/day)") +
  theme_generic +
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"),
        legend.key.height = unit(0.5, 'cm')) + 
  geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), shape = 21, size = 0.8, show.legend = FALSE) +
  geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), color = "black", shape = 21, size = 0.9, stroke = 0.8, show.legend = FALSE) +
  geom_point(data = ind_atln_pacf, aes(lon, lat, color = buoys_rf), shape = 19, size = 0.8, show.legend = FALSE) + 
  #scale_colour_gradientn(colours = c("yellow", "green")) + 
  scale_color_viridis(direction = -1, begin = 0.43, end = 1.0) + 
  labs(color ="Precipi (mm/day)") + 
  theme_generic +
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"),
        legend.key.height = unit(0.5, 'cm'))

ggsave("results/paper_fig/spatil_mean_dist.png", width = 8.9, height = 3.3, 
       units = "in", dpi = 600)
###################################################################################

