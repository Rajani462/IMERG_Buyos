
source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/functions.R')

library(viridis)
library(rnaturalearth)
library(rgdal)
library(gridExtra)

library(raster)
library(cowplot)
library(gtable)

# read the datasets -------------------------------------------------------

shp <- st_read(dsn = "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/ne_110m_admin_0_countries", 
               layer="ne_110m_admin_0_countries")

ind_station <- readRDS("./data/ind_rama_stations.rds")
atln_station <- readRDS("./data/atln_pirata_stations.rds")
pacf_station <- readRDS("./data/pacf_tao_stations.rds")

ind_met <- readRDS("./data/metrics_ind.rds")
atln_met <- readRDS("./data/metrics_atln.rds")
east_pacf_met <- readRDS("./data/metrics_east_pacf.rds")
west_pacf_met <- readRDS("./data/metrics_west_pacf.rds")

# pre-process the data for plotting ----------------------------------------


all_stations <- rbind(ind_station, atln_station, pacf_station)
all_metrcis <- rbind(ind_met, atln_met, east_pacf_met, west_pacf_met)

lat_lon <- all_stations[, .(lat, lon, sname, qrn)]

met_latlon <- all_metrcis[lat_lon, on = 'sname']
met_latlon <- met_latlon[complete.cases(met_latlon), ]

met_latlon <- setnames(met_latlon, c("bias", "rmse", "mae"), c("BIAS", "RMSE", "MAE"))



levels(met_latlon$ocn)


# plot the spatial plot of variuos metrices -------------------------------

# volumetric -----------------------------------------------------------


vol_met <- met_latlon[imrg_run == 'imrg_f', .(lat, lon, BIAS, RMSE, MAE, ocn)]

vol_met <- vol_met[, lapply(.SD, round, 2), by = .(lat, lon, ocn)]

plot_vol_met <- melt(vol_met, c("lat", "lon", "ocn"))

vol_df <- split(plot_vol_met, f = plot_vol_met$variable)

### Bias

pal <- c("yellow2", "yellowgreen", "seagreen", "royalblue2", "darkorange2", "maroon3", "red3")

scale_color_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "color", ...) {
  binned_scale("color", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)  
}



ind_bias <- ggplot(vol_df$BIAS)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_fermenter_custom(name = "Bias", pal, breaks =  c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.10, 4.680)) + 
  theme_small + 
  ggtitle("Indian") + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x=element_blank(), 
        legend.position = "none", plot.title = element_text(hjust = 0.4))

atln_bias <- ind_bias + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + ggtitle("Atlantic") + 
  theme(axis.text.y=element_blank())

east_bias <- atln_bias + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) + 
  ggtitle("East Pacific")

west_bias <- atln_bias %+% vol_df$BIAS + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm")) + 
  ggtitle("West Pacific")



bias <- ggarrange(ind_bias, atln_bias, east_bias, west_bias, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.42))

ggsave("results//BIAS.png",
       width = 7.6, height = 5.3, units = "in", dpi = 600)

# categeoriacal -----------------------------------------------------------


cat_met <- met_latlon[imrg_run == 'imrg_f', .(lat, lon, POD, FAR, CSI, ocn)]
plot_cat_met <- melt(cat_met, c("lat", "lon", "ocn"))

cat_df <- split(plot_cat_met, f = plot_cat_met$variable)


### pod

ind_pod <- ggplot(cat_df$POD)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_gradientn(name = "POD", breaks =  c(0.1, 0.3, 0.5, 0.7, 0.9),
                        colours=c("green","orange", "red", "blue"), limits = c(0, 1)) +
  #ggtitle("Indian") + 
  theme_small + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x=element_blank(), 
        legend.position = "none", plot.title = element_text(hjust = 0.5))

atln_pod <- ind_pod + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + #ggtitle("Atlantic") + 
  theme(axis.text.y=element_blank())
east_pod <- atln_pod + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) #+ ggtitle("East Pacific") 
# theme(axis.text.x=element_text(color=c("black","transparent","black","transparent",
#                                        "black","transparent","black","transparent","black")))

west_pod <- atln_pod %+% cat_df$POD + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  #ggtitle("West Pacific") + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm")) + 
  #       legend.key.height = unit(0.4, 'cm'), 
  #       legend.title = element_blank()) + 
  guides(colour = guide_coloursteps(show.limits = FALSE))


pod <- ggarrange(ind_pod, atln_pod, east_pod, west_pod, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.42))

ggsave("results//POD.png",
       width = 7.6, height = 5.3, units = "in", dpi = 600)

### FAR

ind_far <- ggplot(cat_df$FAR)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_gradientn(name = "FAR", breaks =  c(0.1, 0.3, 0.5, 0.7, 0.9),
                        colours=c("green","orange", "red", "blue"), limits = c(0, 1)) +
  theme_small + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x=element_blank(), 
        legend.position = "none")

atln_far <- ind_far + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + 
  theme(axis.text.y=element_blank())
east_far <- atln_far + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) 
# theme(axis.text.x=element_text(color=c("black","transparent","black","transparent",
#                                        "black","transparent","black","transparent","black")))

west_far <- atln_far %+% cat_df$far + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm")) + 
  #       legend.key.height = unit(0.4, 'cm'), 
  #       legend.title = element_blank()) + 
  guides(colour = guide_coloursteps(show.limits = FALSE))


far <- ggarrange(ind_far, atln_far, east_far, west_far, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.42))

ggsave("results//FAR.png",
       width = 7.6, height = 5.3, units = "in", dpi = 600)

### CSI

ind_csi <- ggplot(cat_df$CSI)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_gradientn(name = "CSI", breaks =  c(0.1, 0.3, 0.5, 0.7, 0.9),
                        colours=c("green","orange", "red", "blue"), limits = c(0, 1)) +
  theme_small + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        #axis.text.x=element_blank(), 
        legend.position = "none")

atln_csi <- ind_csi + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + 
  theme(axis.text.y=element_blank())

east_csi <- atln_csi + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) + 
  theme(axis.text.x=element_text(color=c("transparent","black","transparent","black",
                                         "transparent","black","transparent","black","transparent")))

west_csi <- atln_csi %+% cat_df$CSI + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm")) + 
  #       legend.key.height = unit(0.4, 'cm'), 
  #       legend.title = element_blank()) + 
  guides(colour = guide_coloursteps(show.limits = FALSE))


csi <- ggarrange(ind_csi, atln_csi, east_csi, west_csi, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.42))

ggsave("results//CSI.png",
       width = 7.6, height = 5.3, units = "in", dpi = 600)


ggarrange(bias, pod, far, csi, nrow = 4, align = "hv", heights = c(1.3, 1, 1, 1.3))

ggsave("results//POD_FAR_CSI_BIAS.png",
       width = 7.6, height = 6.3, units = "in", dpi = 600)





################################################################################################

