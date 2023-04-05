# plot the spatial distribution of relative metrics

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
#met_latlon <- met_latlon[complete.cases(met_latlon), ]

#met_latlon <- met_latlon[complete.cases(met_latlon), ]

met_latlon <- met_latlon[, .(lat, lon, imrg_run, rbias, nrmse, nmae, ocn)]

met_latlon <- setnames(met_latlon, c("rbias", "nrmse", "nmae"), c("RBIAS", "NRMSE", "NMAE"))



levels(met_latlon$ocn)

# met_latlon[ocn == "pacf" & lon < 0, ocn := factor("east_pacf")]
# met_latlon[ocn == "pacf", ocn := factor("west_pacf")]

# plot the spatial plot of variuos metrices -------------------------------



# volumetric -----------------------------------------------------------


vol_met <- met_latlon[imrg_run == 'imrg_f', .(lat, lon, RBIAS, NRMSE, NMAE, ocn)]

vol_met <- vol_met[, lapply(.SD, round, 2), by = .(lat, lon, ocn)]

plot_vol_met <- melt(vol_met, c("lat", "lon", "ocn"))

vol_df <- split(plot_vol_met, f = plot_vol_met$variable)

### Bias

pal <- c("yellow2", "yellowgreen", "seagreen", "royalblue2", "darkorange2", "maroon3", "red3")

scale_color_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "color", ...) {
  binned_scale("color", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)  
}



ind_bias <- ggplot(vol_df$RBIAS)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_fermenter_custom(name = "RBIAS\n(mm/day)", pal, breaks =  c(0, 0.5, 1, 1.5, 2), limits = c(-0.5, 25.69)) + 
  theme_small + 
  ggtitle("Indian") + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x=element_blank(), 
        legend.position = "none", plot.title = element_text(hjust = 0.5))

atln_bias <- ind_bias + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + ggtitle("Atlantic") + 
  theme(axis.text.y=element_blank())

east_bias <- atln_bias + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) + 
  ggtitle("East Pacific")
  
west_bias <- atln_bias %+% vol_df$BIAS + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm")) + 
  ggtitle("West Pacific")
  


bias <- ggarrange(ind_bias, atln_bias, east_bias, west_bias, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.44))


### RMSE

ind_rmse <- ggplot(vol_df$NRMSE)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_fermenter_custom(name = "NRMSE\n(mm/day)", pal, breaks = c(3, 4, 5, 6, 7), limits = c(1.94, 133.56)) + 
  theme_small + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        #axis.text.x=element_blank(), 
        legend.position = "none")


atln_rmse <- ind_rmse + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + theme(axis.text.y=element_blank())

east_rmse <- atln_rmse + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) + 
  theme(axis.text.x=element_text(color=c("transparent","black","transparent","black",
                                         "transparent","black","transparent","black","transparent")))


west_rmse <- atln_rmse %+% vol_df$rmse + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm"))

rmse <- ggarrange(ind_rmse, atln_rmse, east_rmse, west_rmse, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.44))



ggarrange(bias, rmse, nrow = 2, align = "hv", heights = c(1.3, 1.2))

ggsave("results/supp_fig/supp_relative_BIAS_RMSE1.png",
       width = 8.2, height = 4.0, units = "in", dpi = 600)

################################################################




### MAE

ind_mae <- ggplot(vol_df$NMAE)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_fermenter_custom(name = "NMAE\n(mm/day)", pal, breaks = c(1, 1.5, 2, 2.5, 3), limits = c(0.78, 63.76)) + 
  theme_small + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        #axis.text.x=element_blank(), 
        legend.position = "none")


atln_mae <- ind_mae + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + theme(axis.text.y=element_blank())

east_mae <- atln_mae + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) + 
  theme(axis.text.x=element_text(color=c("transparent","black","transparent","black",
                                         "transparent","black","transparent","black","transparent")))

west_mae <- atln_mae %+% vol_df$mae + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm"))
#       legend.key.height = unit(0.4, 'cm'), 
#       legend.title = element_blank()) + 
#guides(colour = guide_coloursteps(show.limits = FALSE))


mae <- ggarrange(ind_mae, atln_mae, east_mae, west_mae, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.44))


ggarrange(bias, rmse, mae, nrow = 3, align = "hv", heights = c(1.3, 1, 1.2))


ggsave("results/supp_fig/supp_relative_BIAS_RMSE_MAE.png",
       width = 8.2, height = 6.5, units = "in", dpi = 600)


##########################################################################################





