
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
pacf_met <- readRDS("./data/metrics_pacf.rds")


# pre-process the data for plotting ----------------------------------------



all_stations <- rbind(ind_station, atln_station, pacf_station)
all_metrcis <- rbind(ind_met, atln_met, pacf_met)

lat_lon <- all_stations[, .(lat, lon, sname, qrn)]

met_latlon <- all_metrcis[lat_lon, on = 'sname']
met_latlon <- met_latlon[complete.cases(met_latlon), ]

met_latlon <- setnames(met_latlon, c("bias", "rmse", "mae"), c("BIAS", "RMSE", "MAE"))



levels(met_latlon$ocn)

met_latlon[ocn == "pacf" & lon < 0, ocn := factor("east_pacf")]
met_latlon[ocn == "pacf", ocn := factor("west_pacf")]

# plot the spatial plot of variuos metrices -------------------------------

# categeoriacal -----------------------------------------------------------


cat_met <- met_latlon[imrg_run == 'imrg_f', .(lat, lon, POD, FAR, CSI, ocn)]
plot_cat_met <- melt(cat_met, c("lat", "lon", "ocn"))


cat_df2 <- split(plot_cat_met, f = plot_cat_met$ocn)

### plot

ind2 <- ggplot(cat_df2$ind)+
  geom_point(aes(lon, lat, color = value), size = 2.0) +
  facet_wrap(~variable) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_gradientn(name = "value", breaks =  c(0.1, 0.3, 0.5, 0.7, 0.9),
                        colours=c("green","orange", "red", "blue"), limits = c(0.07, 0.98)) +
  theme_small + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'Black'), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "none")


atln2 <- ggplot(cat_df2$atln)+
  geom_point(aes(lon, lat, color = value), size = 2.0) +
  facet_wrap(~variable) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + 
  scale_color_gradientn(name = "value", breaks =  c(0.1, 0.3, 0.5, 0.7, 0.9),
                        colours=c("green","orange", "red", "blue"), limits = c(0.07, 0.98)) +
  theme_small + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "none") 


east2 <- ggplot(cat_df2$east_pacf)+
  geom_point(aes(lon, lat, color = value), size = 2.0) +
  facet_wrap(~variable) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) +  
  scale_color_gradientn(name = "value", breaks =  c(0.1, 0.3, 0.5, 0.7, 0.9),
                        colours=c("green","orange", "red", "blue"), limits = c(0.07, 0.98)) +
  theme_small + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "none") + 
  theme(axis.text.x=element_text(color=c("black","transparent","black","transparent",
                                         "black","transparent","black","transparent","black")))


west2 <- ggplot(cat_df2$west_pacf)+
  geom_point(aes(lon, lat, color = value), size = 2.0) +
  facet_wrap(~variable) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) +  
  scale_color_gradientn(name = "value", breaks =  c(0.1, 0.3, 0.5, 0.7, 0.9),
                        colours=c("green","orange", "red", "blue"), limits = c(0.07, 0.98)) +
  theme_small + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "bottom", legend.key.width = unit(1.5, "cm"), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.title = element_blank()) + 
  guides(colour = guide_coloursteps(show.limits = FALSE))


g2 <- ggplotGrob(ind2)
g3 <- ggplotGrob(atln2)
g4 <- ggplotGrob(east2)
g5 <- ggplotGrob(west2)
g <- rbind(g2, g3, g4,g5, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths, g4$widths, g5$widths)
grid.newpage()
grid.draw(g)


# ggsave('g2.png', plot = g, width=8.6, height = 8.6, units = "in", dpi = 600)
# ggsave('g3.png', plot = g, width=9.6, height = 8.6, units = "in", dpi = 600)
ggsave('results/paper_fig/POD_FAR_CSI_final2.png', plot = g, width=7.3, height = 9.6, 
       units = "in", dpi = 600)



# volumetric -----------------------------------------------------------


vol_met <- met_latlon[imrg_run == 'imrg_f', .(lat, lon, BIAS, RMSE, MAE, ocn)]
plot_vol_met <- melt(vol_met, c("lat", "lon", "ocn"))

vol_df <- split(plot_vol_met, f = plot_vol_met$variable)


### Bias

ind_bias <- ggplot(vol_df$BIAS)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_gradientn(name = "Bias", breaks =  c(-3, 0, 3, 6, 9, 12),
                        colours=c("green","orange", "red", "blue"), limits = c(0-.3, 13)) +
  theme_small + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x=element_blank(), 
        legend.position = "none")

atln_bias <- ind_bias + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + theme(axis.text.y=element_blank())
east_bias <- atln_bias + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) #+ 
  # theme(axis.text.x=element_text(color=c("black","transparent","black","transparent",
  #                                        "black","transparent","black","transparent","black")))

west_bias <- atln_bias %+% vol_df$BIAS + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm")) + 
  #       legend.key.height = unit(0.4, 'cm'), 
  #       legend.title = element_blank()) + 
  guides(colour = guide_coloursteps(show.limits = FALSE))


bias <- ggarrange(ind_bias, atln_bias, east_bias, west_bias, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.42))


### RMSE

ind_rmse <- ggplot(vol_df$RMSE)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_gradientn(name = "RMSE", breaks =  c(0, 5, 10, 15, 20, 25),
                        colours=c("green","orange", "red", "blue"), limits = c(0.5, 31.5)) +
  theme_small + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x=element_blank(), 
        legend.position = "none")


atln_rmse <- ind_rmse + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + theme(axis.text.y=element_blank())

east_rmse <- atln_rmse + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) #+ 
  # theme(axis.text.x=element_text(color=c("black","transparent","black","transparent",
  #                                        "black","transparent","black","transparent","black")))

west_rmse <- atln_rmse %+% vol_df$rmse + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm")) +  
  #       legend.key.height = unit(0.4, 'cm'), 
  #       legend.title = element_blank()) + 
  guides(colour = guide_coloursteps(show.limits = FALSE))


rmse <- ggarrange(ind_rmse, atln_rmse, east_rmse, west_rmse, ncol = 4, align = "hv", widths = c(1.27, 1, 1, 1.42))



### MAE

ind_mae <- ggplot(vol_df$MAE)+
  geom_point(aes(lon, lat, color = value), size = 1.5) +
  #facet_wrap(~ocn, ncol = 1) + 
  geom_sf(data = shp, fill="#979797", color="white") + 
  coord_sf(ylim = c(-20, 20), xlim = c(55, 99)) + 
  scale_color_gradientn(name = "MAE", breaks =  c(0, 3, 6, 9, 12),
                        colours=c("green","orange", "red", "blue"), limits = c(0.1, 13.1)) +
  theme_small + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "none")


atln_mae <- ind_mae + coord_sf(ylim = c(-20, 20), xlim = c(-45, 0)) + theme(axis.text.y=element_blank())

east_mae <- atln_mae + coord_sf(ylim = c(-20, 20), xlim = c(-168, -97)) + 
  theme(axis.text.x=element_text(color=c("black","transparent","black","transparent",
                                         "black","transparent","black","transparent","black")))

west_mae <- atln_mae %+% vol_df$mae + coord_sf(ylim = c(-20, 20), xlim = c(135, 180)) + 
  theme(legend.position = "right", legend.key.width = unit(0.5, "cm")) +  
  #       legend.key.height = unit(0.4, 'cm'), 
  #       legend.title = element_blank()) + 
  guides(colour = guide_coloursteps(show.limits = FALSE))


mae <- ggarrange(ind_mae, atln_mae, east_mae, west_mae, ncol = 4, align = "hv", widths = c(1.26, 1, 1, 1.42))


ggarrange(bias, rmse, mae, nrow = 3, align = "hv", heights = c(1, 1,  1.3))


ggsave("results/paper_fig/BIAS_RMSE_MAE.png",
       width = 7.6, height = 5.3, units = "in", dpi = 600)

################################################################################################
