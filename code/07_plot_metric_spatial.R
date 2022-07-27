
source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

library(viridis)
library(rnaturalearth)
library(rgdal)
library(gridExtra)

library(raster)
library(cowplot)

# read the datasets -------------------------------------------------------

name_shp <- readOGR("C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/ne_110m_admin_0_countries", 
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

# plot the spatial plot of variuos metrices -------------------------------

# categeoriacal -----------------------------------------------------------


cat_met <- met_latlon[imrg_run == 'imrg_f', .(lat, lon, POD, FAR, CSI)]
plot_cat_met <- melt(cat_met, c("lat", "lon"))


ggplot(plot_cat_met)+
  geom_point(aes(lon, lat, color = value), size = 1.1) +
  coord_sf( ylim = c(-25, 25)) +
  facet_grid(variable~.,) + 
  labs(x = "Longitude", y = "Latitude", fill = "mm/day") + 
  geom_polygon(data = name_shp, 
               aes(x = long, y = lat, group = group), fill="#979797", color="white") + 
  scale_color_gradient(low = "blue", high = "orange") + 
  theme_small


ggplot(plot_cat_met)+
  geom_point(aes(lon, lat, color = value), size = 0.9) +
  coord_sf( ylim = c(-25, 25)) + 
  # coord_sf(ylim = c(-25, 25), expand = FALSE) + 
  facet_wrap(~variable, ncol = 1) + 
  geom_polygon(data = name_shp, 
               aes(x = long, y = lat, group = group), fill="#979797", color="white") + 
  scale_color_gradientn(name = "value", breaks =  c(0.1, 0.3, 0.5, 0.7, 0.9),
                       colours=c("green","orange", "red", "blue"), limits = c(0.07, 0.98)) +
  # scale_color_gradientn(name = "value" ,colours = rainbow(5)) + 
  #theme_small + 
  theme_generic + # for presentation 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'Black'), 
        strip.text.x = element_text(size = 14)) + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) + 
  theme(legend.position = "bottom", legend.key.width = unit(2.3, "cm"), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.title = element_blank()) + 
  guides(colour = guide_coloursteps(show.limits = TRUE))


ggsave("results/paper_fig/POD_FAR_CSI_final.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


# volumetric -----------------------------------------------------------


vol_met <- met_latlon[imrg_run == 'imrg_f', .(lat, lon, BIAS, RMSE, MAE)]
plot_vol_met <- melt(vol_met, c("lat", "lon"))

vol_df <- split(plot_vol_met, f = plot_vol_met$variable)


plt_bias <- ggplot(vol_df$BIAS)+ 
  geom_point(aes(lon, lat, color = value), size = 0.9) +
  coord_fixed(ratio = 1) + 
  coord_sf( ylim = c(-25, 25)) +
  facet_wrap(~variable, ncol = 1) + 
  labs(x = "Longitude", y = "Latitude", fill = "mm/day") + 
  geom_polygon(data = name_shp, 
               aes(x = long, y = lat, group = group), fill="#979797", color="white") + 
  #scale_color_gradient("", low = "blue", high = "orange") + 
  #theme_small + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'Black'), 
        strip.text.x = element_text(size = 14)) +
  theme(legend.position = "right", legend.direction = "vertical") +  # for presentation
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.title = element_blank())


 p1 <- plt_bias + scale_colour_stepsn(colours = c("green","orange", "red", "blue"),
                         limits = c(-0.3, 13.1),
                         guide = guide_coloursteps(even.steps = TRUE,
                                                   show.limits = TRUE),
                         breaks = c(0, 2, 4, 8, 10))

# # do it for each "facet"
plt_rmse <- plt_bias %+% vol_df$RMSE
p2 <- plt_rmse + scale_colour_stepsn(colours = c("green","orange", "red", "blue"),
                               limits = c(0.6, 31.5),
                               guide = guide_coloursteps(even.steps = TRUE,
                                                         show.limits = TRUE),
                               breaks = c(3, 6, 10, 15, 20))

plt_mae <- plt_bias %+% vol_df$MAE
p3 <- plt_mae + scale_colour_stepsn(colours = c("green","orange", "red", "blue"),
                                     limits = c(0.1, 13.1),
                                     guide = guide_coloursteps(even.steps = TRUE,
                                                               show.limits = TRUE),
                                     breaks = c(1, 2, 4, 6, 8))



plot_grid(
  p1 + theme(legend.justification = c(0,1)), 
  p2 + theme(legend.justification = c(0,1)),
  p3 + theme(legend.justification = c(0,1)),
  align = "v", ncol = 1)

ggsave("results/paper_fig/volmet_final.png",
       width = 7.6, height = 5.3, units = "in", dpi = 600)

################################################################################################
