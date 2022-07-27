source('./source/libs.R')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")


# load the raw dataset --------------------------------------------------------

ocnrain_w_roger <- readRDS('./data/ocnrain_w_roger_pacf_raw.rds')
ocnrain_w_sonne_I <- readRDS('./data/ocnrain_w_sonne_I_pacf_raw.rds')
ocnrain_w_sonne_II <- readRDS('./data/ocnrain_w_sonne_II_pacf_raw.rds')


ocnrain_w_sonne_II <- ocnrain_w_sonne_II[complete.cases(ocnrain_w_sonne_II)]

name_shp <- readOGR("C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/ne_110m_admin_0_countries", 
                    layer="ne_110m_admin_0_countries")


# merge all

ocnrain_comb <- rbind(ocnrain_w_roger, ocnrain_w_sonne_I, ocnrain_w_sonne_II) 
ocnrain_comb[, daily_date := as.Date(date)]

ocnrain_comb_1day <- ocnrain_comb[between(lon, 145, 150) & between(lat, 1, 10) & daily_date >= "2016-11-13"]
#view(ocnrain_comb_1day)

ocnrain_comb_aggrrain <- ocnrain_comb_1day[, .(rf_daily = sum(rf)/15.5), by = daily_date]

ggplot(ocnrain_comb_1day) + 
  geom_point(aes(lon, lat)) + 
  #scale_fill_viridis(direction = -1, limits = c(0, 50)) + 
  #coord_sf(ylim = c(-25, 25), xlim = c(100, 180)) + 
  coord_sf(ylim = c(1, 4), xlim = c(145.5, 147.5)) + 
  labs(x = "", y = "", fill = "IMERG (mm/day)") + 
  geom_polygon(data = name_shp, 
               aes(x = long, y = lat, group = group), fill="white", color="black") + 
  geom_point(data = dat_plot, aes(lon, lat), color = "red", shape = 19, size = 1.5) + 
  #scale_color_gradientn(colours = c("grey","orange", "red", "black"), limits = c(0, 14)) +  
  #breaks = c(0, 3, 6, 9, 12, 14)) +
  #labs(color ="Buoys (mm/day)") + 
  theme_generic + 
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"), 
        legend.key.height = unit(0.5, 'cm'))


ggsave("results/paper_fig/ocnrain_buoys_pacf.png",
       width = 8.2, height = 5.3, units = "in", dpi = 600)


ggplot(ocnrain_w_sonne_II) + 
  geom_point(aes(lon, lat)) + 
  #scale_fill_viridis(direction = -1, limits = c(0, 50)) + 
  #coord_sf(ylim = c(-25, 25), xlim = c(100, 180)) + 
  coord_sf(ylim = c(-10, 10), xlim = c(140, 180)) + 
  labs(x = "", y = "", fill = "IMERG (mm/day)") + 
  geom_polygon(data = name_shp, 
               aes(x = long, y = lat, group = group), fill="white", color="black") + 
  geom_point(data = dat_plot, aes(lon, lat), color = "red", shape = 19, size = 1.5) + 
  #scale_color_gradientn(colours = c("grey","orange", "red", "black"), limits = c(0, 14)) +  
  #breaks = c(0, 3, 6, 9, 12, 14)) +
  #labs(color ="Buoys (mm/day)") + 
  theme_generic + 
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"), 
        legend.key.height = unit(0.5, 'cm'))

# Summary statistics ------------------------------------------------------

ggplot(ocnrain_w_roger, aes(rf)) +
  geom_histogram(fill = "#97B8C2") +
  #facet_wrap(~name, scales = 'free') +
  theme_bw()



# Aggregate ---------------------------------------------------------------

ocnrain_w_roger[, daily_date := as.Date(date)]
ocnrain_w_roger_daily <- ocnrain_w_roger[, .(daily_rf = sum(rf)), by = .(lon, lat, daily_date)]

dat_mean_subset <- dat_mean2[lon >= 100]

ggplot(dat_mean_subset) +
  geom_tile(aes(lon, lat, fill = value)) + 
  scale_fill_viridis(direction = -1, limits = c(0, 50)) + 
  coord_sf(ylim = c(-25, 25), xlim = c(100, 180)) + 
  labs(x = "", y = "", fill = "IMERG (mm/day)") + 
  geom_polygon(data = name_shp, 
               aes(x = long, y = lat, group = group), fill="white", color="black") + 
  geom_point(data = ocnrain_w_sonne_II, aes(lon, lat, color = rf), shape = 19, size = 1.5) + 
  scale_color_gradientn(colours = c("grey","orange", "red", "black"), limits = c(0, 14)) +  
                        #breaks = c(0, 3, 6, 9, 12, 14)) +
  #labs(color ="Buoys (mm/day)") + 
  theme_generic + 
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"), 
        legend.key.height = unit(0.5, 'cm'))



ocnrain_w_sonne_II_sub <- ocnrain_w_sonne_II[lon >= 145 & lon <= 150]
ocnrain_w_sonne_II_sub <- ocnrain_w_sonne_II_sub[lat >= 0 & lat <= 2]

ggplot(dat_plot) + 
  geom_point(aes(lon, lat)) + 
  #scale_fill_viridis(direction = -1, limits = c(0, 50)) + 
  coord_sf(ylim = c(-25, 25), xlim = c(100, 180)) + 
  labs(x = "", y = "", fill = "IMERG (mm/day)") + 
  geom_polygon(data = name_shp, 
               aes(x = long, y = lat, group = group), fill="white", color="black") + 
  geom_point(data = ocnrain_w_sonne_II_sub, aes(lon, lat, color = rf), shape = 19, size = 1.5) + 
  scale_color_gradientn(colours = c("grey","orange", "red", "black"), limits = c(0, 14)) +  
  #breaks = c(0, 3, 6, 9, 12, 14)) +
  #labs(color ="Buoys (mm/day)") + 
  theme_generic + 
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"), 
        legend.key.height = unit(0.5, 'cm'))


ggplot(dat_mean_subset) + 
  #ggplot(dat_mean_subset) +
  geom_tile(aes(lon, lat, fill = value)) + 
  scale_fill_viridis(direction = -1) + 
  coord_sf(ylim = c(0, 1), xlim = c(147, 148)) + 
  labs(x = "", y = "", fill = "IMERG (mm/day)") + 
  geom_polygon(data = name_shp, 
               aes(x = long, y = lat, group = group), fill="white", color="black") + 
  #geom_point(data = dat_plot, aes(lon, lat), shape = 19) + 
  geom_point(data = ocnrain_w_sonne_II_sub, aes(lon, lat, color = rf), shape = 19, size = 1.5) + 
  scale_color_gradientn(colours = c("grey","orange", "red", "black"), limits = c(0, 14)) +  
  #breaks = c(0, 3, 6, 9, 12, 14)) +
  #labs(color ="Buoys (mm/day)") + 
  theme_generic + 
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"), 
        legend.key.height = unit(0.5, 'cm'))



