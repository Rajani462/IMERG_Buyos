
source('./source/libs.R')
source('./source/functions.R')
source('./source/themes.R')
source('./source/palettes.R')
library(viridis)
library(ggpointdensity)

#### read the datasets


ind_rama_imrg <- readRDS("./data/ind_rama_imrg.rds")
atln_pirata_imrg <- readRDS("./data/atln_pirata_imrg.rds")
pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")


### preprocess

ind <- ind_rama_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, buyos = ind_rama, sname,
                         ocn = factor("Indian"))]

atln <- atln_pirata_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, buyos = atln_pirata,
                             sname, ocn = factor("Atlantic"))]

pacf <- pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, buyos = pacf_tao, sname, 
                          ocn = factor("pacf"))]

pacf$ocn <- with(pacf_tao_imrg, ifelse(lon < 0, "East Pacific", "West Pacific"))

ind_atln_pacf <- rbind(ind, atln, pacf)
setnames(ind_atln_pacf, c("imrg_e", "imrg_l", "imrg_f"), c("IMERG-E", "IMERG-L", "IMERG-F"))

ind_atln_pacf_mean <- ind_atln_pacf[, lapply(.SD, mean, na.rm=TRUE), by = .(date, ocn), 
                                    .SDcols=c("IMERG-E", "IMERG-L", "IMERG-F", "buyos")]

ind_alt_pacf_meanplot <- melt(ind_atln_pacf_mean, c("date", 'ocn', 'buyos'), value.name = "imrg_rf")

### stats for plots

stat_met <- ind_alt_pacf_meanplot[, .(N = .N, COR = round(cor(imrg_rf, buyos), 2), 
                              RMSE = round(sqrt(sum((imrg_rf - buyos)^2)/.N), 2), 
                              MAE = round(sum(abs(imrg_rf - buyos))/.N, 2)),
                              #Bias =  round(sum(imrg_rf - buyos)/.N, 2)), 
                              by = .(variable, ocn)] 

stat_met2 <- as_tibble(stat_met) %>% group_by(variable) %>%
  group_by(ocn) %>%
  mutate(lab = paste("N = ", N, "\nCOR = ", COR, "\nRMSE = ", RMSE, "\nMAE = ", MAE))


# plots -------------------------------------------------------------------


### only for hit days (precipitation >= 0.1) and imrg_final run

ggplot(ind_alt_pacf_meanplot[variable == "IMERG-F" & buyos >= 0.1 & imrg_rf >= 0.1], aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 50) + 
  labs(x = "Buyos (mm/day)", y = "IMERG (mm/day)") +  
  geom_abline(intercept = 0, slope = 1, col = "black") + 
  #scale_fill_continuous(type = "viridis") +  
  scale_fill_distiller(palette = "Spectral") + 
  facet_grid(~ocn) + 
  scale_x_log10()+scale_y_log10()+
  coord_cartesian(xlim=c(0.1, 220),ylim=c(0.1,220)) + 
  geom_label(data = stat_met2,
             aes(x = 95, y = 0.3, label = lab), label.padding = unit(0.1, "lines"), 
             label.size=0, size = 2.5) + 
  theme_very_small + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("results/paper_fig/scat_dens_hitdays_imrg_f.png",
       width = 9.9, height = 3.0, units = "in", dpi = 600)



#### revised based on reviewer_3 request (added qq-line) ------------------

threshold <- 0

ind <- ind_alt_pacf_meanplot[variable == "IMERG-F" & ocn == "Indian" & buyos >= threshold & imrg_rf >= threshold]
atln <- ind_alt_pacf_meanplot[variable == "IMERG-F" & ocn == "Atlantic" & buyos >= threshold & imrg_rf >= threshold]
east_pacf <- ind_alt_pacf_meanplot[variable == "IMERG-F" & ocn == "East Pacific" & buyos >= threshold & imrg_rf >= threshold]
west_pacf <- ind_alt_pacf_meanplot[variable == "IMERG-F" & ocn == "West Pacific" & buyos >= threshold & imrg_rf >= threshold]




qval <- seq(0.01, 1.0, 0.01)
qval <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.90, 0.93, 0.95, 0.96, 0.97, 0.98, 0.99, 1.0)

ind_buoy <- unname(round(quantile(ind$buyos, na.rm = T, probs = c(qval))))
ind_imrg <- unname(round(quantile(ind$imrg_rf, na.rm = T, probs = c(qval))))

dt_ind <- data.table(quantiles = c(qval), 
                     buoy =  c(ind_buoy),
                     imrg = c(ind_imrg), 
                     ocn = "Indian")

atln_buoy <- unname(round(quantile(atln$buyos, na.rm = T, probs = c(qval))))
atln_imrg <- unname(round(quantile(atln$imrg_rf, na.rm = T, probs = c(qval))))

dt_atln <- data.table(quantiles = c(qval), 
                      buoy =  c(atln_buoy),
                      imrg = c(atln_imrg), 
                      ocn = "Atlantic")

east_pacf_buoy <- unname(round(quantile(east_pacf$buyos, na.rm = T, probs = c(qval))))
east_pacf_imrg <- unname(round(quantile(east_pacf$imrg_rf, na.rm = T, probs = c(qval))))

dt_east_pacf <- data.table(quantiles = c(qval), 
                           buoy =  c(east_pacf_buoy),
                           imrg = c(east_pacf_imrg), 
                           ocn = "East Pacific")

west_pacf_buoy <- unname(round(quantile(west_pacf$buyos, na.rm = T, probs = c(qval))))
west_pacf_imrg <- unname(round(quantile(west_pacf$imrg_rf, na.rm = T, probs = c(qval))))

dt_west_pacf <- data.table(quantiles = c(qval), 
                           buoy =  c(west_pacf_buoy),
                           imrg = c(west_pacf_imrg), 
                           ocn = "West Pacific")

all_dat <- rbind(dt_ind, dt_atln, dt_east_pacf, dt_west_pacf)
all_dat$ocn <- factor(all_dat$ocn, levels = c("Indian", "Atlantic", "East Pacific", "West Pacific"))

plot_precip_days <- ind_alt_pacf_meanplot[variable == "IMERG-F" & buyos >= 0.1 & imrg_rf >= 0.1]
qq_dat <- plot_precip_days[, .(buyos = sort(buyos), imrg_rf = sort(imrg_rf)), by = 'ocn']

ggplot(plot_precip_days, aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 50) + 
  labs(x = "Buyos (mm/day)", y = "IMERG (mm/day)") +  
  geom_abline(intercept = 0, slope = 1, col = "black") + 
  #scale_fill_continuous(type = "viridis") +  
  scale_fill_distiller(palette = "Spectral") + 
  facet_grid(~ocn) + 
  scale_x_log10()+scale_y_log10()+
  coord_cartesian(xlim=c(0.1, 220),ylim=c(0.1,220)) + 
  geom_label(data = stat_met2,
             aes(x = 95, y = 0.25, label = lab), label.padding = unit(0.1, "lines"), 
             label.size=0, size = 2.5) + 
  theme_very_small + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  geom_line(aes(buyos, imrg_rf), qq_dat, col = "red", type = "dotted")

ggsave("./results/paper_fig/scat_dens_hitdays_imrg_f.png",
       width = 9.9, height = 3.0, units = "in", dpi = 600)

#without log scale
ggplot(plot_precip_days, aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 50) + 
  labs(x = "Buyos (mm/day)", y = "IMERG (mm/day)") +  
  geom_abline(intercept = 0, slope = 1, col = "black") + 
  #scale_fill_continuous(type = "viridis") +  
  scale_fill_distiller(palette = "Spectral") + 
  facet_grid(~ocn) + 
  #scale_x_log10()+scale_y_log10()+
  coord_cartesian(xlim=c(0.1, 100),ylim=c(0.1,100)) + 
  geom_label(data = stat_met2,
             aes(x = 95, y = 0.25, label = lab), label.padding = unit(0.1, "lines"), 
             label.size=0, size = 2.5) + 
  theme_very_small + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  geom_line(aes(buyos, imrg_rf), qq_dat, col = "red", type = "dotted")


##########################################################################


###  for all imrg runs

ggplot(ind_alt_pacf_meanplot[buyos >= 0.1 & imrg_rf >= 0.1], aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 50) + 
  labs(x = "Buyos (mm/day)", y = "IMERG (mm/day)") +  
  geom_abline(intercept = 0, slope = 1, col = "red") + 
  scale_fill_continuous(type = "viridis") +  
  facet_grid(variable~ocn) + 
  scale_x_log10()+scale_y_log10()+
  coord_cartesian(xlim=c(0.1, 220),ylim=c(0.1,220)) + 
  geom_label(data = stat_met2,
             aes(x = 0.28, y = 70, label = lab), label.padding = unit(0.25, "lines"), 
             label.size=0, size = 2.5) + 
  theme_small

ggsave("results/figures/scat_dens_hitdays.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


### all_days

ind_alt_pacf_meanplot[buyos < 0]

ggplot(ind_alt_pacf_meanplot, aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 100) + 
  labs(x = "Buoys (mm/day)", y = "IMERG (mm/day)") + 
  #coord_equal(ratio = 1) + 
  geom_abline(intercept = 0, slope = 1, col = "black") + 
  #scale_fill_continuous(type = "viridis", breaks = c(0, 20, 40, 60, 80 ,100)) +  
  #scale_fill_distiller(type = "seq", palette = "Reds") + 
  scale_fill_distiller(palette = "Spectral", breaks = c(0, 20, 40, 60, 80, 100)) + 
  # scale_fill_fermenter() + 
  #scale_fill_binned(type = "viridis", breaks = c(0, 10, 20, 30, 40, 60, 80 ,100)) + 
  #scale_fill_binned_diverging(palette = "Blue-Red2", breaks = c(0, 10, 20, 30, 40, 60, 80 ,100)) + 
  facet_grid(variable~ocn) + 
  scale_x_log10()+scale_y_log10()+
  coord_cartesian(xlim=c(0.1, 220),ylim=c(0.1,220)) + 
  geom_label(data = stat_met2,
             aes(x = 65, y = 0.3, label = lab), label.padding = unit(0.1, "lines"), 
             label.size=0, size = 2.0) + 
  theme_small

ggsave("results/figures/scat_dens_alldays.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


### only for IMERG-final run

ggplot(ind_alt_pacf_meanplot[variable == "IMERG-F"], aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 100) + 
  labs(x = "Buoys (mm/day)", y = "IMERG (mm/day)") + 
  #coord_equal(ratio = 1) + 
  geom_abline(intercept = 0, slope = 1, col = "black") + 
  #scale_fill_continuous(type = "viridis", breaks = c(0, 20, 40, 60, 80 ,100)) +  
  #scale_fill_distiller(type = "seq", palette = "Reds") + 
  scale_fill_distiller(palette = "Spectral", breaks = c(0, 20, 40, 60, 80, 100)) + 
   facet_wrap(~ocn) + 
  scale_x_log10() + scale_y_log10() +
  coord_cartesian(xlim=c(0.1, 220), ylim=c(0.1,220)) + 
  geom_label(data = stat_met2,
             aes(x = 115, y = 0.3, label = lab), label.padding = unit(0.1, "lines"), 
             label.size=0, size = 2.0) + 
  theme_small


ggsave("results/figures/scat_dens_alldays_finalrun.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)




library(ggrepel)
ggplot(ind_alt_pacf_meanplot, aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 100) + 
  labs(x = "Buyos (mm/day)", y = "IMERG (mm/day)") + 
  #coord_equal(ratio = 1) + 
  geom_abline(intercept = 0, slope = 1, col = "black") + 
  #scale_fill_continuous(type = "viridis", breaks = c(0, 20, 40, 60, 80 ,100)) +  
  #scale_fill_distiller(type = "seq", palette = "Reds") + 
  scale_fill_distiller(palette = "Spectral", breaks = c(0, 20, 40, 60, 80, 100)) + 
  # scale_fill_fermenter() + 
  #scale_fill_binned(type = "viridis", breaks = c(0, 10, 20, 30, 40, 60, 80 ,100)) + 
  #scale_fill_binned_diverging(palette = "Blue-Red2", breaks = c(0, 10, 20, 30, 40, 60, 80 ,100)) + 
  facet_grid(variable~ocn) + 
  scale_x_log10()+scale_y_log10()+
  coord_cartesian(xlim=c(0.1, 220),ylim=c(0.1,220)) + 
  geom_label_repel(data = stat_met2,
             aes(x = 200, y = 0.01, label = lab), label.padding=.1, alpha = 0.6, 
             label.size=0, size = 2.0) + 
  theme_small


ggsave("results/figures/scat_dens_alldays2.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)



a <- ggplot(ind_alt_pacf_meanplot, aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 100) + 
  labs(x = "buyos", y = "imrg") + 
  #coord_equal(ratio = 1) + 
  geom_abline(intercept = 0, slope = 1, col = "red") + 
  facet_grid(variable~ocn) + 
  scale_x_log10()+scale_y_log10()+
  coord_cartesian(xlim=c(0.1, 220),ylim=c(0.1,220)) + 
  theme_small

a + binned_scale("fill",
             "foo",
             ggplot2:::binned_pal(scales::manual_pal(c("blue","green", "yellow", "orange", "red"))),
             guide="coloursteps",
             breaks=seq(1, 100, by=20),
             limits=c(1,101),
             show.limits=TRUE)



# line_plot ---------------------------------------------------------------

ggplot(ind_atln_pacf_mean, aes(date, imrg_f)) + 
  geom_line() + 
  geom_point()

data_plot <- melt(ind_atln_pacf_mean, c("date", "ocn"))

ggplot(data_plot, aes(date, value, col = variable)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~ocn) + 
  theme_generic

ggplot(ind_alt_pacf_meanplot[variable == "imrg_f"], aes(date, imrg_rf)) + 
  geom_line() + 
  geom_point()

### monthly
ind_atln_pacf_mean
ind_atln_pacf_mean2 <- ind_atln_pacf_mean[, .(date = YearMonth(date), ocn, imrg_e, imrg_l, 
                                              imrg_f, buyos)] 

monthly <- ind_atln_pacf_mean2[, lapply(.SD, sum, na.rm=TRUE), by = .(date, ocn), 
                              .SDcols=c("IMERG_E", "imrg_l", "imrg_f", "buyos")]

monthly_plot <- melt(monthly, c("date", "ocn"))



ggplot(monthly_plot, aes(group = variable, y = value, x = factor(date))) + 
  geom_line(aes(color = variable)) + 
  geom_point() + 
  facet_wrap(~ocn) + 
  theme_generic

# yearly
yearly <- ind_atln_pacf_mean[, lapply(.SD, sum, na.rm=TRUE), by = .(year(date), ocn), 
                               .SDcols=c("imrg_e", "imrg_l", "imrg_f", "buyos")]

yearly_plot <- melt(yearly, c("year", "ocn"))

ggplot(yearly_plot, aes(group = variable, y = value, x = factor(year))) + 
  geom_line(aes(color = variable)) + 
  geom_point() + 
  facet_wrap(~ocn) + 
  theme_generic

ggplot(yearly_plot[ocn == "west_pacf"], aes(group = variable, y = value, x = factor(year))) + 
  geom_line(aes(color = variable)) + 
  geom_point() + 
  #facet_wrap(~ocn) + 
  theme_generic


# with threshold of rain/norain = 1.0 mm/day ------------------------------

threshold <- 0

ind <- ind_alt_pacf_meanplot[variable == "IMERG-F" & ocn == "Indian" & buyos >= threshold & imrg_rf >= threshold]
atln <- ind_alt_pacf_meanplot[variable == "IMERG-F" & ocn == "Atlantic" & buyos >= threshold & imrg_rf >= threshold]
east_pacf <- ind_alt_pacf_meanplot[variable == "IMERG-F" & ocn == "East Pacific" & buyos >= threshold & imrg_rf >= threshold]
west_pacf <- ind_alt_pacf_meanplot[variable == "IMERG-F" & ocn == "West Pacific" & buyos >= threshold & imrg_rf >= threshold]




qval <- seq(0.1, 1.0, 0.01)
qval <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.75, 0.8, 0.85, 0.9, 0.95, 1.0)

ind_buoy <- unname(round(quantile(ind$buyos, na.rm = T, probs = c(qval))))
ind_imrg <- unname(round(quantile(ind$imrg_rf, na.rm = T, probs = c(qval))))

dt_ind <- data.table(quantiles = c(qval), 
                     buoy =  c(ind_buoy),
                     imrg = c(ind_imrg), 
                     ocn = "Indian")

atln_buoy <- unname(round(quantile(atln$buyos, na.rm = T, probs = c(qval))))
atln_imrg <- unname(round(quantile(atln$imrg_rf, na.rm = T, probs = c(qval))))

dt_atln <- data.table(quantiles = c(qval), 
                      buoy =  c(atln_buoy),
                      imrg = c(atln_imrg), 
                      ocn = "Atlantic")

east_pacf_buoy <- unname(round(quantile(east_pacf$buyos, na.rm = T, probs = c(qval))))
east_pacf_imrg <- unname(round(quantile(east_pacf$imrg_rf, na.rm = T, probs = c(qval))))

dt_east_pacf <- data.table(quantiles = c(qval), 
                           buoy =  c(east_pacf_buoy),
                           imrg = c(east_pacf_imrg), 
                           ocn = "East Pacific")

west_pacf_buoy <- unname(round(quantile(west_pacf$buyos, na.rm = T, probs = c(qval))))
west_pacf_imrg <- unname(round(quantile(west_pacf$imrg_rf, na.rm = T, probs = c(qval))))

dt_west_pacf <- data.table(quantiles = c(qval), 
                           buoy =  c(west_pacf_buoy),
                           imrg = c(west_pacf_imrg), 
                           ocn = "West Pacific")

all_dat <- rbind(dt_ind, dt_atln, dt_east_pacf, dt_west_pacf)
all_dat$ocn <- factor(all_dat$ocn, levels = c("Indian", "Atlantic", "East Pacific", "West Pacific"))

ggplot(ind_alt_pacf_meanplot[variable == "IMERG-F" & buyos >= 0.1 & imrg_rf >= 0.1], aes(x = buyos, y = imrg_rf)) +
  geom_bin2d(bins = 50) + 
  labs(x = "Buyos (mm/day)", y = "IMERG (mm/day)") +  
  geom_abline(intercept = 0, slope = 1, col = "black") + 
  #scale_fill_continuous(type = "viridis") +  
  scale_fill_distiller(palette = "Spectral") + 
  facet_grid(~ocn) + 
  scale_x_log10()+scale_y_log10()+
  coord_cartesian(xlim=c(1, 220),ylim=c(1,220)) + 
  geom_label(data = stat_met2,
             aes(x = 95, y = 1.5, label = lab), label.padding = unit(0.1, "lines"), 
             label.size=0, size = 2.5) + 
  theme_very_small + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + geom_line(aes(buoy, imrg), all_dat, col = "red")

ggsave("./results/supp_fig/supp_scatdens_hitdaysimrgf.png",
       width = 9.9, height = 4.0, units = "in", dpi = 600)
