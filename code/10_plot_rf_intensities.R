
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

ind_atln_pacf_meanplot <- melt(ind_atln_pacf_mean, c('date', 'ocn', 'buyos'), value.name = "imrg_rf")



# function for categerical mterices ---------------------------------------


cat_met <- function(dt, thld){ 
  
  cat_metric_f <- dt[imrg_rf >= thld & buyos >= thld, rf_class := factor('H')]
  cat_metric_f[imrg_rf < thld & buyos >= thld, rf_class := factor('M')]
  cat_metric_f[imrg_rf >= thld & buyos < thld, rf_class := factor('FA')]
  cat_metric_f[imrg_rf < thld & buyos < thld, rf_class := factor('CN')]
  
  cat_metric_daily_f <- cat_metric_f[, .N, by = .(rf_class, variable)]
  
  #wider_f <- data.frame(t(unstack(rev(cat_metric_daily_f))), row.names = NULL, check.names = FALSE)
  #wider_f <- as.data.table(wider_f)
  
  cat_metric_wide <- dcast(cat_metric_daily_f, variable ~ rf_class)
  
  cat_met <- cat_metric_wide[, `:=`(POD = H /(H + M), FAR = FA / (H + FA), CSI = H / (H + M + FA), 
                                    tot_events = (H + M + FA + CN)), by = variable]
  
  cat_met <- cat_met[, .(variable, POD, FAR, CSI)]
  
  return(cat_met)
  
}


# calculate the metrices using the function --------------------------------

### Indian Ocean

ind_dat <- ind_atln_pacf_meanplot[ocn == "Indian"]

ind <- ind_dat[buyos > 0]
ind_percnt <- quantile(ind$buyos, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
thresholds_ind <- as.list(ind_percnt)

intensity <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)

ind_met <- lapply(thresholds_ind, cat_met, dt=ind_dat)
ind_met_dt <- rbindlist(ind_met)
ind_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('Indian'))]
ind_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('Indian'))]

ggplot(ind_met_dt[variable == 'IMERG-F'], aes(thresh, POD)) + 
  geom_line() + 
  geom_point()


### Atln

atln_dat <- ind_atln_pacf_meanplot[ocn == "Atlantic"]
atln <- atln_dat[buyos > 0]
atln_percnt <- quantile(atln$buyos, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
thresholds_atln <- as.list(atln_percnt)

atln_met <- lapply(thresholds_atln, cat_met, dt=atln_dat)
atln_met_dt <- rbindlist(atln_met)
atln_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('Atlantic'))]


### East Pacf

east_pacf_dat <- ind_atln_pacf_meanplot[ocn == "East Pacific"]
east_pacf <- east_pacf_dat[buyos > 0]
east_pacf_percnt <- quantile(east_pacf$buyos, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
thresholds_east_pacf <- as.list(east_pacf_percnt)

east_pacf_met <- lapply(thresholds_east_pacf, cat_met, dt=east_pacf_dat)
east_pacf_met_dt <- rbindlist(east_pacf_met)
east_pacf_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('East Pacific'))]


### West Pacf

west_pacf_dat <- ind_atln_pacf_meanplot[ocn == "West Pacific"]
west_pacf <- west_pacf_dat[buyos > 0]
west_pacf_percnt <- quantile(west_pacf$buyos, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
thresholds_west_pacf <- as.list(west_pacf_percnt)

west_pacf_met <- lapply(thresholds_west_pacf, cat_met, dt=west_pacf_dat)
west_pacf_met_dt <- rbindlist(west_pacf_met)
west_pacf_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('West Pacific'))]


### join all to plot with facets

ind_alt_pacf <- rbind(ind_met_dt, atln_met_dt, east_pacf_met_dt, west_pacf_met_dt)
ind_alt_pacf_long <- melt(ind_alt_pacf, c("variable", "thresh", "ocn"), variable.name = "metrices")


ggplot(ind_alt_pacf_long[variable == "IMERG-F"], aes(factor(thresh), value, col = ocn, group = ocn)) + 
  geom_line() + 
  geom_point() + 
  facet_grid(~metrices) + 
  scale_color_manual(values = mycol_continent5[c(4, 3, 2, 1)]) +
  labs(x = "Percentile", y = "") + 
  geom_hline(yintercept=0.5,linetype=2) + 
  #coord_cartesian(xlim=c(1, 100)) + 
  theme_small + 
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  #theme(legend.text = element_text(family = font, size = 12, color = "#222222")) + 
  #theme(axis.text.x = element_text(size = 12)) + 
  theme(axis.text.x = element_text(margin = unit(c(0.4, 0, 0, 0), "cm"), 
                                   family = font, size = 10, color = "#222222"), 
        axis.title.x = element_text(margin = unit(c(0.4, 0, 0, 0), "cm"), 
                                    family = font, size = 12, color = "#222222"),
        axis.title.y = element_text(margin = unit(c(0, 0.3, 0, 0), "cm"), 
                                    family = font, size = 12, color = "#222222"), 
        axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm"), 
                                   family = font, size = 10, color = "#222222"), 
        legend.text = element_text(family = font, size = 14, color = "#222222")) + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'Black'), 
        strip.text.x = element_text(size = 14))

ggsave("results/paper_fig/cat_met_pecentile_f.png",
       width = 7.2, height = 5.2, units = "in", dpi = 600)

#############################################################