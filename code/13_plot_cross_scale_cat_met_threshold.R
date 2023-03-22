
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

#ind <- ind_dat[buyos > 0]
ind_percnt <- seq(0.1, 1, 0.1)
thresholds <- as.list(ind_percnt)

intensity <- seq(0.1, 1, 0.1)

ind_met <- lapply(thresholds, cat_met, dt=ind_dat)
ind_met_dt <- rbindlist(ind_met)
ind_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('Indian'))]
ind_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('Indian'))]

ggplot(ind_met_dt[variable == 'IMERG-F'], aes(factor(thresh), POD)) + 
  geom_line() + 
  geom_point()


### Atln

atln_dat <- ind_atln_pacf_meanplot[ocn == "Atlantic"]
#atln <- atln_dat[buyos > 0]
#atln_percnt <- quantile(atln$buyos, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
#thresholds_atln <- as.list(atln_percnt)

atln_met <- lapply(thresholds, cat_met, dt=atln_dat)
atln_met_dt <- rbindlist(atln_met)
atln_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('Atlantic'))]


### East Pacf

east_pacf_dat <- ind_atln_pacf_meanplot[ocn == "East Pacific"]
# east_pacf <- east_pacf_dat[buyos > 0]
# east_pacf_percnt <- quantile(east_pacf$buyos, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
# thresholds_east_pacf <- as.list(east_pacf_percnt)

east_pacf_met <- lapply(thresholds, cat_met, dt=east_pacf_dat)
east_pacf_met_dt <- rbindlist(east_pacf_met)
east_pacf_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('East Pacific'))]


### West Pacf

west_pacf_dat <- ind_atln_pacf_meanplot[ocn == "West Pacific"]
# west_pacf <- west_pacf_dat[buyos > 0]
# west_pacf_percnt <- quantile(west_pacf$buyos, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
# thresholds_west_pacf <- as.list(west_pacf_percnt)

west_pacf_met <- lapply(thresholds, cat_met, dt=west_pacf_dat)
west_pacf_met_dt <- rbindlist(west_pacf_met)
west_pacf_met_dt[, `:=`(thresh = rep(intensity, each = 3), ocn = factor('West Pacific'))]


### join all to plot with facets

ind_alt_pacf <- rbind(ind_met_dt, atln_met_dt, east_pacf_met_dt, west_pacf_met_dt)
ind_alt_pacf_long <- melt(ind_alt_pacf, c("variable", "thresh", "ocn"), variable.name = "metrices")


plot_thres <- ggplot(ind_alt_pacf_long[variable == "IMERG-F"], aes(factor(thresh), value, col = ocn, group = ocn)) + 
  geom_line() + 
  geom_point() + 
  facet_grid(~metrices) + 
  scale_color_manual(values = mycol_continent5[c(4, 3, 2, 1)]) +
  labs(x = "Threshold", y = "") + 
  geom_hline(yintercept=0.5,linetype=2) + 
  #coord_cartesian(xlim=c(1, 100)) + 
  theme_small + 
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  theme(strip.background = element_rect(fill = "white")) + 
  theme(axis.text.x=element_text(color=c("black","transparent","black",
                                         "transparent","black","transparent","black","transparent")))



ggsave("results/paper_fig/cat_met_threshold_cross_scale.png",
       width = 7.2, height = 5.2, units = "in", dpi = 600)


#############################################################



# Compare the categorical metrics with rain/no-rain = 1 mm/day -----------

source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/functions.R')

#### read the datasets

ind_rama_imrg <- readRDS("./data/ind_rama_imrg.rds")
atln_pirata_imrg <- readRDS("./data/atln_pirata_imrg.rds")
pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")


## pre-process
ind_rama_imrg <- ind_rama_imrg[complete.cases(ind_rama_imrg)]
ind_rama_imrg <- ind_rama_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = ind_rama, sname)]
ind_rama_imrg_long <- melt(ind_rama_imrg, c("lat", "lon", "date", "sname", "obs"), 
                           value.name = "imrg_rf", variable.name = "imrg_run")

atln_pirata_imrg <- atln_pirata_imrg[complete.cases(atln_pirata_imrg)]
atln_pirata_imrg <- atln_pirata_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = atln_pirata, sname)]
atln_pirata_imrg_long <- melt(atln_pirata_imrg, c("lat", "lon", "date", "sname", "obs"), 
                              value.name = "imrg_rf", variable.name = "imrg_run")

east_pacf_tao_imrg <- pacf_tao_imrg[lon < 0]
east_pacf_tao_imrg <- east_pacf_tao_imrg[complete.cases(east_pacf_tao_imrg)]
east_pacf_tao_imrg <- east_pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = pacf_tao, sname)]
east_pacf_tao_imrg_long <- melt(east_pacf_tao_imrg, c("lat", "lon", "date", "sname", "obs"), 
                                value.name = "imrg_rf", variable.name = "imrg_run")

west_pacf_tao_imrg <- pacf_tao_imrg[lon >= 0]
west_pacf_tao_imrg <-west_pacf_tao_imrg[complete.cases(west_pacf_tao_imrg)]
west_pacf_tao_imrg <- west_pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = pacf_tao, sname)]
west_pacf_tao_imrg_long <- melt(west_pacf_tao_imrg, c("lat", "lon", "date", "sname", "obs"), 
                                value.name = "imrg_rf", variable.name = "imrg_run")


# estimate the metrics

thld <- 1 #change according to the requirements

catmet_ind <- catmet_stats(ind_rama_imrg_long, thld)
catmet_ind[, `:=`(ocn = factor("Indian"))]

catmet_atln <- catmet_stats(atln_pirata_imrg_long, thld)
catmet_atln[, `:=`(ocn = factor("Atlantic"))]

catmet_east_pacf <- catmet_stats(east_pacf_tao_imrg_long, thld)
catmet_east_pacf[, `:=`(ocn = factor("East Pacific"))]

catmet_west_pacf <- catmet_stats(west_pacf_tao_imrg_long, thld)
catmet_west_pacf[, `:=`(ocn = factor("West Pacific"))]


catmet_all <- rbind(catmet_ind, catmet_atln, catmet_east_pacf, catmet_west_pacf)


# plot ----------------------------------------------------------------

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
mycol_continent5 <- c( "#00B0F6", "#A3A500", "#00BF7D", "#e07b39", 
                                "#80391e") 
                                
met_all_plot <- melt(catmet_all, c("sname", "imrg_run", "ocn"))

#levels(met_all_plot$variable) <- c("Bias (mm/day)", "RMSE (mm/day)", "MAE (mm/day)", "COR", "POD",  "FAR", "CSI")
levels(met_all_plot$imrg_run) <- c("IMERG-E", "IMERG-L", "IMERG-F")
levels(met_all_plot$ocn) <- c("Indian", "Atlantic", "East Pacific", "West Pacific")

box_plot <- ggplot(met_all_plot, aes(ocn, value, fill = imrg_run)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales = "free_y", ncol = 3) + 
  scale_fill_manual(values = c("808000",  "#D35C37", "#6590bb")) + 
  #scale_fill_manual(values = mycol_continent5[c(4, 3, 2, 1)]) + 
  #scale_fill_manual(values = c("#b64925", "808000", "#6590bb")) + 
  labs(x = "", y = "") + 
  theme_small + # for presentation slides
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'),
        axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 0.9))
#strip.text.x = element_text(size = 12))

ggsave("results/supp_fig/boxplot_metrices_threshold_1mm.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)


ggarrange(plot_thres, box_plot, ncol = 1, nrow = 2,
          labels = c("a)", "b)"), legend = "right")

ggsave("results/supp_fig/box&lineplot_metrices_threshold_1mm.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)


##################################################################

ind_rama_imrg[obs < 0 ]

ind_rama_imrg[imrg_f < 0.1]

