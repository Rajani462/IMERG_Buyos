
source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/functions.R')

#### read the datasets

ind_rama_imrg <- readRDS("./data/ind_rama_imrg.rds")
atln_pirata_imrg <- readRDS("./data/atln_pirata_imrg.rds")
pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")

#pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")


### preprocess

# sname15_65 <- ind_rama_imrg[sname == "15_65"]
# summary(sname15_65)
# table(sname15_65$qrn)
# table(sname15_65$srn)

unique(ind_rama_imrg, by = "sname")
ind_rama_imrg <- ind_rama_imrg[complete.cases(ind_rama_imrg)]
ind_rama_imrg <- ind_rama_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, ind_rama, sname)]
summary(ind_rama_imrg)

unique(atln_pirata_imrg, by = "sname")
atln_pirata_imrg <- atln_pirata_imrg[complete.cases(atln_pirata_imrg)]
atln_pirata_imrg <- atln_pirata_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, atln_pirata, sname)]
summary(atln_pirata_imrg)

east_pacf_tao_imrg <- pacf_tao_imrg[lon < 0]
unique(east_pacf_tao_imrg, by = "sname")
east_pacf_tao_imrg <- east_pacf_tao_imrg[complete.cases(east_pacf_tao_imrg)]
east_pacf_tao_imrg <- east_pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, pacf_tao, sname)]
summary(east_pacf_tao_imrg)

west_pacf_tao_imrg <- pacf_tao_imrg[lon >= 0]
unique(west_pacf_tao_imrg, by = "sname")
west_pacf_tao_imrg <-west_pacf_tao_imrg[complete.cases(west_pacf_tao_imrg)]
west_pacf_tao_imrg <- west_pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, pacf_tao, sname)]
summary(west_pacf_tao_imrg)


#

# Indian -----------------------------------------

ind_rama_imrg_long <- melt(ind_rama_imrg, c("lat", "lon", "date", "sname", "ind_rama"), 
                           value.name = "imrg_rf", variable.name = "imrg_run")
ind_rama_imrg_long <- ind_rama_imrg_long[, .(lat, lon, date, sname, obs = ind_rama, imrg_run, imrg_rf)]

volmet_ind <- vol_stats(ind_rama_imrg_long)
volmet_ind[, `:=`(ocn = factor("Indian"))]
summary(volmet_ind[imrg_run == "imrg_f"])


# Atlantic -----------------------------------------

atln_pirata_imrg_long <- melt(atln_pirata_imrg, c("lat", "lon", "date", "sname", "atln_pirata"), 
                              value.name = "imrg_rf", variable.name = "imrg_run")
atln_pirata_imrg_long <- atln_pirata_imrg_long[, .(lat, lon, date, sname, obs = atln_pirata, imrg_run, imrg_rf)]

volmet_atln <- vol_stats(atln_pirata_imrg_long)
volmet_atln[, `:=`(ocn = factor("Atlantic"))]
volmet_atln <- volmet_atln[sname != "-20_-10"]
summary(volmet_atln)

# East_pacific -----------------------------------------

east_pacf_tao_imrg_long <- melt(east_pacf_tao_imrg, c("lat", "lon", "date", "sname", "pacf_tao"), 
                           value.name = "imrg_rf", variable.name = "imrg_run")

east_pacf_tao_imrg_long <- east_pacf_tao_imrg_long[, .(lat, lon, date, sname, obs = pacf_tao, imrg_run, imrg_rf)]

volmet_east_pacf <- vol_stats(east_pacf_tao_imrg_long)
volmet_east_pacf[, `:=`(ocn = factor("East Pacific"))]
summary(volmet_east_pacf)


# West_pacific -----------------------------------------

west_pacf_tao_imrg_long <- melt(west_pacf_tao_imrg, c("lat", "lon", "date", "sname", "pacf_tao"), 
                           value.name = "imrg_rf", variable.name = "imrg_run")

west_pacf_tao_imrg_long <- west_pacf_tao_imrg_long[, .(lat, lon, date, sname, obs = pacf_tao, imrg_run, imrg_rf)]
volmet_west_pacf <- vol_stats(west_pacf_tao_imrg_long)
volmet_west_pacf[, `:=`(ocn = factor("West Pacific"))]
summary(volmet_west_pacf)


# plot --------------------------------------------------------------------

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
mycol_continent5 <- c( "#00B0F6", "#A3A500", "#00BF7D", "#e07b39", 
                                "#80391e") 

met_all <- rbind(volmet_ind, volmet_atln, volmet_east_pacf, volmet_west_pacf)
                                
met_all_plot <- melt(met_all, c("sname", "imrg_run", "ocn"))
met_all_plot2 <- met_all_plot[variable == "rbias" | variable == "nrmse" | variable == "nmae"]

#levels(met_all_plot2$variable) <- c("RBias", "NRMSE", "NMAE")
levels(met_all_plot2$imrg_run) <- c("IMERG-E", "IMERG-L", "IMERG-F")
levels(met_all_plot2$ocn) <- c("Indian", "Atlantic", "East Pacific", "West Pacific")

plot_metrics <- ggplot(met_all_plot, aes(ocn, value, fill = imrg_run)) + 
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

ggsave("results/supp_fig/relative_metrices_boxplot.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)

#######################################################################################


# Remove the stations with mean <= 0 -----------------------------------------

source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

#### read the datasets

ind_rama_imrg <- readRDS("./data/ind_rama_imrg.rds")
atln_pirata_imrg <- readRDS("./data/atln_pirata_imrg.rds")
pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")


## pre-process

unique(ind_rama_imrg, by = "sname")
ind_rama_imrg <- ind_rama_imrg[complete.cases(ind_rama_imrg)]
ind_rama_imrg <- ind_rama_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = ind_rama, sname)]
summary(ind_rama_imrg)

ind_rama_imrg <- ind_rama_imrg[, `:=` (ref_mean = mean(obs, na.rm = TRUE)), by = .(sname)]
unique(ind_rama_imrg[ref_mean <= 1], by = "sname")
ind_rama_imrg <- ind_rama_imrg[ref_mean >= 1]


unique(atln_pirata_imrg, by = "sname")
atln_pirata_imrg <- atln_pirata_imrg[complete.cases(atln_pirata_imrg)]
atln_pirata_imrg <- atln_pirata_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = atln_pirata, sname)]
summary(atln_pirata_imrg)

atln_pirata_imrg <- atln_pirata_imrg[, `:=` (ref_mean = mean(obs, na.rm = TRUE)), by = .(sname)]
unique(atln_pirata_imrg[ref_mean <= 1], by = "sname")
atln_pirata_imrg <- atln_pirata_imrg[ref_mean >= 1]

east_pacf_tao_imrg <- pacf_tao_imrg[lon < 0]
unique(east_pacf_tao_imrg, by = "sname")
east_pacf_tao_imrg <- east_pacf_tao_imrg[complete.cases(east_pacf_tao_imrg)]
east_pacf_tao_imrg <- east_pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = pacf_tao, sname)]
summary(east_pacf_tao_imrg)

east_pacf_tao_imrg <- east_pacf_tao_imrg[, `:=` (ref_mean = mean(obs, na.rm = TRUE)), by = .(sname)]
unique(east_pacf_tao_imrg[ref_mean <= 1], by = "sname")
east_pacf_tao_imrg <- east_pacf_tao_imrg[ref_mean >= 1]

west_pacf_tao_imrg <- pacf_tao_imrg[lon >= 0]
unique(west_pacf_tao_imrg, by = "sname")
west_pacf_tao_imrg <-west_pacf_tao_imrg[complete.cases(west_pacf_tao_imrg)]
west_pacf_tao_imrg <- west_pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = pacf_tao, sname)]
summary(west_pacf_tao_imrg)

west_pacf_tao_imrg <- west_pacf_tao_imrg[, `:=` (ref_mean = mean(obs, na.rm = TRUE)), by = .(sname)]
unique(west_pacf_tao_imrg[ref_mean <= 1], by = "sname")
west_pacf_tao_imrg <- west_pacf_tao_imrg[ref_mean >= 1]

# Indian -----------------------------------------

ind_rama_imrg <- ind_rama_imrg[, ref_mean:=NULL]
ind_rama_imrg_long <- melt(ind_rama_imrg, c("lat", "lon", "date", "sname", "obs"), 
                           value.name = "imrg_rf", variable.name = "imrg_run")

volmet_ind <- vol_stats(ind_rama_imrg_long)
volmet_ind[, `:=`(ocn = factor("Indian"))]

summary(volmet_ind[imrg_run == "imrg_f"])


# Atlantic -----------------------------------------

atln_pirata_imrg <- atln_pirata_imrg[, ref_mean:=NULL]
atln_pirata_imrg_long <- melt(atln_pirata_imrg, c("lat", "lon", "date", "sname", "obs"), 
                              value.name = "imrg_rf", variable.name = "imrg_run")

volmet_atln <- vol_stats(atln_pirata_imrg_long)
volmet_atln[, `:=`(ocn = factor("Atlantic"))]

summary(volmet_atln)

# East_pacific -----------------------------------------

east_pacf_tao_imrg <- east_pacf_tao_imrg[, ref_mean:=NULL]
east_pacf_tao_imrg_long <- melt(east_pacf_tao_imrg, c("lat", "lon", "date", "sname", "obs"), 
                                value.name = "imrg_rf", variable.name = "imrg_run")

volmet_east_pacf <- vol_stats(east_pacf_tao_imrg_long)
volmet_east_pacf[, `:=`(ocn = factor("East Pacific"))]
summary(volmet_east_pacf)

# West_pacific -----------------------------------------

west_pacf_tao_imrg <- west_pacf_tao_imrg[, ref_mean:=NULL]
west_pacf_tao_imrg_long <- melt(west_pacf_tao_imrg, c("lat", "lon", "date", "sname", "obs"), 
                                value.name = "imrg_rf", variable.name = "imrg_run")

volmet_west_pacf <- vol_stats(west_pacf_tao_imrg_long)
volmet_west_pacf[, `:=`(ocn = factor("West Pacific"))]
summary(volmet_west_pacf)


# plot --------------------------------------------------------------------

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
mycol_continent5 <- c( "#00B0F6", "#A3A500", "#00BF7D", "#e07b39", 
                                "#80391e") 
                                
met_all <- rbind(volmet_ind, volmet_atln, volmet_east_pacf, volmet_west_pacf)

met_all_plot <- melt(met_all, c("sname", "imrg_run", "ocn"))
met_all_plot <- met_all_plot[variable == "rbias" | variable == "rrmse" | variable == "rrmse2"| variable == "rmae"]

#levels(met_all_plot$variable) <- c("RBias", "RRMSE", "RMAE")
levels(met_all_plot$imrg_run) <- c("IMERG-E", "IMERG-L", "IMERG-F")
levels(met_all_plot$ocn) <- c("Indian", "Atlantic", "East Pacific", "West Pacific")

plot_metrics <- ggplot(met_all_plot, aes(ocn, value, fill = imrg_run)) + 
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

ggsave("results/supp_fig/boxplot_metrices_mean_morethan1.png",
               width = 7.2, height = 6.5, units = "in", dpi = 600)

###########################################################################

# see how the metrics changes when considering only high quality datasets
# remove station values with qrn <1 -------------------------------

source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/functions.R')

#### read the datasets

ind_rama_imrg <- readRDS("./data/ind_rama_imrg.rds")
atln_pirata_imrg <- readRDS("./data/atln_pirata_imrg.rds")
pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")


## pre-process

high_qual <- 1
med_qual <- 2

#ind
unique(ind_rama_imrg, by = "sname")
ind_rama_imrg <- ind_rama_imrg[complete.cases(ind_rama_imrg)]
summary(ind_rama_imrg)

unique(ind_rama_imrg[qrn <= high_qual], by = "sname")
ind_rama_imrg <- ind_rama_imrg[qrn <= high_qual]
ind_rama_imrg <- ind_rama_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = ind_rama, sname)]

#atln
unique(atln_pirata_imrg, by = "sname")
atln_pirata_imrg <- atln_pirata_imrg[complete.cases(atln_pirata_imrg)]
summary(atln_pirata_imrg)


unique(atln_pirata_imrg[qrn <= high_qual], by = "sname")
atln_pirata_imrg <- atln_pirata_imrg[qrn <= high_qual]
atln_pirata_imrg <- atln_pirata_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = atln_pirata, sname)]


#east_pacf
east_pacf_tao_imrg <- pacf_tao_imrg[lon < 0]
unique(east_pacf_tao_imrg, by = "sname")
east_pacf_tao_imrg <- east_pacf_tao_imrg[complete.cases(east_pacf_tao_imrg)]
summary(east_pacf_tao_imrg)

unique(east_pacf_tao_imrg[qrn <= high_qual], by = "sname")
east_pacf_tao_imrg <- east_pacf_tao_imrg[qrn <= high_qual]
east_pacf_tao_imrg <- east_pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = pacf_tao, sname)]

#west
west_pacf_tao_imrg <- pacf_tao_imrg[lon >= 0]
unique(west_pacf_tao_imrg, by = "sname")
west_pacf_tao_imrg <-west_pacf_tao_imrg[complete.cases(west_pacf_tao_imrg)]
summary(west_pacf_tao_imrg)

unique(west_pacf_tao_imrg[qrn >= high_qual], by = "sname")
west_pacf_tao_imrg <- west_pacf_tao_imrg[qrn <= high_qual]
west_pacf_tao_imrg <- west_pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = pacf_tao, sname)]

# Indian -----------------------------------------


ind_rama_imrg_long <- melt(ind_rama_imrg, c("lat", "lon", "date", "sname", "obs"), 
                           value.name = "imrg_rf", variable.name = "imrg_run")

volmet_ind <- vol_stats(ind_rama_imrg_long)
volmet_ind[, `:=`(ocn = factor("Indian"))]

summary(volmet_ind[imrg_run == "imrg_f"])


# Atlantic -----------------------------------------

atln_pirata_imrg_long <- melt(atln_pirata_imrg, c("lat", "lon", "date", "sname", "obs"), 
                              value.name = "imrg_rf", variable.name = "imrg_run")

volmet_atln <- vol_stats(atln_pirata_imrg_long)
volmet_atln[, `:=`(ocn = factor("Atlantic"))]

summary(volmet_atln)

# East_pacific -----------------------------------------

east_pacf_tao_imrg_long <- melt(east_pacf_tao_imrg, c("lat", "lon", "date", "sname", "obs"), 
                                value.name = "imrg_rf", variable.name = "imrg_run")

volmet_east_pacf <- vol_stats(east_pacf_tao_imrg_long)
volmet_east_pacf[, `:=`(ocn = factor("East Pacific"))]
summary(volmet_east_pacf)

# West_pacific -----------------------------------------

west_pacf_tao_imrg_long <- melt(west_pacf_tao_imrg, c("lat", "lon", "date", "sname", "obs"), 
                                value.name = "imrg_rf", variable.name = "imrg_run")

volmet_west_pacf <- vol_stats(west_pacf_tao_imrg_long)
volmet_west_pacf[, `:=`(ocn = factor("West Pacific"))]
summary(volmet_west_pacf)


# plot --------------------------------------------------------------------

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
mycol_continent5 <- c( "#00B0F6", "#A3A500", "#00BF7D", "#e07b39", 
                                "#80391e") 
                                
volmet_all <- rbind(volmet_ind, volmet_atln, volmet_east_pacf, volmet_west_pacf)

volmet_all_plot <- melt(volmet_all, c("sname", "imrg_run", "ocn"))
#met_all_plot <- met_all_plot[variable == "rbias" | variable == "rrmse" | variable == "rrmse2"| variable == "rmae"]

#levels(met_all_plot$variable) <- c("RBias", "RRMSE", "RMAE")
levels(volmet_all_plot$imrg_run) <- c("IMERG-E", "IMERG-L", "IMERG-F")
levels(volmet_all_plot$ocn) <- c("Indian", "Atlantic", "East Pacific", "West Pacific")

plot_metrics <- ggplot(volmet_all_plot, aes(ocn, value, fill = imrg_run)) + 
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

ggsave("results/supp_fig/boxplot_rel_metrices_highquality.png",
       width = 7.2, height = 6.5, units = "in", dpi = 600)


# Categeorical metrics ----------------------------------------------------

thld <- 0.1 #change according to the requirements

catmet_ind <- catmet_stats(ind_rama_imrg_long, thld)
catmet_ind[, `:=`(ocn = factor("Indian"))]

catmet_atln <- catmet_stats(atln_pirata_imrg_long, thld)
catmet_atln[, `:=`(ocn = factor("Atlantic"))]

catmet_east_pacf <- catmet_stats(east_pacf_tao_imrg_long, thld)
catmet_east_pacf[, `:=`(ocn = factor("East Pacific"))]

catmet_west_pacf <- catmet_stats(west_pacf_tao_imrg_long, thld)
catmet_west_pacf[, `:=`(ocn = factor("West Pacific"))]


catmet_all <- rbind(catmet_ind, catmet_atln, catmet_east_pacf, catmet_west_pacf)

vol_cat_met <- volmet_all[catmet_all, on = .(sname, imrg_run, ocn)]

vol_cat_long <- melt(vol_cat_met, c("sname", "imrg_run", "ocn", "ref_mean"))


to_plot <- vol_cat_met[, .(sname, imrg_run, ocn, ref_mean, bias, rmse, mae, POD, FAR, CSI)]
to_plot_long <- melt(to_plot, c("sname", "imrg_run", "ocn", "ref_mean"))

ggplot(to_plot_long , aes(ocn, value, fill = imrg_run)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales = "free_y", ncol = 3) + 
  scale_fill_manual(values = c("808000",  "#D35C37", "#6590bb")) + 
  labs(x = "", y = "") + 
  theme_small + # for presentation slides
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'),
        axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 0.9))

ggsave("results/supp_fig/boxplot_metrices_highquality.png",
       width = 7.2, height = 6.5, units = "in", dpi = 600)
