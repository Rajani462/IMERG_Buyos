source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

#### read the datasets

atln_pirata_imrg <- readRDS("./data/atln_pirata_imrg.rds")

### preprocess

unique(atln_pirata_imrg, by = "sname")
atln_pirata_imrg <- atln_pirata_imrg[complete.cases(atln_pirata_imrg)]
atln_pirata_imrg <- atln_pirata_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, atln_pirata, sname)]
summary(atln_pirata_imrg)


# volumetric_metrices Biuas, RMSE, MAE -----------------------------------------

atln_pirata_imrg_long <- melt(atln_pirata_imrg, c("lat", "lon", "date", "sname", "atln_pirata"), 
                      value.name = "imrg_rf", variable.name = "imrg_run")

volmet_atln <- atln_pirata_imrg_long[, .(ref_mean = mean(atln_pirata, na.rm = TRUE), 
                                         bias = sum(imrg_rf - atln_pirata)/.N, 
                                         rbias = ((sum(imrg_rf - atln_pirata))/sum(atln_pirata))*100, 
                                         rmse = sqrt(sum((imrg_rf - atln_pirata)^2)/.N), 
                                         mae = sum(abs(imrg_rf - atln_pirata))/.N, 
                                         cor = cor(imrg_rf, atln_pirata), 
                                         ocn = factor('atln')), by = .(sname, imrg_run)]

### plot

volmet_atln_plot <- melt(volmet_atln, c("sname", "imrg_run", "ocn"))

ggplot(volmet_atln_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/volmet_atln.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# categorical_matrices_POD,FAR,CSI ----------------------------------------

catmet_atln <- atln_pirata_imrg_long[imrg_rf >= 0.1 & atln_pirata >= 0.1, rf_class := factor('H')]
catmet_atln[imrg_rf < 0.1 & atln_pirata >= 0.1, rf_class := factor('M')]
catmet_atln[imrg_rf >= 0.1 & atln_pirata < 0.1, rf_class := factor('FA')]
catmet_atln[imrg_rf <= 0.1 & atln_pirata <= 0.1, rf_class := factor('CN')]


catmet_atln2 <- catmet_atln[, .N, by = .(rf_class, sname, imrg_run)]
catmet_atln_wide <- dcast(catmet_atln2,  sname + imrg_run ~ rf_class)

catmet_atln_wide[, `:=`(POD = H /(H + M), FAR = FA / (H + FA), CSI = H / (H + M + FA), 
                              tot_events = (H + M + FA + CN), ocn = factor('atln')), 
                      by = .(sname, imrg_run)]

### plot

catmet_atln_plot <- catmet_atln_wide[, .(sname, imrg_run, POD, FAR, CSI)]
catmet_atln_plot <- melt(catmet_atln_plot, c("sname", "imrg_run"))

ggplot(catmet_atln_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  ggtitle("all_datasets_atlnian_ocean (2001-20)") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/catmet_atln.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# save the datasets ------------------------------------------------

metrics_atln <- volmet_atln[catmet_atln_wide, on = .(sname, imrg_run, ocn)]
saveRDS(metrics_atln, "./data/metrics_atln.rds")

############################################
