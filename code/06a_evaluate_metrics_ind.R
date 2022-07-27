source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

#### read the datasets

ind_rama_imrg <- readRDS("./data/ind_rama_imrg.rds")

### preprocess

unique(ind_rama_imrg, by = "sname")
ind_rama_imrg <- ind_rama_imrg[complete.cases(ind_rama_imrg)]
ind_rama_imrg <- ind_rama_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, ind_rama, sname)]
summary(ind_rama_imrg)


# volumetric_metrices Biuas, RMSE, MAE -----------------------------------------

ind_rama_imrg_long <- melt(ind_rama_imrg, c("lat", "lon", "date", "sname", "ind_rama"), 
                      value.name = "imrg_rf", variable.name = "imrg_run")

volmet_ind <- ind_rama_imrg_long[, .(bias = sum(imrg_rf - ind_rama)/.N, 
                                 rmse = sqrt(sum((imrg_rf - ind_rama)^2)/.N), 
                                 mae = sum(abs(imrg_rf - ind_rama))/.N, 
                                 ocn = factor('ind')), by = .(sname, imrg_run)]

### plot

volmet_ind_plot <- melt(volmet_ind, c("sname", "imrg_run", "ocn"))

ggplot(volmet_ind_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/volmet_ind.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# categorical_matrices_POD,FAR,CSI ----------------------------------------

catmet_ind <- ind_rama_imrg_long[imrg_rf >= 0.1 & ind_rama >= 0.1, rf_class := factor('H')]
catmet_ind[imrg_rf < 0.1 & ind_rama >= 0.1, rf_class := factor('M')]
catmet_ind[imrg_rf >= 0.1 & ind_rama < 0.1, rf_class := factor('FA')]
catmet_ind[imrg_rf <= 0.1 & ind_rama <= 0.1, rf_class := factor('CN')]


catmet_ind2 <- catmet_ind[, .N, by = .(rf_class, sname, imrg_run)]
catmet_ind_wide <- dcast(catmet_ind2,  sname + imrg_run ~ rf_class)

catmet_ind_wide[, `:=`(POD = H /(H + M), FAR = FA / (H + FA), CSI = H / (H + M + FA), 
                              tot_events = (H + M + FA + CN), ocn = factor('ind')), 
                      by = .(sname, imrg_run)]

### plot

catmet_ind_plot <- catmet_ind_wide[, .(sname, imrg_run, POD, FAR, CSI)]
catmet_ind_plot <- melt(catmet_ind_plot, c("sname", "imrg_run"))

ggplot(catmet_ind_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  ggtitle("all_datasets_indian_ocean (2004-20)") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/catmet_ind.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# save the datasets ------------------------------------------------

metrics_ind <- volmet_ind[catmet_ind_wide, on = .(sname, imrg_run, ocn)]
saveRDS(metrics_ind, "./data/metrics_ind.rds")

############################################
