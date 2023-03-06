source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

#### read the datasets

pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")

### preprocess

pacf_tao_imrg <- pacf_tao_imrg[lon >= 0]
unique(pacf_tao_imrg, by = "sname")
pacf_tao_imrg <- pacf_tao_imrg[complete.cases(pacf_tao_imrg)]
pacf_tao_imrg <- pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, pacf_tao, sname)]
summary(pacf_tao_imrg)


# volumetric_metrices Biuas, RMSE, MAE -----------------------------------------

pacf_tao_imrg_long <- melt(pacf_tao_imrg, c("lat", "lon", "date", "sname", "pacf_tao"), 
                      value.name = "imrg_rf", variable.name = "imrg_run")

volmet_pacf <- pacf_tao_imrg_long[, .(ref_mean = mean(pacf_tao, na.rm = TRUE), 
                                      bias = sum(imrg_rf - pacf_tao)/.N, 
                                      pbias = ((sum(imrg_rf - pacf_tao))/sum(pacf_tao))*100, 
                                      rbias = ((sum(imrg_rf - pacf_tao))/sum(pacf_tao)),
                                      rmse = sqrt(sum((imrg_rf - pacf_tao)^2)/.N), 
                                      nrmse = sqrt(sum((imrg_rf - pacf_tao)^2)/.N)/sd(pacf_tao), 
                                      mae = sum(abs(imrg_rf - pacf_tao))/.N, 
                                      cor = cor(imrg_rf, pacf_tao), 
                                      ocn = factor('west_pacf')), by = .(sname, imrg_run)]

### plot

volmet_pacf_plot <- melt(volmet_pacf, c("sname", "imrg_run", "ocn"))

ggplot(volmet_pacf_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/volmet_west_pacf.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# categorical_matrices_POD,FAR,CSI ----------------------------------------

catmet_pacf <- pacf_tao_imrg_long[imrg_rf >= 0.1 & pacf_tao >= 0.1, rf_class := factor('H')]
catmet_pacf[imrg_rf < 0.1 & pacf_tao >= 0.1, rf_class := factor('M')]
catmet_pacf[imrg_rf >= 0.1 & pacf_tao < 0.1, rf_class := factor('FA')]
catmet_pacf[imrg_rf <= 0.1 & pacf_tao <= 0.1, rf_class := factor('CN')]


catmet_pacf2 <- catmet_pacf[, .N, by = .(rf_class, sname, imrg_run)]
catmet_pacf_wide <- dcast(catmet_pacf2,  sname + imrg_run ~ rf_class)

catmet_pacf_wide[, `:=`(POD = H /(H + M), FAR = FA / (H + FA), CSI = H / (H + M + FA), 
                              tot_events = (H + M + FA + CN), ocn = factor('west_pacf')), 
                      by = .(sname, imrg_run)]

### plot

catmet_pacf_plot <- catmet_pacf_wide[, .(sname, imrg_run, POD, FAR, CSI)]
catmet_pacf_plot <- melt(catmet_pacf_plot, c("sname", "imrg_run"))

ggplot(catmet_pacf_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  ggtitle("all_datasets_pacfian_ocean (2001-20)") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/catmet_west_pacf.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# save the datasets ------------------------------------------------

metrics_pacf <- volmet_pacf[catmet_pacf_wide, on = .(sname, imrg_run, ocn)]
saveRDS(metrics_pacf, "./data/metrics_west_pacf.rds")

############################################
