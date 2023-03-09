source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/functions.R')

#### read the datasets

pacf_tao_imrg <- readRDS("./data/pacf_tao_imrg.rds")

### preprocess

pacf_tao_imrg <- pacf_tao_imrg[lon > 0]
unique(pacf_tao_imrg, by = "sname")
pacf_tao_imrg <- pacf_tao_imrg[complete.cases(pacf_tao_imrg)]
pacf_tao_imrg <- pacf_tao_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = pacf_tao, sname)]
summary(pacf_tao_imrg)


# volumetric_metrices Biuas, RMSE, MAE -----------------------------------------

pacf_tao_imrg_long <- melt(pacf_tao_imrg, c("lat", "lon", "date", "sname", "obs"), 
                           value.name = "imrg_rf", variable.name = "imrg_run")

volmet_pacf <- vol_stats(pacf_tao_imrg_long)
volmet_pacf[, `:=`(ocn = factor("West Pacific"))]
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

catmet_pacf <- catmet_stats(pacf_tao_imrg_long, 0.1)
catmet_pacf[, `:=`(ocn = factor("West Pacific"))]

### plot

catmet_pacf_plot <- melt(catmet_pacf, c("sname", "imrg_run", "ocn"))

ggplot(catmet_pacf_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  ggtitle("all_datasets_pacfian_ocean (2001-20)") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/catmet_west_pacf.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# save the datasets ------------------------------------------------

metrics_pacf <- volmet_pacf[catmet_pacf, on = .(sname, imrg_run, ocn)]
saveRDS(metrics_pacf, "./data/metrics_west_pacf.rds")

############################################
