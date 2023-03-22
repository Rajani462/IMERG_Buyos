source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/functions.R')

#### read the datasets

ind_rama_imrg <- readRDS("./data/ind_rama_imrg.rds")

### preprocess

unique(ind_rama_imrg, by = "sname")
ind_rama_imrg <- ind_rama_imrg[complete.cases(ind_rama_imrg)]
ind_rama_imrg <- ind_rama_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = ind_rama, sname)]
summary(ind_rama_imrg)


# volumetric_metrices Biuas, RMSE, MAE -----------------------------------------

ind_rama_imrg_long <- melt(ind_rama_imrg, c("lat", "lon", "date", "sname", "obs"), 
                      value.name = "imrg_rf", variable.name = "imrg_run")

volmet_ind <- vol_stats(ind_rama_imrg_long)
volmet_ind[, `:=`(ocn = factor("Indian"))]

### plot

volmet_ind_plot <- melt(volmet_ind, c("sname", "imrg_run", "ocn"))

ggplot(volmet_ind_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggplot(volmet_ind_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/volmet_ind.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# categorical_matrices_POD,FAR,CSI ----------------------------------------

catmet_ind <- catmet_stats(ind_rama_imrg_long, 0.1)
catmet_ind[, `:=`(ocn = factor("Indian"))]

### plot


catmet_ind_plot <- melt(catmet_ind, c("sname", "imrg_run", "ocn"))

ggplot(catmet_ind_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  ggtitle("all_datasets_indian_ocean (2004-20)") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/catmet_ind.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# save the datasets ------------------------------------------------

metrics_ind <- volmet_ind[catmet_ind, on = .(sname, imrg_run, ocn)]
saveRDS(metrics_ind, "./data/metrics_ind.rds")

############################################
