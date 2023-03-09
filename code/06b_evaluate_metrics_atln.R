source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/functions.R')

#### read the datasets

atln_pirata_imrg <- readRDS("./data/atln_pirata_imrg.rds")

### preprocess

unique(atln_pirata_imrg, by = "sname")
atln_pirata_imrg <- atln_pirata_imrg[complete.cases(atln_pirata_imrg)]
atln_pirata_imrg <- atln_pirata_imrg[, .(lat, lon, date, imrg_e, imrg_l, imrg_f, obs = atln_pirata, sname)]
summary(atln_pirata_imrg)


# volumetric_metrices Biuas, RMSE, MAE -----------------------------------------

atln_pirata_imrg_long <- melt(atln_pirata_imrg, c("lat", "lon", "date", "sname", "obs"), 
                      value.name = "imrg_rf", variable.name = "imrg_run")

volmet_atln <-vol_stats(atln_pirata_imrg_long) 
volmet_atln[, `:=`(ocn = factor("Atlantic"))]

### plot

volmet_atln_plot <- melt(volmet_atln, c("sname", "imrg_run", "ocn"))

ggplot(volmet_atln_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

# ggsave("results/figures/volmet_atln.png",
#        width = 8.2, height = 6.3, units = "in", dpi = 600)


# categorical_matrices_POD,FAR,CSI ----------------------------------------

catmet_atln <- catmet_stats(atln_pirata_imrg_long, 0.1)
catmet_atln[, `:=`(ocn = factor("Atlantic"))]

### plot

catmet_atln_plot <- melt(catmet_atln, c("sname", "imrg_run", "ocn"))

ggplot(catmet_atln_plot, aes(fill = imrg_run, y = value, x = sname)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(variable ~ ., scales = "free") + 
  ggtitle("all_datasets_atlnian_ocean (2001-20)") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/catmet_atln.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


# save the datasets ------------------------------------------------

metrics_atln <- volmet_atln[catmet_atln, on = .(sname, imrg_run, ocn)]
saveRDS(metrics_atln, "./data/metrics_atln.rds")

############################################
