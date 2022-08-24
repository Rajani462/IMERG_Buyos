
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



# categorical_matrices_POD,FAR,CSI ----------------------------------------

catmet_ocn <- ind_atln_pacf_meanplot[imrg_rf >= 0.1 & buyos >= 0.1, rf_class := factor('H')]
catmet_ocn[imrg_rf < 0.1 & buyos >= 0.1, rf_class := factor('M')]
catmet_ocn[imrg_rf >= 0.1 & buyos < 0.1, rf_class := factor('FA')]
catmet_ocn[imrg_rf <= 0.1 & buyos <= 0.1, rf_class := factor('CN')]

########

catmet_ocn
setnames(catmet_ocn, "variable", "imrg_run")
error_decom <- catmet_ocn[, .(bias = sum(imrg_rf - buyos)/.N), by = .(ocn, imrg_run, rf_class)]

error_total <- catmet_ocn[, .(rf_class = factor('tot_bias'), bias = sum(imrg_rf - buyos)/.N), by = .(ocn, imrg_run)]

# join 

error_plot <- rbind(error_decom, error_total)
levels(error_plot$rf_class) <- c("Hit bias", "Missed bias", "False alarm bias", "CN", 
                                "Total bias")

### plot

ggplot(error_plot[rf_class != "CN"], aes(ocn, bias, fill = imrg_run)) + 
  geom_col(position=position_dodge()) + 
  facet_wrap(~rf_class, scales = "free_x", ncol = 4) + 
  labs(x = "Ocean", y = "Bias (mm/day)") + 
  scale_fill_manual(values = c("808000",  "#D35C37", "#6590bb")) + 
  theme_small + 
  coord_flip() + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave("results/paper_fig/error_decomp.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)


ggplot(error_plot[rf_class != "CN" & imrg_run == "IMERG-F"], aes(ocn, bias, fill = ocn)) + 
  geom_col() + 
  facet_wrap(~rf_class, ncol = 4) + 
  labs(x = "Ocean", y = "Bias (mm/day)") + 
  scale_fill_manual(values = c("808000",  "#D35C37", "#6590bb", "#b64925")) + 
  theme_small + 
  coord_flip() + 
  theme(legend.position = "none",
        legend.title = element_blank())


ggsave("results/paper_fig/error_decomp_f.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)

###########################################################



false_alrms <- catmet_ocn[imrg_run == "IMERG-F" & rf_class == "FA"]

ggplot(false_alrms[imrg_run == "IMERG-F"], aes(buyos)) + 
  geom_bar() + 
  facet_grid(~ocn)
  
