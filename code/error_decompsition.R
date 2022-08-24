
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

##########

# categorical_matrices_POD,FAR,CSI ----------------------------------------

catmet_ind <- ind_rama_imrg_long[imrg_rf >= 0.1 & ind_rama >= 0.1, rf_class := factor('H')]
catmet_ind[imrg_rf < 0.1 & ind_rama >= 0.1, rf_class := factor('M')]
catmet_ind[imrg_rf >= 0.1 & ind_rama < 0.1, rf_class := factor('FA')]
catmet_ind[imrg_rf <= 0.1 & ind_rama <= 0.1, rf_class := factor('CN')]

########

catmet_ind

error_decom <- catmet_ind[, .(bias = sum(imrg_rf - ind_rama)/.N, 
                                     #rmse = sqrt(sum((imrg_rf - ind_rama)^2)/.N), 
                                     #mae = sum(abs(imrg_rf - ind_rama))/.N, 
                                     ocn = factor('ind')), by = .(sname, imrg_run, rf_class)]


hit_bias <- error_decom[imrg_run == "imrg_f" & rf_class == "H"]
false_alarm <- error_decom[imrg_run == "imrg_f" & rf_class == "FA"]
miss_bias <- error_decom[imrg_run == "imrg_f" & rf_class == "M"]
negt <- error_decom[imrg_run == "imrg_f" & rf_class == "CN"]

catmet_ind[imrg_run == "imrg_f" & rf_class == "CN"]

ggplot(hit_bias, aes(sname, bias)) + 
  geom_col() + 
  theme_small + 
  coord_flip()


ggplot(false_alarm, aes(sname, bias)) + 
  geom_col() + 
  theme_small + 
  coord_flip()

ggplot(miss_bias, aes(sname, bias)) + 
  geom_col() + 
  theme_small + 
  coord_flip()


bias_er <- error_decom[imrg_run == "imrg_f"]

ggplot(bias_er, aes(sname, bias)) + 
  geom_col() + 
  facet_wrap(~rf_class) + 
  theme_small + 
  coord_flip()




miss <- catmet_ind[rf_class == "FA" & imrg_run == "imrg_f"]

ggplot(miss[ind_rama >= 0], aes(x = ind_rama)) + 
  geom_histogram(aes(y = ..density..), 
                 colour = "black", fill = "blue", alpha = .2) + 
  #geom_density(aes(x = imrg_rf, y = ..density.., group = variable, colour = variable), lty = 1, size = 0.8) + 
  #facet_wrap(~ocn, ncol = 4) + 
  #coord_cartesian(xlim=c(0.1, 25)) +
  labs(x = "Precipitatin (mm/day)") +  
  theme_small + 
  theme(legend.title = element_blank())