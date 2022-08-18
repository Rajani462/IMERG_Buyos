source('./source/libs.R')
source('./source/functions.R')
source('./source/themes.R')
source('./source/palettes.R')
library(viridis)
library(ggpointdensity)

#### read the datasets --------------------------------


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

ind_alt_pacf_meanplot <- melt(ind_atln_pacf_mean, c("date", 'ocn', 'buyos'), value.name = "imrg_rf")


data_f <- ind_alt_pacf_meanplot[variable == "IMERG-F"]


# calculate the frequnncies -------------------------------------------------


data_freq <- data_f

### for buoys

data_freq[buyos < 0.1, rf_class := factor('no rain')]
data_freq[buyos > 0.1 & buyos <= 1, rf_class := factor('0.1-1')]
data_freq[buyos > 1 & buyos <= 2, rf_class := factor('1-2')]
data_freq[buyos > 2 & buyos <= 3, rf_class := factor('2-3')]
data_freq[buyos > 3 & buyos <= 4, rf_class := factor('3-4')]
data_freq[buyos > 4 & buyos <= 5, rf_class := factor('4-5')]
data_freq[buyos > 5 & buyos <= 10, rf_class := factor('5-10')]
data_freq[buyos > 10 & buyos <= 20, rf_class := factor('10-20')]
data_freq[buyos > 20 & buyos <= 30, rf_class := factor('20-30')]
data_freq[buyos > 30, rf_class := factor('>30')]

### for imerg

data_freq[imrg_rf < 0.1, imrg_class := factor('no rain')]
data_freq[imrg_rf > 0.1 & imrg_rf <= 1, imrg_class := factor('0.1-1')]
data_freq[imrg_rf > 1 & imrg_rf <= 2, imrg_class := factor('1-2')]
data_freq[imrg_rf > 2 & imrg_rf <= 3, imrg_class := factor('2-3')]
data_freq[imrg_rf > 3 & imrg_rf <= 4, imrg_class := factor('3-4')]
data_freq[imrg_rf > 4 & imrg_rf <= 5, imrg_class := factor('4-5')]
data_freq[imrg_rf > 5 & imrg_rf <= 10, imrg_class := factor('5-10')]
data_freq[imrg_rf > 10 & imrg_rf <= 20, imrg_class := factor('10-20')]
data_freq[imrg_rf > 20 & imrg_rf <= 30, imrg_class := factor('20-30')]
data_freq[imrg_rf > 30, imrg_class := factor('>30')]


# plot --------------------------------------------------------------------


freq_plot <- data_freq[, .(imrg_rf, buyos, rf_class, imrg_class, ocn)]

# data_obs <- freq_plot %>% group_by(rf_class, ocn) %>% summarize(n=n()) %>% mutate(freq=n/sum(n)) 
# data_imrg <- freq_plot %>% group_by(imrg_class, ocn) %>% summarize(n=n()) %>% mutate(freq=n/sum(n))

data_obs <- freq_plot[, .(n = .N), by = .(rf_class, ocn)]
data_obs[, `:=`(freq = n/sum(n)), by = .(ocn)]

data_imrg <- freq_plot[, .(n = .N), by = .(imrg_class, ocn)]
data_imrg[, `:=`(freq = n/sum(n)), by = .(ocn)]


data_obs[, type := factor('obs')]
data_imrg <- data_imrg[, .(rf_class = imrg_class, ocn, n, freq, type = factor('imrg'))]


imrg_obs <- rbind(data_obs, data_imrg)

ggplot(imrg_obs, aes(rf_class, freq, fill = type)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "Precipitation intensities (mm/day)", y = "Frequency") + 
  theme_small + 
  facet_wrap(~ocn) + 
  #scale_fill_discrete(name = "", labels = c("Buoys", "IMERG")) + 
  scale_fill_manual(values = c("#4682B4", "#FF8247"), name = "", labels = c("Buoys", "IMERG")) + 
  theme(legend.position = c(0.07, 0.90)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 0.9))

ggsave("results/paper_fig/freqency_ind_atln_pacf.png",
       width = 7.2, height = 5.2, units = "in", dpi = 600)


# volume vs intensity -----------------------------------------------------


obs_vol <- freq_plot[rf_class != "no rain", .(type = factor('buyos'), amount = sum(buyos)), by = .(rf_class, ocn)]
imrg_vol <- freq_plot[imrg_class != "no rain", .(type = factor('imrg'), amount = sum(imrg_rf)), by = .(imrg_class, ocn)]


obs_vol[, perc:= round(amount/sum(amount), 2), by = .(ocn)]
imrg_vol[, perc:= round(amount/sum(amount), 2), by = .(ocn)]

setnames(imrg_vol, "imrg_class", "rf_class")

imrg_obs_vol <- rbind(obs_vol, imrg_vol)

ggplot(imrg_obs_vol, aes(rf_class, perc, fill = type)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "Precipitation intensities (mm/day)", y = "Volume") + 
  facet_wrap(~ocn) + 
  theme_small + 
  #scale_fill_discrete(name = "", labels = c("Buoys", "IMERG")) + 
  scale_fill_manual(values = c("#4682B4", "#FF8247"), name = "", labels = c("Buoys", "IMERG")) + 
  theme(legend.position = c(0.07, 0.90)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 0.9))

ggsave("results/paper_fig/volume_ind_atln_pacf.png",
       width = 7.2, height = 5.2, units = "in", dpi = 600)

################################################################################