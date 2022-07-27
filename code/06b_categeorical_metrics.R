source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

#### read the datasets

rf_imrgobs_dai <- readRDS('./data/rf_imrgobs_dai.rds')



# Categeorical_indices_POD_FAR_CSI ----------------------------------------

cat_metric <- rf_imrgobs_dai[imr_rf >= 0.5 & obs_rf >= 0.5, rf_class := factor('H')]
cat_metric[imr_rf < 0.5 & obs_rf >= 0.5, rf_class := factor('M')]
cat_metric[imr_rf >= 0.5 & obs_rf < 0.5, rf_class := factor('F')]
cat_metric[imr_rf <= 0.5 & obs_rf <= 0.5, rf_class := factor('CN')]


cat_metric_daily <- cat_metric[, .N, by = .(rf_class, stn_name)]

wider <- dcast(cat_metric_daily, stn_name ~ rf_class)

cat_met_daily <- wider[, `:=`(POD = H /(H + M), FAR = F / (H + F), CSI = H / (H + M + F), 
                              tot_events = (H + M + F + CN)), by = stn_name]

for_plot <- cat_met_daily[, .(stn_name, POD, FAR, CSI)]
for_plot <- melt(for_plot)

for_plot$stn_name <- factor(for_plot$stn_name, levels = c("-12_67e", "-8_55e", "-8_67e", "-8_80.5e", "-8_95e", "-5_95e", "-4_67e", "-4_80.5e", 
                                                        "-1.5_80.5e", "0_80.5e", "0_90e", "4_90e", "8_90e", "12_90e", "15_90e"))


ggplot(for_plot, aes(factor(stn_name), value,  fill = variable)) + 
  geom_bar(stat =  "identity", position=position_dodge()) + 
  labs(x='station name') + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2))

ggsave("results/figures/POD_FAR_CSi_rama.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)


#### monthly scale (mm/day)

cat_metric_mon <- cat_metric[, .N, by = .(rf_class, stn_name, year, month)]

wider_mon <- dcast(cat_metric_mon, stn_name + month + year ~ rf_class)
unique (unlist (lapply (wider_mon, function (x) which (is.na (x)))))
wider_mon[is.na(wider_mon)] <- 0

cat_met_mon <- wider_mon[, `:=`(POD = H /(H + M), FAR = F / (H + F), CSI = H / (H + M + F), 
                                tot_events = (H + M + F + CN)), by = .(stn_name, month, year)]

ggplot(cat_met_mon, aes(factor(month), POD)) + 
  geom_boxplot() +
  facet_wrap(~stn_name) + 
  theme_very_small

ggsave("results/figures/POD_rama.png",
       width = 8.2, height = 6.3, units = "in", dpi = 600)

longer <- melt(cat_met_mon, id.vars = c("stn_name", "month", "year"), 
               measure.vars = c("POD", "FAR"), variable.name = "metrics")


abc <- longer[stn_name == '15_90e']

ggplot(longer, aes(factor(month), value, fill = metrics)) + 
  geom_boxplot() +
  facet_wrap(~stn_name) + 
  theme_generic


################## functions for different threshold


cat_ann <- function(data, threshold){
  
  data <- data[imr_rf >= threshold & obs_rf >= threshold, event := factor('H')]
  data[imr_rf < threshold & obs_rf >= threshold, event := factor('M')]
  data[imr_rf >= threshold & obs_rf < threshold, event := factor('F')]
  data[imr_rf < threshold & obs_rf < threshold, event := factor('CN')]
  
  data <- data[, .N, by = .(event, stn_name, year(date))]
  
  wider_data <- dcast(data, stn_name + year ~ event)
  unique (unlist (lapply (wider_data, function (x) which (is.na (x)))))
  wider_data[is.na(wider_data)] <- 0
  
  wider_data[, `:=`(POD = H /(H + M), FAR = F / (H + F), CSI = H / (H + M + F), 
                                 tot_events = (H + M + F + CN)), by = .(stn_name, year)]
  wider_data2 <- wider_data[, `:=`(rate = factor(threshold))]
  return(wider_data2)
  
}

categ <- rf_imrgobs_dai[, .(lon_imrg, lat_imrg, stn_name, date, imr_rf, obs_rf)]

rf_2 <- cat_ann(categ, 2)
rf_5 <- cat_ann(categ, 5)
rf_7 <- cat_ann(categ, 7)
rf_10 <- cat_ann(categ, 10)
rf_15 <- cat_ann(categ, 15)
rf_20 <- cat_ann(categ, 20)

dim(rf_15)
all <- bind_rows(rf_2, rf_5, rf_7, rf_10, rf_15, rf_20)

dim(all)

ggplot(all, aes(factor(rate), POD)) + 
  geom_boxplot() +
  facet_wrap(~stn_name)

#### daily (single value for a station)

cat_ind_daily <- function(data, threshold){
  
  data <- data[imr_rf >= threshold & obs_rf >= threshold, event := factor('H')]
  data[imr_rf < threshold & obs_rf >= threshold, event := factor('M')]
  data[imr_rf >= threshold & obs_rf < threshold, event := factor('F')]
  data[imr_rf < threshold & obs_rf < threshold, event := factor('CN')]
  
  data <- data[, .N, by = .(event, stn_name)]
  
  wider_data <- dcast(data, stn_name ~ event)
  unique (unlist (lapply (wider_data, function (x) which (is.na (x)))))
  wider_data[is.na(wider_data)] <- 0
  
  wider_data[, `:=`(POD = H /(H + M), FAR = F / (H + F), CSI = H / (H + M + F), 
                    tot_events = (H + M + F + CN)), by = .(stn_name)]
  wider_data2 <- wider_data[, `:=`(rate = factor(threshold))]
  return(wider_data2)
  
}

rf_2 <- cat_ind_daily(categ, 2)
rf_5 <- cat_ind_daily(categ, 5)
rf_7 <- cat_ind_daily(categ, 7)
rf_10 <- cat_ind_daily(categ, 10)
rf_15 <- cat_ind_daily(categ, 15)
rf_20 <- cat_ind_daily(categ, 20)

dim(rf_15)
all <- bind_rows(rf_2, rf_5, rf_7, rf_10, rf_15, rf_20)

dim(all)

ggplot(all, aes(rate, POD, group = 1)) + 
  geom_line() + 
  geom_point() + 
  #geom_line() + 
  facet_wrap(~stn_name)

ggplot(all, aes(rate, POD)) + 
  geom_bar(stat = "identity") + 
  #geom_line() + 
  facet_wrap(~stn_name) + 
  labs(x = "Threshold (mm/day)") + 
  theme_small
  
ggsave("results/figures/POD_intensities.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)


############################3





library(verification)
new <- edit(table.stats)
