source('./source/libs.R')

# load the dataset --------------------------------------------------------

rama_rf <- readRDS("./data/rama_selected.rds")
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")


# Summary statistics ------------------------------------------------------

rama_stats <- rama_rf[, .(mean_day = round(mean(rf), 0),
                       sd_day = round(sd(rf), 0),
                       min_day = round(min(rf), 0),
                       max_day = round(max(rf), 0)), by = name]
head(rama_stats, 10)

ggplot(rama_rf, aes(rf)) +
  geom_histogram(fill = "#97B8C2") +
  facet_wrap(~name, scales = 'free') +
  theme_bw()



# data_trnasformation -----------------------------------------------------

rama_stats[, quantile(mean_day)]

# Aggregate ---------------------------------------------------------------
rama_rf[, year := year(date)]
rama_rf[, month := month(date)]

rama_month <- rama_rf[, .(rf = sum(rf)), by = .(month, year, name)]
rama_month[, date := as.Date(paste0(year, '-', month, '-1'))]


ggplot(rama_month, aes(x = factor(month), y = rf)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~name, scales = 'free') + 
  theme_bw()

#### annual

rama_year <- rama_rf[, .(rf = sum(rf)), by = .(year, name)]

ggplot(rama_year[year > 2009], aes(x = year, y = rf)) +
  geom_line(col =  colset_4[1]) +
  geom_point(col = colset_4[1]) + 
  facet_wrap(~name, scales = 'free') +
  theme_minimal()


rama_year[, rf_norm := (rf - mean(rf)) / sd(rf), by = name]
n_stations <- nrow(runoff_summary)

ggplot(rama_year[year > 2009], aes(x = year, y = rf_norm, col = name)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(values = colorRampPalette(colset_4)(39)) +
  theme_bw()

#############
coun <- rama_rf[, .(days = .N), by = .(name, year(date))]

st_0_80.5e <- rama_rf[name == "0_80.5e"]



ggplot(st_0_80.5e, aes(date, rf)) + 
  geom_line()

st_0_80.5e_days <- st_0_80.5e[, .(days = .N), by = .(name, year(date))]

ggplot(st_0_80.5e_days, aes(year, days)) + 
  geom_point() + 
  geom_line()

#####comparison

dat <- readRDS("./data/imerg_raw_2016.rds")

imerg_2016 <- dat[lon >= "80" & lon <= "81"]
imerg_2016 <- imerg_2016[lat >= "0" & lat <= "1"]


imerg_2016_round <- imerg_2016[, .SD, by = round(imerg_2016[, lat:precipitation], 2)]

imer_0_80.5e <- imerg_2016_round[lat == "0.05" & lon == "80.55"]
summary(imer_0_80.5e)


dat[, year := year(date_time_start)]
dat[, month := month(date_time_start)]

rama_2016 <- rama_rf[name == "0_80.5e" & year == "2016"]
summary(rama_2016)

rama_2016_rf <- rama_2016[, .(rf, date)]

ggplot(rama_2016_rf, aes(date, rf)) + 
  geom_line()

setnames(imer_0_80.5e, "date_time_start", "date")

imer_0_80.5e <- imer_0_80.5e[, .(date = ymd(date), lat, lon, precipitation)]

rama_imerg_rf <- rama_2016_rf[imer_0_80.5e, on = 'date']
rama_imerg_rf <- rama_imerg_rf[, .(date, lat, lon, rf = rf * 24, precipitation)]


to_plot <- gather(rama_imerg_rf, rain, values, rf:precipitation, factor_key=TRUE)




ggplot(to_plot, aes(date, values, col = rain)) + 
  geom_line()



library(mapview) 
library(sf) 
library(lubridate)
## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3
station_coords_sf <- st_as_sf(rama_rf, 
                              coords = c("lon", "lat"), 
                              crs = 4326)
mapview(station_coords_sf, map.types = 'Stamen.TerrainBackground')

