source('./source/libs.R')
source('./source/functions.R')

##############################

# import the raw datasets -------------------------------------------------

pacf_station <- readRDS("./data/pacf_tao_stations.rds")

## Note: the order of the points should be lat and long but not long and lat
## Note: raster consider IMERG lat = x , and long = y, but in panoply and Arcgis 
## the other way around is true.

pacf_station <- pacf_station[, .(lat, lon)]
pacf_station$lon[pacf_station$lon == 180] <- 179.95 # 180 replaced by 179.95 as this is the last grid of IMERG (keeping 180 gives NA's)
pacf_station_df <- as.data.frame(pacf_station)


# IMERG_F ---------------------------------------------------------------


file_name <- list.files("E:/IMERG/final/", pattern = ".nc4$", 
                        full.names = TRUE) %>% as.list()

no_cores <- detectCores() - 1
cluster <-  parallel::makeCluster(no_cores, type = "PSOCK")

data_list <- parLapply(cluster, file_name, extract_grids, pacf_station_df) #19 min 48 sec 
parallel::stopCluster(cluster)
imrgf_dt <- rbindlist(data_list)

imrgf_dt$y[imrgf_dt$y == 179.95] <- 180 # get back the original longitude (180) for buoys (column 'y') 

saveRDS(imrgf_dt, './data/pacf_imrgf_extract.rds')

###########################################################


# IMERG_L -----------------------------------------------------------------


file_name <- list.files("E:/IMERG/late/", pattern = ".nc4$", 
                        full.names = TRUE) %>% as.list()

no_cores <- detectCores() - 1
cluster <-  parallel::makeCluster(no_cores, type = "PSOCK")

data_list <- parLapply(cluster, file_name, extract_grids, pacf_station_df)
parallel::stopCluster(cluster)
imrgl_dt <- rbindlist(data_list)

imrgl_dt$y[imrgl_dt$y == 179.95] <- 180 # get back the original longitude (180) for buoys (column 'y') 

saveRDS(imrgl_dt, './data/pacf_imrgl_extract.rds')

######################################################


# IMERG_E -----------------------------------------------------------------


file_name <- list.files("E:/IMERG/early/", pattern = ".nc4$", 
                         full.names = TRUE) %>% as.list()

no_cores <- detectCores() - 1
cluster <-  parallel::makeCluster(no_cores, type = "PSOCK")

data_list <- parLapply(cluster, file_name, extract_grids, pacf_station_df)
parallel::stopCluster(cluster)
imrge_dt <- rbindlist(data_list)

imrge_dt$y[imrge_dt$y == 179.95] <- 180 # get back the original longitude (180) for buoys (column 'y') 

saveRDS(imrge_dt, './data/pacf_imrge_extract.rds')

####################################################################