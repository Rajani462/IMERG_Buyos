source('./source/libs.R')
source('./source/functions.R')

##############################

# import the raw datasets -------------------------------------------------

ind_station <- readRDS("./data/ind_rama_stations.rds")

## Note: the order of the points should be lat and long but not long and lat
## Note: raster consider IMERG lat = x , and long = y, but in panoply and Arcgis 
## the other way around is true.

ind_station <- ind_station[, .(lat, lon)] 
ind_station_df <- as.data.frame(ind_station)


# IMERG_F ---------------------------------------------------------------


file_name <- list.files("E:/IMERG/final/", pattern = ".nc4$", 
                        full.names = TRUE) %>% as.list()

no_cores <- detectCores() - 1
cluster <-  parallel::makeCluster(no_cores, type = "PSOCK")

data_list <- parLapply(cluster, file_name, extract_grids, ind_station_df) #19 min 48 sec 
parallel::stopCluster(cluster)
imrgf_dt <- rbindlist(data_list)

saveRDS(imrgf_dt, './data/ind_imrgf_extract.rds')

###########################################################


# IMERG_L -----------------------------------------------------------------


file_name <- list.files("E:/IMERG/late/", pattern = ".nc4$", 
                        full.names = TRUE) %>% as.list()

no_cores <- detectCores() - 1
cluster <-  parallel::makeCluster(no_cores, type = "PSOCK")

data_list <- parLapply(cluster, file_name, extract_grids, ind_station_df)
parallel::stopCluster(cluster)
imrgl_dt <- rbindlist(data_list)


saveRDS(imrgl_dt, './data/ind_imrgl_extract.rds')

######################################################


# IMERG_E -----------------------------------------------------------------


file_name <- list.files("E:/IMERG/early/", pattern = ".nc4$", 
                         full.names = TRUE) %>% as.list()

no_cores <- detectCores() - 1
cluster <-  parallel::makeCluster(no_cores, type = "PSOCK")

data_list <- parLapply(cluster, file_name, extract_grids, ind_station_df)
parallel::stopCluster(cluster)
imrge_dt <- rbindlist(data_list)


saveRDS(imrge_dt, './data/ind_imrge_extract.rds')

####################################################################