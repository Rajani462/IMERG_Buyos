source('./source/libs.R')
source('./source/functions.R')

################

ind_rama_files <- list.files("./data/raw/buoys/rama_indian", pattern = "cdf$", full.names = TRUE) %>% as.list()
atln_pirata_files <- list.files("./data/raw/buoys/pirata_atlantic", pattern = "cdf$", full.names = TRUE) %>% as.list()
pacf_tao_files <- list.files("./data/raw/buoys/tao_pacific", pattern = "cdf$", full.names = TRUE) %>% as.list()



ind_rama_list <- lapply(ind_rama_files, data_nc)
ind_rama_dtable <- rbindlist(ind_rama_list) #create a single data.table merging all the individual data.table lists

atln_pirata_list <- lapply(atln_pirata_files, data_nc)
atln_pirata_dtable <- rbindlist(atln_pirata_list)

pacf_tao_list <- lapply(pacf_tao_files, data_nc)
pacf_tao_dtable <- rbindlist(pacf_tao_list)


saveRDS(ind_rama_dtable, "./data/ind_rama_raw.rds")
saveRDS(atln_pirata_dtable, "./data/atln_pirata_raw.rds")
saveRDS(pacf_tao_dtable, "./data/pacf_tao_raw.rds")

##### ----------------------------------------------------------------

