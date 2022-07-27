source('./source/libs.R')
source('./source/functions.R')


#### Import raw data 

#file_name <- list.files("./data/raw/imerg", pattern = ".nc4$", full.names = TRUE) %>% as.list()

file_name <- list.files("E:/IMERG/final", pattern = ".*3IMERG.2015.*\\.nc4$", full.names = TRUE) %>% as.list()


no_cores <- detectCores() - 1
cluster <-  parallel::makeCluster(no_cores, type = "PSOCK")

data_list <- parLapply(cluster, file_name, imerg_nc_dtable) # don't forget to select appropriate function form ./source/function.R 
parallel::stopCluster(cluster)
imerg_dtable <- rbindlist(data_list) #create a single data.table merging all the individual data.table lists

saveRDS(imerg_dtable, "./data/imerg_raw_2015.rds")



###################

# In case of memory limit error use the followings

memory.limit()
memory.limit(size=60000)






