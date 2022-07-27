
# Importing the OceanRAIN dataets for the pacific ocean  ------------------

source('./source/libs.R')
source('./source/functions.R')
library(ncdf4.helpers)


# read and convert the cdf into a data.table ------------------------------

ocnrain_w_roger <- ocnrain_dt('./data/raw/Oceanrain/pacific/OceanRAIN-W_Roger-Revelle_v1_n_1.nc')
ocnrain_w_sonne_I <- ocnrain_dt('./data/raw/Oceanrain/pacific/OceanRAIN-W_SonneI_v1_n_1.nc')
ocnrain_w_sonne_II <- ocnrain_dt('./data/raw/Oceanrain/pacific/OceanRAIN-W_SonneII_v1_n_1.nc')


saveRDS(ocnrain_w_roger, "./data/ocnrain_w_roger_pacf_raw.rds")
saveRDS(ocnrain_w_sonne_I, "./data/ocnrain_w_sonne_I_pacf_raw.rds")
saveRDS(ocnrain_w_sonne_II, "./data/ocnrain_w_sonne_II_pacf_raw.rds")

#################

