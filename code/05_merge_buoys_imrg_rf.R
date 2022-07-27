source('./source/libs.R')


# Indian_ocean ------------------------------------------------------------

#### read the datasets

ind_imrgf <- readRDS("./data/ind_imrgf_extract.rds")
ind_imrgl <- readRDS("./data/ind_imrgl_extract.rds")
ind_imrge <- readRDS("./data/ind_imrge_extract.rds")

ind_rama <- readRDS("./data/ind_rama.rds")

######
setnames(ind_imrgf, c("precipitationCal", "x", "y"), c("imrg_f", "lat", "lon"))
setnames(ind_imrgl, "precipitationCal", "imrg_l")
setnames(ind_imrge, "precipitationCal", "imrg_e")

setnames(ind_rama, "rf", "ind_rama")

ind_imrg <- cbind(ind_imrgf, ind_imrgl, ind_imrge)
ind_imrg <- ind_imrg[, .(lat, lon, x, y, date, imrg_e, imrg_l, imrg_f)]


rama_imrg_merge <- ind_imrg[ind_rama, on = .(lat, lon, date)]

saveRDS(rama_imrg_merge, './data/ind_rama_imrg.rds')

###############################################


# Atlantic_ocean ------------------------------------------------------------

### read the datasets

atln_imrgf <- readRDS("./data/atln_imrgf_extract.rds")
atln_imrgl <- readRDS("./data/atln_imrgl_extract.rds")
atln_imrge <- readRDS("./data/atln_imrge_extract.rds")

atln_pirata <- readRDS("./data/atln_pirata.rds")

###
setnames(atln_imrgf, c("precipitationCal", "x", "y"), c("imrg_f", "lat", "lon"))
setnames(atln_imrgl, "precipitationCal", "imrg_l")
setnames(atln_imrge, "precipitationCal", "imrg_e")

setnames(atln_pirata, "rf", "atln_pirata")

atln_imrg <- cbind(atln_imrgf, atln_imrgl, atln_imrge)
atln_imrg <- atln_imrg[, .(lat, lon, x, y, date, imrg_e, imrg_l, imrg_f)]


pirata_imrg_merge <- atln_imrg[atln_pirata, on = .(lat, lon, date)]
pirata_imrg_merge <- pirata_imrg_merge[date >= "2000-06-01"] # as IMERG don't have data before this

saveRDS(pirata_imrg_merge, './data/atln_pirata_imrg.rds')

#######################################################

# Pacific_ocean ------------------------------------------------------------

### read the datasets

pacf_imrgf <- readRDS("./data/pacf_imrgf_extract.rds")
pacf_imrgl <- readRDS("./data/pacf_imrgl_extract.rds")
pacf_imrge <- readRDS("./data/pacf_imrge_extract.rds")

pacf_tao <- readRDS("./data/pacf_tao.rds")

###
setnames(pacf_imrgf, c("precipitationCal", "x", "y"), c("imrg_f", "lat", "lon"))
setnames(pacf_imrgl, "precipitationCal", "imrg_l")
setnames(pacf_imrge, "precipitationCal", "imrg_e")

setnames(pacf_tao, "rf", "pacf_tao")

pacf_imrg <- cbind(pacf_imrgf, pacf_imrgl, pacf_imrge)
pacf_imrg <- pacf_imrg[, .(lat, lon, x, y, date, imrg_e, imrg_l, imrg_f)]


tao_imrg_merge <- pacf_imrg[pacf_tao, on = .(lat, lon, date)]
tao_imrg_merge <- tao_imrg_merge[date >= "2000-06-01"] # as IMERG don't have data before this

saveRDS(tao_imrg_merge, './data/pacf_tao_imrg.rds')


###################################################
