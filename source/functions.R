source('./source/libs.R')


# buoys_data --------------------------------------------------------------
# get a data.table from a .cdf or .nc file --------------------------------


data_nc <- function(file){
  rain_dat <- nc_open(file)
  lon <- ncvar_get(rain_dat, "lon")
  lat <- ncvar_get(rain_dat, "lat")
  rf <-  ncvar_get(rain_dat, "RN_485")
  qrn <- ncvar_get(rain_dat, "QRN_5485") # precipitation quality code
  srn <- ncvar_get(rain_dat, "SRN_6485") # precipitation source code
  
  tunits <- ncatt_get(rain_dat, "time", attname="units")
  tustr <- strsplit(tunits$value, " ")
  date <- (origin = unlist(tustr)[3])
  date <- as.Date(date) + rain_dat$dim$time$vals
  
  dat <- data.table(lon, lat, date, rf, qrn, srn)
  return(dat)
  
}



# imerg -------------------------------------------------------------------
# convert the imerg nc4 files into data.table format

imerg_nc_dtable <- function(file){
  imer <- ncdf4::nc_open(file)
  lon <- ncdf4::ncvar_get(imer, "lon")
  lat <- ncdf4::ncvar_get(imer, "lat")
  rf.array <-  ncdf4::ncvar_get(imer, "precipitationCal")
  
  nc_atts <- ncdf4::ncatt_get(imer, 0)
  date_time_start <- as.POSIXlt(nc_atts$BeginDate)
  
  dimnames(rf.array)[[1]] <- imer$dim$lat$vals
  dimnames(rf.array)[[2]] <- imer$dim$lon$vals
  
  ncdf4::nc_close(imer)
  
  #rf.array_tropic <- rf.array[700:1100, ] #-20S to 19.95N & -180 to 180 (adjust it according to the needs)
  rf.array_tropic <- rf.array[650:1150, ] #--25.05S to 24.95N
  precip_summary <- data.table::data.table(reshape2::melt(rf.array_tropic,
                                                          varnames = c("lat", "lon"),
                                                          value.name = "precipitation"))
  
  
  precip_summary2 <- cbind(precip_summary, date_time_start)
  
  return(precip_summary2)
  
}

### for western_pacific -----------

imerg_nc_dtable_pacf_w <- function(file){
  imer <- ncdf4::nc_open(file)
  lon <- ncdf4::ncvar_get(imer, "lon")
  lat <- ncdf4::ncvar_get(imer, "lat")
  rf.array <-  ncdf4::ncvar_get(imer, "precipitationCal")
  
  nc_atts <- ncdf4::ncatt_get(imer, 0)
  date_time_start <- as.POSIXlt(nc_atts$BeginDate)
  
  dimnames(rf.array)[[1]] <- imer$dim$lat$vals
  dimnames(rf.array)[[2]] <- imer$dim$lon$vals
  
  ncdf4::nc_close(imer)
  
  
  rf.array_tropic <- rf.array[800:1000, 3250:3600] #-10S to 9.95N & +145 to 180 (adjust it according to the needs)
  
  
  precip_summary <- data.table::data.table(reshape2::melt(rf.array_tropic,
                                                          varnames = c("lat", "lon"),
                                                          value.name = "precipitation"))
  
  
  precip_summary2 <- cbind(precip_summary, date_time_start)
  
  return(precip_summary2)
  
}




# subset selected lat_long from IMERG data.table --------------------------
## data- IMERG data.table
## dist- unique lat_long data.table


subset_dat <- function(data, station){
  for(i in 1:nrow(station))
    sub2 <- data[lat >= station$lat[i] & lat <= (station$lat[i] + 0.07) & 
                   lon >= station$lon[i] & lon <= (station$lon[i] + 0.07), sname := factor(station$sname[i])] 
  sub2 <- sub2[complete.cases(sub2),]
  #sub2[complete.cases(sub2),]
  return(sub2)
}



# OceanRAIN_data --------------------------------------------------------------
# get a data.table from a .cdf or .nc file --------------------------------

ocnrain_dt <- function(file){
  rain_dat <- ncdf4::nc_open(file)
  lon <- ncdf4::ncvar_get(rain_dat, "longitude")
  lat <- ncdf4::ncvar_get(rain_dat, "latitude")
  rf <-  ncdf4::ncvar_get(rain_dat, "ODM470_precipitation_rate_R")
  
  date <- ncdf4.helpers::nc.get.time.series(rain_dat)
  
  dat_dt <- data.table(lon, lat, date, rf)
  dat_dt$date <- as.POSIXct(dat_dt$date, format = '%d%b%Y:%H:%M:%S') # change the date posixct format

  return(dat_dt)
    
}


# function to extract grids overlaying points------------------------------------
## extracts IMERG pixels by buoys lat long

extract_grids <- function(file, points){
  imerg_rast <-terra::rast(file)
  imerg_rast <- terra::subset(imerg_rast, "precipitationCal")
  terra::crs(imerg_rast) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
  
  ext_df <- as.data.frame(terra::extract(imerg_rast, points, 
                                         xy = TRUE, cells = TRUE))
  
  date <- lubridate::ymd(gsubfn::strapplyc(file, "[.]\\d{8}[-]", simplify = TRUE))
  
  ext_df_coords <- cbind(ext_df, terra::xyFromCell(imerg_rast, ext_df[,3]), date)
  ext_df_coords <- data.table::as.data.table(ext_df_coords)
  
  return(ext_df_coords)
  
}


# generate seasonal precipitation (sum) from monthly ----------------------

add_seasons <- function(dt){
  dt[month == 12 | month == 1 | month == 2, season := 'winter']
  dt[month == 3 | month == 4 | month == 5, season := 'spring']
  dt[month == 6 | month == 7 | month == 8, season := 'summer']
  dt[month == 9 | month == 10 | month == 11, season := 'autumn']
  dt[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]
  
}

