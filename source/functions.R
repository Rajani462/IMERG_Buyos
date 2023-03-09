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


# function to estimate the volumetric & categeorical scores----------------------------------------------------------------


vol_stats <- function(x){ 
  x[, .(ref_mean = mean(obs, na.rm = TRUE), 
        bias = sum(imrg_rf - obs)/.N, 
        rbias = ((sum(imrg_rf - obs)/.N)/mean(obs, na.rm = TRUE)),
        rbias2 = ((sum(imrg_rf - obs)/.N)/mean(obs, na.rm = TRUE))*100,
        rmse = sqrt(sum((imrg_rf - obs)^2)/.N), 
        nrmse = (sqrt(sum((imrg_rf - obs)^2)/.N)/mean(obs, na.rm = TRUE)), 
        nrmse2 = (sqrt(sum((imrg_rf - obs)^2)/.N)/sqrt(sum(obs)))*100, 
        mae = sum(abs(imrg_rf - obs))/.N, 
        nmae = ((sum(abs(imrg_rf - obs))/.N) / mean(obs, na.rm = TRUE)), 
        nmae2 = ((sum(abs(imrg_rf - obs))/.N) / IQR(obs, na.rm = TRUE))*100,
        cor = cor(imrg_rf, obs)), by = .(sname, imrg_run)]
}


catmet_stats <- function(dt, thld){ 
  
  cat_metric_f <- dt[imrg_rf >= thld & obs >= thld, rf_class := factor('H')]
  cat_metric_f[imrg_rf < thld & obs >= thld, rf_class := factor('M')]
  cat_metric_f[imrg_rf >= thld & obs < thld, rf_class := factor('FA')]
  cat_metric_f[imrg_rf < thld & obs < thld, rf_class := factor('CN')]
  
  cat_metric_daily_f <- cat_metric_f[, .N, by = .(rf_class, imrg_run, sname)]
  cat_metric_wide <- dcast(cat_metric_daily_f, imrg_run + sname ~ rf_class)
  
  cat_met <- cat_metric_wide[, `:=`(POD = H /(H + M), FAR = FA / (H + FA), CSI = H / (H + M + FA), 
                                    tot_events = (H + M + FA + CN)), by = .(sname, imrg_run)]
  
  cat_met <- cat_met[, .(sname, imrg_run, POD, FAR, CSI)]
  
  return(cat_met)
  
}

# generate seasonal precipitation (sum) from monthly ----------------------

add_seasons <- function(dt){
  dt[month == 12 | month == 1 | month == 2, season := 'winter']
  dt[month == 3 | month == 4 | month == 5, season := 'spring']
  dt[month == 6 | month == 7 | month == 8, season := 'summer']
  dt[month == 9 | month == 10 | month == 11, season := 'autumn']
  dt[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]
  
}


# create colorscale -------------------------------------------------------

plot_discrete_cbar = function(
    breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
    palette = "Greys", # RColorBrewer palette to use
    colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette), # Alternatively, manually set colors
    direction = 1, # Flip colors? Can be 1 or -1
    spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
    border_color = NA, # NA = no border color
    legend_title = NULL,
    legend_direction = "horizontal", # Can be "horizontal" or "vertical"
    font_size = 5,
    expand_size = 1, # Controls spacing around legend plot
    spacing_scaling = 1, # Multiplicative factor for label and legend title spacing
    width = 0.1, # Thickness of color bar
    triangle_size = 0.1 # Relative width of +-Inf triangles
) {
  require(ggplot2)
  if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
  if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
  if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
  breaks = as.numeric(breaks)
  new_breaks = sort(unique(breaks))
  if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
  breaks = new_breaks
  if (class(colors) == "function") colors = colors(length(breaks) - 1)
  if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
  if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")
  
  if (direction == -1) colors = rev(colors)
  
  inf_breaks = which(is.infinite(breaks))
  if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
  plotcolors = colors
  
  n_breaks = length(breaks)
  
  labels = breaks
  
  if (spacing == "constant") {
    breaks = 1:n_breaks
  }
  
  r_breaks = range(breaks)
  
  cbar_df = data.frame(stringsAsFactors = FALSE,
                       y = breaks,
                       yend = c(breaks[-1], NA),
                       color = as.character(1:n_breaks)
  )[-n_breaks,]
  
  xmin = 1 - width/2
  xmax = 1 + width/2
  
  cbar_plot = ggplot(cbar_df, aes(xmin=xmin, xmax = xmax, ymin = y, ymax = yend, fill = factor(color, levels = 1:length(colors)))) +
    geom_rect(show.legend = FALSE,
              color=border_color)
  
  if (any(inf_breaks == 1)) { # Add < arrow for -Inf
    firstv = breaks[1]
    polystart = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-1]
    cbar_plot = cbar_plot +
      geom_polygon(data=polystart, aes(x=x, y=y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[1],
                   color=border_color)
  }
  if (any(inf_breaks > 1)) { # Add > arrow for +Inf
    lastv = breaks[n_breaks]
    polyend = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-length(plotcolors)]
    cbar_plot = cbar_plot +
      geom_polygon(data=polyend, aes(x=x, y=y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[length(colors)],
                   color=border_color)
  }
  
  if (legend_direction == "horizontal") { #horizontal legend
    mul = 1
    x = xmin
    xend = xmax
    cbar_plot = cbar_plot + coord_flip()
    angle = 0
    legend_position = xmax + 0.1 * spacing_scaling
  } else { # vertical legend
    mul = -1
    x = xmax
    xend = xmin
    angle = -90
    legend_position = xmax + 0.2 * spacing_scaling
  }
  
  cbar_plot = cbar_plot +
    geom_segment(data=data.frame(y = breaks, yend = breaks),
                 aes(y=y, yend=yend),
                 x = x - 0.05 * mul * spacing_scaling, xend = xend,
                 inherit.aes = FALSE) +
    annotate(geom = 'text', x = x - 0.1 * mul * spacing_scaling, y = breaks,
             label = labels,
             size = font_size) +
    scale_x_continuous(expand = c(expand_size,expand_size)) +
    scale_fill_manual(values=plotcolors) +
    theme_void()
  
  if (!is.null(legend_title)) { # Add legend title
    cbar_plot = cbar_plot +
      annotate(geom = 'text', x = legend_position, y = mean(r_breaks),
               label = legend_title,
               angle = angle,
               size = font_size)
  }
  
  cbar_plot
}

########################################################################################
