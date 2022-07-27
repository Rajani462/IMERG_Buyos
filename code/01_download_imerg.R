###########################
library(getPass)
library(dplyr)
library(stringr)
library(xml2)
library(RCurl)
library(XML)
library(stringr)
library(plyr)
library(rvest)
library(curl)


#'GPM_IMERG data downloader_download IMERG_F daily data ------------------------------------------------------------------
#' 
#'
#' Function for downloading GPM IMERG-F NC4 files.
#'
#' @param destination a character string with the path where the downloaded file is saved.
#' @param start_year numeric. Start year should be between 2000-2020.
#' @param end_year numeric. End year should be between 2000-2020, and should be greater or equal to start year.
#' @note user must \href{"https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+For+an+EarthData+Login+Profile"}{Create an Earthdata account} and \href{https://disc.gsfc.nasa.gov/earthdata-login}{Link GES DISC}

download_imerg <- function(destination, start_year, end_year){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 2000:2020)) | (!any(end_year == 2000:2020)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 2000-2020, and end_year should be greater or equal to start_year")
  }
  username <- getPass("Enter the username: ") %>% URLencode(reserved = TRUE)
  password <- getPass("Enter the password: ") %>% URLencode(reserved = TRUE)
  for (year in start_year:end_year){
    if (year == 2000){
      start_month <- 6
    } else {
      start_month <- 1
    }
    for (month in start_month:12){
      base_url <- paste0("https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDF.06/", year, "/", str_pad(month, 2, pad = "0"), "/")
      page <- read_html(base_url)
      raw_list <- page # takes the page above for which we've read the html
      links <- html_nodes(raw_list, "a") %>% # find all links in the page
        html_attr("href") %>% # get the url for these links
        str_subset("\\.nc4$")
      #username <- getPass("Enter the username: ") %>% URLencode(reserved = TRUE)
      #password <- getPass("Enter the password: ") %>% URLencode(reserved = TRUE)
      file_url_base <- paste0("https://", username, ":", password, "@", "gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDF.06/", year, "/", str_pad(month, 2, pad = "0"), "/")
      
      url_download <- str_c(file_url_base, links)
      for (i in seq_along(url_download)) {
        fname <- str_c(destination, links[i])
        if (!file.exists(fname)) {
          download.file(url_download[i], fname, mode = "wb")
          #Sys.sleep(1)
        }
      }
    }
  }
}



# download IMERG half_hourly products in HDF5 -----------------------------

download_imerghh <- function(destination, start_year, end_year){
  if (!is.character(destination)) stop ("destination should be a character string.")
  if (!(is.numeric(start_year) & is.numeric(end_year))) stop ("start_year and end_year should be numeric.")
  if ((!any(start_year == 2000:2020)) | (!any(end_year == 2000:2020)) | !(end_year >= start_year)){
    stop("Error: start_year and end_year should be between 2000-2020, and end_year should be greater or equal to start_year")
  }
  username <- getPass("Enter the username: ") %>% URLencode(reserved = TRUE)
  password <- getPass("Enter the password: ") %>% URLencode(reserved = TRUE)
  for (year in start_year:end_year){
    if (year == 2000){
      start_day <- 153
      end_day <- 366
    } else if (year == 2004|2008){
      start_day <- 1
      end_day <- 366 
    } else { 
      start_day <- 1
      end_day <- 365
    }
    for (day in start_day:end_day){
      base_url <- paste0("https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGHH.06/", year, "/", str_pad(day, 3, pad = "0"), "/")
      page <- read_html(base_url)
      raw_list <- page
      links <- html_nodes(raw_list, "a") %>% 
        html_attr("href") %>% 
        str_subset("\\.HDF5$")
      #username <- getPass("Enter the username: ") %>% URLencode(reserved = TRUE)
      #password <- getPass("Enter the password: ") %>% URLencode(reserved = TRUE)
      file_url_base <- paste0("https://", username, ":", password, "@", "gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGHH.06/", year, "/", str_pad(day, 3, pad = "0"), "/")
      
      url_download <- str_c(file_url_base, links)
      for (i in seq_along(url_download)) {
        fname <- str_c(destination, links[i])
        if (!file.exists(fname)) {
          download.file(url_download[i], fname, mode = "wb")
          #Sys.sleep(1)
        }
      }
    }
  }
}


# Test the function -------------------------------------------------------

#destin <- "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_IMERG/data/raw/"

destin <- "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_IMERG/data/raw/trial"
#download_imerg(destin, 2011, 2012)

destin2 <- "D:/IMERG/final/"

download_imerg(destin2, 2020, 2020)
download_imerghh(destin2, 2000, 2020)

