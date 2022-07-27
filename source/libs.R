#Libraries commonly used in code
if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'DescTools',
               'ggplot2', 'ggalt', 'ggExtra',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales', 
               'ncdf4', 'raster', 'sf', 
               'sp', 'lubridate', 'parallel', 'tidyverse')



library(data.table)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(parallel)
library(doParallel)
library(terra)
library(lubridate)
library(gsubfn)
