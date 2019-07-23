
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fls <- list.files('../data/gdb/order/match/maps', full.names = TRUE, pattern = '.shp$') %>% 
  grep('JOIN', ., value = TRUE)

shp <- lapply(fls, st_read)

# Okey

