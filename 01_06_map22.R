
###########################################################################
# Mapa de rangos de dad de las personas con discapacidad
###########################################################################

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, Hmisc)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tbl <- read_excel('../tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx')
brr <- st_read('../data/shp/base/bcs_barrios.shp') %>% 
  mutate(BARRIO = as.character(BARRIO))

# Cleaning dataframe ------------------------------------------------------
tbl <- tbl %>% 
  dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia, edad_dis)
rcl <- pull(tbl, edad_dis) %>% 
  cut2(., seq(9, 98, 10)) 
rng <- data.frame(ranges = as.character(unique(rcl)), category = c(60,20,40,50,10,30,90,70,80)) %>% 
  arrange(category)
tbl <- tbl %>% 
  mutate(ranges = rcl) %>% 
  inner_join(., rng, by = c('ranges' = 'ranges'))
smm <- tbl %>% 
  group_by(barrio_residencia, category) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(category = paste0('c_', category)) %>% 
  spread(category, count)

# Inner join with the shapefile -------------------------------------------
fnl <- inner_join(brr, smm, by = c('BARRIO' = 'barrio_residencia'))
fnl <- as(fnl, 'Spatial')
writeOGR(obj = fnl, dsn = '../data/gdb/order/match/ok', layer = 'mapa22_edad_pcd', driver = 'ESRI Shapefile')






