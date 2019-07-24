
###########################################################################
# Mapa del genero de las personas con discapacidad
###########################################################################

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tbl <- read_excel('../tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx')
brr <- st_read('../data/shp/base/bcs_barrios.shp') %>% 
  mutate(BARRIO = as.character(BARRIO))

lbl <- data.frame(values = c(1, 2, 'HOMBRE', 'MUJER'),
                  category = c('Hombre', 'Mujer', 'Hombre', 'Mujer')) %>% 
  mutate(values = as.character(values))

# Cleaning dataframe ------------------------------------------------------
tbl <- tbl %>% 
  dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia, sexo_dis) %>% 
  inner_join(., lbl, by = c('sexo_dis' = 'values'))

smm <- tbl %>% 
  group_by(barrio_residencia, category) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  spread(category, count)
  
fnl <- inner_join(brr, smm, by = c('BARRIO' = 'barrio_residencia'))
fnl <- as(fnl, 'Spatial')
writeOGR(obj = fnl, dsn = '../data/gdb/order/match/ok', layer = 'mapa21_genero_pcd', driver = 'ESRI Shapefile')


