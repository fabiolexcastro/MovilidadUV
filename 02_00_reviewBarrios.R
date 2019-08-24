

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, qdap, hablar, Hmisc)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
shp2tbl <- function(tbl){
  tbl <- tbl %>% 
    as.data.frame %>% 
    as_tibble %>% 
    arrange(ID_ENCUEST) %>% 
    dplyr::select(ID_ENCUEST, BARRIO)
  print('Done!')
  return(tbl)
}

# Load data ---------------------------------------------------------------
fls <- list.files('../data/shp/own/movilidad/geocoding/oneByone', full.names = TRUE, pattern = '.shp$')
tb1 <- read_excel('../data/tbl/movilidad_accesible/wide/0814_total_wide.xlsx')

# Cleaning the table
tb1 <- tb1 %>% 
  dplyr::select(id_encuesta, a_5, e_11) %>% 
  setNames(c('id_encuesta', 'origen_tbl', 'destino_tbl'))

# Origen y destino (shapefiles)
ori <- fls %>% 
  grep('Origen', ., value = TRUE) %>% 
  map(.x = ., .f = st_read)
dst <- fls %>% 
  grep('Destino', ., value = TRUE) %>% 
  map(.x = ., .f = st_read)

ori <- shp2tbl(ori[[3]]) %>% 
  rename(origen_shp = BARRIO)
dst <- shp2tbl(dst[[3]]) %>% 
  rename(destino_shp = BARRIO)

# Join the two tables into only one
tb2 <- inner_join(ori, dst, by = 'ID_ENCUEST') %>% 
  mutate(origen_shp = iconv(origen_shp, to = 'latin1'),
         destino_shp = iconv(destino_shp, to = 'latin1'))

fnl <- inner_join(tb1, tb2, by = c('id_encuesta' = 'ID_ENCUEST')) %>% 
  mutate(comparison = origen_tbl == origen_shp)

trb <- fnl %>% 
  filter(comparison == FALSE)

if(nrow(trb) == 0){
  print('Todo bien')
} else {
  ('Hay inconsistencias, por favor, revisar')
}






