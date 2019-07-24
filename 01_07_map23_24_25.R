
##################################################################################
# Mapa de tiempo promedio de espera de las personas con discapacidad
# Mapa de tiempo promedio de trayecto de las personas con discapacidad
# Mapa tiempo promedio total de desplazamiento de las personas con discapacidad
##################################################################################

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
vjs <- read_csv('../tbl/join/base_viajes_CIDSE_LFM.csv')

# Label
lbl <- data.frame(values = as.character(c(1:9 , 11:19)),
                  category = c('camin', 'vpartic', 'moto', 'bici', 'taxi', 'mototaxi', 'bicitaxi', 'jeep', 'vinform', 'bus', 'tr_intm', 'vescolar', 'bprivado', 'padron', 'artic', 'alim', 'cable', 'otro')) %>% 
  mutate(category = as.character(category),
         values = as.character(values))

# Cleaning dataframe ------------------------------------------------------
tbl <- tbl %>% 
  dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia)

v_cd <- vjs %>% 
  filter(fuente == 'CIDSE') %>% 
  dplyr::select(-X1) %>% 
  inner_join(., lbl, by = c('transporte_viaje' = 'values')) %>% 
  mutate(transporte_viaje = category) %>% 
  dplyr::select(-category)
  
v_lf <- vjs %>% 
  filter(fuente == 'LFM') %>% 
  dplyr::select(-X1) 
  
vjs <- rbind(v_cd, v_lf)

###########################################################################
# Tiempo promedio de espera de las PcD ------------------------------------
###########################################################################
v_espera <- vjs %>% 
  dplyr::select(num_encuesta, etapa_viaje, transporte_viaje, tiempoespera_viaje) %>% 
  inner_join(., tbl, by = c('num_encuesta' = 'num_encuesta')) %>% 
  group_by(num_encuesta, barrio_residencia, transporte_viaje) %>% 
  summarise(tespera = mean(tiempoespera_viaje, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(barrio_residencia, transporte_viaje) %>% 
  summarise(tespera = mean(tespera)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  spread(transporte_viaje, tespera)

v_espera_shp <- inner_join(brr, v_espera, by = c('BARRIO' = 'barrio_residencia'))
v_espera_shp <- as(v_espera_shp, 'Spatial')  
writeOGR(obj = v_espera_shp, dsn = '../data/gdb/order/match/ok', layer = 'mapa23_tiempoespera', driver = 'ESRI Shapefile')

###########################################################################
# Tiempo promedio de trayecto de las PcD ----------------------------------
###########################################################################
v_viaje <- vjs %>% 
  dplyr::select(num_encuesta, etapa_viaje, transporte_viaje, tiempotrayecto_viaje) %>% 
  inner_join(., tbl, by = c('num_encuesta' = 'num_encuesta')) %>% 
  group_by(num_encuesta, barrio_residencia, transporte_viaje) %>% 
  summarise(tviaje = mean(tiempotrayecto_viaje, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(barrio_residencia, transporte_viaje) %>% 
  summarise(tviaje = mean(tviaje)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  spread(transporte_viaje, tviaje)

v_viaje_shp <- inner_join(brr, v_viaje, by = c('BARRIO' = 'barrio_residencia'))
v_viaje_shp <- as(v_viaje_shp, 'Spatial')  
writeOGR(obj = v_viaje_shp, dsn = '../data/gdb/order/match/ok', layer = 'mapa24_tiempoviaje', driver = 'ESRI Shapefile')

###########################################################################
# Tiempo promedio total de desplazamiento de las PcD ----------------------
###########################################################################
v_total <- vjs %>% 
  dplyr::select(num_encuesta, etapa_viaje, transporte_viaje, tiempoespera_viaje, tiempotrayecto_viaje) %>% 
  inner_join(., tbl, by = c('num_encuesta' = 'num_encuesta')) %>% 
  mutate(tiempototal = tiempoespera_viaje + tiempotrayecto_viaje) %>% 
  dplyr::select(-tiempoespera_viaje, -tiempotrayecto_viaje) %>% 
  group_by(num_encuesta, barrio_residencia, transporte_viaje) %>% 
  summarise(ttotal = mean(tiempototal, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(barrio_residencia, transporte_viaje) %>% 
  summarise(ttotal = mean(ttotal)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  spread(transporte_viaje, ttotal)

v_total_shp <- inner_join(brr, v_total, by = c('BARRIO' = 'barrio_residencia'))
v_total_shp <- as(v_total_shp, 'Spatial')  
writeOGR(obj = v_total_shp, dsn = '../data/gdb/order/match/ok', layer = 'mapa25_tiempototal', driver = 'ESRI Shapefile')



















