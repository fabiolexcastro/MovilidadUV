

# Cargar librerias --------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Cargar datos ------------------------------------------------------------
tbl <- read_excel('../data/tbl/movilidad_accesible/wide/0826_total_wide.xlsx')
ori <- shapefile('../data/shp/own/movilidad/geocoding/oneByone/8-26_Origen.shp')
dst <- shapefile('../data/shp/own/movilidad/geocoding/oneByone/8-26_Destino.shp')
adm <- shapefile('../data/shp/base/bcs_barrios_geo.shp')
com <- shapefile('../data/shp/base/bcs_comunas_geo.shp')
lfm.ori <- st_read('../data/shp/own/movilidad/lfm/03_origen_viajes.shp')
lfm.dst <- st_read('../data/shp/own/movilidad/lfm/04_destino_viajes.shp')

# Interseccion entre el shape de destino y el shape de barrios a nivel general
dst <- raster::intersect(dst, adm)
dst <- st_as_sf(dst)
dst <- dst %>% mutate(BARRIO = iconv(BARRIO, from = 'UTF-8', to = 'latin1'))

# Interseccion entre el shape de origen y el shape de barrios a nivel general
ori <- raster::intersect(ori, adm)
ori <- st_as_sf(ori)
ori <- ori %>% mutate(BARRIO = iconv(BARRIO, from = 'UTF-8', to = 'latin1'))

# Conocer la comuna con mayor destino, la comuna que mas atrae poblacion en condicion de discapacidad
smm_dst <- dst %>% 
  as_tibble() %>% 
  dplyr::select(COMUNA, BARRIO) %>% 
  group_by(COMUNA) %>% 
  dplyr::summarise(CONTEO = n()) %>% 
  ungroup() %>% 
  arrange(desc(CONTEO))

ids <- dst %>% 
  as_tibble %>% 
  filter(COMUNA == 19) %>% 
  pull(ID_ENCUEST) %>% 
  as.numeric()

# Filtrar los ids de la comuna 19 en la tabla grande para conocer el origen
ori <- ori %>% 
  filter(ID_ENCUEST %in% ids) %>% 
  dplyr::select(ID_ENCUEST, COMUNA, BARRIO)

smm_ori <- ori %>% 
  as_tibble %>% 
  group_by(COMUNA) %>% 
  dplyr::summarise(CONTEO = n()) %>% 
  ungroup() %>% 
  arrange(desc(CONTEO))

######
lfm.ori <- lfm.ori %>% 
  dplyr::select(ID_COMUNA, TOT_VIAJES) %>% 
  as.data.frame() %>% 
  group_by(ID_COMUNA) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() %>% 
  setNames(c('COMUNA', 'CONTEO'))
lfm.dst <- lfm.dst %>% 
  dplyr::select(ID_COMUNA, TOTAL_VIAJ) %>% 
  as.data.frame() %>% 
  group_by(ID_COMUNA) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() %>% 
  setNames(c('COMUNA', 'CONTEO'))

smm_ori <- rbind(lfm.ori, smm_ori)
smm_dst <- rbind(lfm.dst, smm_dst) %>% 
  group_by(COMUNA) %>% 
  dplyr::summarise(count = sum(CONTEO)) %>% 
  ungroup()

# Union de la tabla resumen con el shapefile de comunas
com <- st_as_sf(com)
com_dst <- inner_join(x = com, y = smm_dst, by = c('COMUNA' = 'COMUNA'))
com_dst <- as(com_dst, 'Spatial')
com_ori <- inner_join(x = com, y = smm_ori, by = c('COMUNA' = 'COMUNA'))
com_ori <- as(com_ori, 'Spatial')

# Mapa de generacion de las comuna con mas atraccion (Comuna 19)
dir.create('../data/shp/own/movilidad/analysis')
writeOGR(obj = com_dst, dsn = '../data/shp/own/analysis', layer = 'comunas_destino', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(obj = com_ori, dsn = '../data/shp/own/analysis', layer = 'comunas_origen', driver = 'ESRI Shapefile', overwrite_layer = TRUE)


