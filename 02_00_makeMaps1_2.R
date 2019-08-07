
# -------------------------------------------------------------------------
# Proyecto de Movilidad  --------------------------------------------------
# -------------------------------------------------------------------------

# Cargar librerias --------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, qdap, hablar, Hmisc)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# -------------------------------------------------------------------------
# Cargar datos ------------------------------------------------------------
# -------------------------------------------------------------------------
pth <- '../data/tbl/movilidad_accesible/wide/0731_total_wide.xlsx'
tbl <- read_excel(pth) %>% 
  dplyr::select(id_encuesta, a_5, d_1)
shp <- st_read('../data/shp/own/movilidad/Shape 7-31/7-31_Origen.shp')
dte <- 0731
prd <- shapefile('../data/shp/mio/01_PARADAS_190722.shp')
stt <- shapefile('../data/shp/mio/estaciones.shp')
com <- shapefile('../data/shp/base/bcs_comunas_geo.shp')
com <- st_as_sf(com) %>% 
  dplyr::select(COMUNA) %>% 
  setNames(c('COMUNA', 'geometry')) %>% 
  as(., 'Spatial')

# Categorizando las discapacidades
dcp <- tbl %>% 
  distinct(d_1) %>% 
  pull()
dcp <- data.frame(discapacidad = dcp,
                  categoria = c('Sistemica', 'Sistemica', 'Movilidad', 'Visual', 'Sistemica', 'Sistemica', 'Sistemica', 'Sistemica'))
dcp <- dcp %>% 
  mutate(discapacidad = as.character(discapacidad),
         categoria = as.character(categoria)) 
tbl <- inner_join(tbl, dcp, by = c('d_1' = 'discapacidad'))
shp <- inner_join(shp, tbl, by = c('ID_ENCUEST' = 'id_encuesta'))

makeMapCount <- function(){
  # Hacer conteo
  int <- raster::intersect(as(shp, 'Spatial'), com) %>% 
    st_as_sf() %>% 
    rename(comuna = d) %>% 
    as(., 'Spatial')
  count <- int %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    group_by(comuna, categoria) %>% 
    dplyr::summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(name_com = paste0('Comuna ', comuna)) %>% 
    spread(categoria, count) %>% 
    NAer() %>% 
    retype() %>% 
    mutate(comuna = as.character(comuna))
  com <- st_as_sf(com)
  rsl <- inner_join(com, count, by = c('COMUNA' = 'comuna'))
  rsl <- as(rsl, 'Spatial')
  rsl <- as.data.frame(rsl)
  # writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('01_02_count_category_estaciones_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
  
  lfm <- st_read('../data/shp/own/movilidad/lfm/01_tiempo_estaciones_geo.shp') %>% 
    dplyr::select(ID_COMUNA, PORCEN_MOV, MOV_SISTE, PORCET_VIS) %>% 
    setNames(c('COMUNA', 'Movilidad', 'Sistemica', 'Visual', 'geometry')) %>% 
    as(., 'Spatial') %>% 
    as.data.frame()
  
  rsl <- rsl %>% 
    dplyr::select(COMUNA, Movilidad, Sistemica, Visual) %>% 
    rbind(., lfm)
  rsl <- inner_join(com, rsl, by = c('COMUNA' = 'COMUNA'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('01_02_count_category_estaciones_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
}
makeMapCount()

# Going to ArcGIS and apply the tool (Model Builder)

###########################################################################
# Mapa 1. Accesiblidad a estaciones ---------------------------------------
###########################################################################

# Reading the output from ArcGIS
shp <- shapefile('../data/shp/own/movilidad/Shape 7-31/7-31_Origen_dist_stt.shp')
shp <- raster::intersect(shp, com) %>% 
  st_as_sf() %>% 
  rename(comuna = d) %>% 
  as(., 'Spatial')
smm <- shp %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  group_by(comuna) %>% 
  dplyr::summarise(time = mean(time, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(comuna = as.character(comuna))
rsl <- inner_join(st_as_sf(com), smm, by = c('COMUNA' = 'comuna'))
rsl <- as.data.frame(rsl) %>% dplyr::select(-geometry)
lfm <- st_read('../data/shp/own/movilidad/lfm/01_tiempo_estaciones_geo.shp') %>% 
  dplyr::select(ID_COMUNA, INDICADOR) %>% 
  setNames(c('COMUNA', 'time', 'geometry')) %>% 
  as(., 'Spatial') %>% 
  as.data.frame()
rsl <- rbind(rsl, lfm)
rsl <- inner_join(st_as_sf(com), rsl, by = c('COMUNA' = 'COMUNA'))
rsl <- as(rsl, 'Spatial')
writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('01_tiempo_estaciones_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)

###########################################################################
# Mapa 2. Accesiblidad a paradas ------------------------------------------
###########################################################################
shp <- shapefile('../data/shp/own/movilidad/Shape 7-31/7-31_Origen_dist_paradas.shp')
shp <- raster::intersect(shp, as(com, 'Spatial')) %>% 
  st_as_sf() %>% 
  rename(comuna = d) %>% 
  as(., 'Spatial')
smm <- shp %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  group_by(comuna) %>% 
  dplyr::summarise(time = mean(time, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(comuna = as.character(comuna))
rsl <- inner_join(st_as_sf(com), smm, by = c('COMUNA' = 'comuna'))
rsl <- as.data.frame(rsl) %>% dplyr::select(-geometry)
lfm <- st_read('../data/shp/own/movilidad/lfm/02_tiempo_paradas_geo.shp') %>% 
  dplyr::select(ID_COMUNA, INDICADOR) %>% 
  setNames(c('COMUNA', 'time', 'geometry')) %>% 
  as(., 'Spatial') %>% 
  as.data.frame()
rsl <- rbind(rsl, lfm)
rsl <- inner_join(st_as_sf(com), rsl, by = c('COMUNA' = 'COMUNA'))
rsl <- as(rsl, 'Spatial')
writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('02_tiempo_paradas_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
