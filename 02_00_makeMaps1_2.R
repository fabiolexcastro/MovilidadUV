
# -------------------------------------------------------------------------
# Proyecto de Movilidad  --------------------------------------------------
# -------------------------------------------------------------------------

# Cargar librerias --------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, qdap, hablar, Hmisc)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Cargar datos ------------------------------------------------------------
pth <- '../data/tbl/movilidad_accesible/wide/0731_total_wide.xlsx'
tbl <- read_excel(pth) %>% 
  dplyr::select(id_encuesta, a_5, d_1)
shp <- st_read('../data/shp/own/movilidad/Shape 7-31/7-31_Origen.shp')
dte <- 0731
prd <- shapefile('../data/shp/mio/01_PARADAS_190722.shp')
stt <- shapefile('../data/shp/mio/estaciones.shp')
com <- shapefile('../data/shp/base/comunas.shp')
com <- spTransform(com, CRSobj = crs(shp))
com <- st_as_sf(com) %>% 
  dplyr::select(NOMBRE) %>% 
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
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('01_02_count_category_estaciones_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMapCount()


