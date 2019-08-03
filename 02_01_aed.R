
# -------------------------------------------------------------------------
# Proyecto de Movilidad  --------------------------------------------------
# -------------------------------------------------------------------------

# Cargar libreiras --------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl, qdap, hablar)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Cargar datos ------------------------------------------------------------
tbl <- read_excel('../data/tbl/movilidad_accesible/wide/0731_total_wide.xlsx')
shp <- st_read('../data/shp/base/bcs_barrios_geo.shp') %>% 
  mutate(BARRIO = as.character(BARRIO))
dte <- gsub('-', '_', Sys.Date())
dst <- st_read('../data/shp/own/movilidad/Shape 7-31/7-31_Destino.shp')

# Labels ------------------------------------------------------------------
trn <- data.frame(label = c('camin_rod', 'vpart', 'mpart', 'bici',
                            'taxi', 'mototaxi', 'bicitaxi', 'jeep', 'vinform',
                            'buspubl', 'tinter', 'vescol',
                            'buspriv', 'MIOp', 'MIOart', 'MIOali', 'MIOc', 'Amb'),
                  transp = c('Caminando o rodando', 'Vehículo particular', 'Moto particular', 'Bicicleta',
                             'Taxi', 'Moto taxi', 'Bici taxi', 'Jeep o Guala', 'Vehículo informal',
                             'Bus o buseta pública', 'Transporte intermunicipal', 'Vehículo escolar',
                             'Bus privado de empresa', 'MIO padrón/pretroncal', 'MIO articulado/troncal',
                             'MIO alimentador', 'Mio cable', 'Ambulancia'))
trn <- mutate(trn, transp = as.character(transp),
                   label = as.character(label))

# Mapa 3. Origen total de los viajes de las PcD ---------------------------
makeMap_03 <- function(dfm){
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5) %>% 
    group_by(a_5) %>% 
    summarize(count = n()) %>%  
    ungroup()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'a_5'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('03_origenPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_03(dfm = tbl)

# Mapa 4. Destino total de los viajes de las PcD --------------------------
makeMap_04 <- function(sft){
  # sft <- dst
  int <- raster::intersect(as(sft, 'Spatial'), as(shp, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(BARRIO) %>% 
    as.data.frame() %>% 
    group_by(BARRIO) %>% 
    summarize(count = n()) %>% 
    ungroup()
  rsl <- inner_join(shp, int, by = c('BARRIO' = 'BARRIO'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('04_destinoPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_04(sft = dst)

# Mapa 5. Origen de viajes por modos de transporte de las PcD -------------
makeMap_05 <- function(dfm){
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(a_5, e_7_1:e_7_10) %>% 
    gather(nameCol, transporte, -a_5) %>% 
    drop_na() %>% 
    group_by(a_5, transporte) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    inner_join(., trn, by = c('transporte' = 'transp')) %>% 
    dplyr::select(a_5, label, count) %>% 
    spread(label, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'a_5'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('05_origenMTPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_05(dfm = tbl)

# Mapa 6. Destino de viajes por modos de transporte de las PcD ------------
makeMap_06 <- function(sft, dfm){
  # sft <- dst
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, e_14_1:e_14_10)
  int <- raster::intersect(as(sft, 'Spatial'), as(shp, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    dplyr::select(-ID_ENCUEST) %>% 
    gather(nameCol, transporte, -BARRIO, -COMUNA) %>% 
    drop_na() %>% 
    group_by(BARRIO, transporte) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    inner_join(., trn, by = c('transporte' = 'transp')) %>% 
    dplyr::select(BARRIO, label, count) %>% 
    spread(label, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'BARRIO'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('06_destinoMTPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_06(sft = dst, dfm = tbl)

# Mapa 7. Barreras ida moto y bicicleta -----------------------------------
makeMap_07 <- function(dfm){
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_19:f_33)
  lbl <- data.frame(nameCol = paste0('f_', c(19:24, 26:33)),
                    barrera = c('m_abordar', 'm_ubicar', 'm_manejar', 'm_retr', 'm_leer', 'm_pr_pav', 'm_bajar',
                                'b_abordar', 'b_ubicar', 'b_leer', 'b_movili', 'b_pr_pav', 'b_dcdrrecor', 'b_bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio) %>% 
    drop_na() %>%
    filter(tipo %in% c('Alto', 'Muy Alto')) %>% 
    group_by(barrio, dificultad) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count)
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('07_barrera_ida_bicimoto_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_07(dfm = tbl)









