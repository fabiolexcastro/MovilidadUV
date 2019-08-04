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
pth <- '../data/tbl/movilidad_accesible/wide/0731_total_wide.xlsx'
tbl <- read_excel(pth)
shp <- st_read('../data/shp/base/bcs_barrios_geo.shp') %>% 
  mutate(BARRIO = as.character(BARRIO))
dte <- basename(pth) %>% str_sub(., start = 1, end = 4)
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

# Mapa 8. Barreras regreso moto y bicicleta -------------------------------
makeMap_08 <- function(sft, dfm){
  # sft <- dst
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_19:f_33)
  int <- raster::intersect(as(sft, 'Spatial'), as(shp, 'Spatial')) 
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  # Labels 
  lbl <- data.frame(nameCol = paste0('f_', c(19:24, 26:33)),
                    barrera = c('m_abordar', 'm_ubicar', 'm_manejar', 'm_retr', 'm_leer', 'm_pr_pav', 'm_bajar',
                                'b_abordar', 'b_ubicar', 'b_leer', 'b_movili', 'b_pr_pav', 'b_dcdrrecor', 'b_bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'comuna', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio, -comuna) %>% 
    drop_na() %>% 
    dplyr::select(-comuna) %>% 
    group_by(barrio, dificultad) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  
  if(ncol(dfm) == 15){
    dfm <- dfm %>% 
      transmute(barrio,
                abordar = b_abordar + m_abordar,
                ubicar = b_ubicar + m_ubicar,
                bajar = b_bajar + m_bajar,
                dcdrrecor = b_dcdrrecor,
                leer = m_leer + b_leer,
                retr = m_retr,
                pr_pav = m_pr_pav + m_pr_pav,
                movili = b_movili,
                manejar = m_manejar)
  } else {
    dfm <- dfm
  }
  
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio'))  
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('08_barrera_regreso_bicimoto_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_08(sft = dst, dfm = tbl)

# Mapa 9. Barreras ida taxi -----------------------------------------------
makeMap_09 <- function(dfm){
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_34:f_41)
  lbl <- data.frame(nameCol = paste0('f_', c(34:36, 38:41)),
                    barrera = c('abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar'))
  
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio) %>% 
    drop_na() %>% 
    filter(tipo %in%  c('Alto', 'Muy Alto')) %>% 
    group_by(barrio, dificultad) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('09_barreras_ida_taxi_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_09(dfm = tbl)

# Mapa 10. Barreras regreso taxi ------------------------------------------
makeMap_10 <- function(sft, dfm){
  # sft <- dst
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_34:f_41)
  int <- raster::intersect(as(sft, 'Spatial'), as(shp, 'Spatial'))
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Labels
  lbl <- data.frame(nameCol = paste0('f_', c(34:36, 38:41)),
                    barrera = c('abordar', 'a_pri', 'ub_asi', 'comu_dst', 'pagar', 'leer', 'bajar'))
  
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'comuna', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio, -comuna) %>% 
    drop_na() %>% 
    group_by(barrio, dificultad) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('10_barrera_regreso_taxi_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_10(sft = dst, dfm = tbl)

# Mapa 11. Barreras ida Jeep, Guala o Vehiculo informal -------------------
makeMap_11 <- function(dfm){
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_78:f_87)
  lbl <- data.frame(nameCol = paste0('f_', c(78:82, 84:87)),
                    barrera = c('ubic', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'com_dst', 'pgr_trn', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio) %>% 
    drop_na() %>% 
    filter(tipo %in% c('Alto', 'Muy alto')) %>% 
    group_by(barrio, dificultad) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('11_barreras_ida_guala_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_11(dfm = tbl)
    
# Mapa 12. Barreras retorno Jeep, Guala o Vehiculo informal --------------
makeMap_12 <- function(sft, dfm){
  # sft <- dst
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_78:f_87)
  int <- raster::intersect(as(sft, 'Spatial'), as(shp, 'Spatial'))
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Labels
  lbl <- data.frame(nameCol = paste0('f_', c(78:82, 84:87)),
                    barrera = c('ubic', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'com_dst', 'pgr_trn', 'leer', 'bajar'))
  
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'comuna', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio, -comuna) %>% 
    drop_na() %>% 
    group_by(barrio, dificultad) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('12_barrera_regreso_guala_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_12(sft = dst, dfm = tbl)

# Mapa 13. Barreras ida MIO articulado ------------------------------------
makeMap_13 <- function(dfm){
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_98:f_108)
  lbl <- data.frame(nameCol = paste0('f_', 98:108),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio) %>% 
    drop_na() %>% 
    filter(tipo %in% c('Alto', 'Muy alto')) %>% 
    group_by(barrio, dificultad) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('13_barreras_ida_articulado_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_13(dfm = tbl)

# Mapa 14. Barreras retorno MIO articulado --------------------------------
makeMap_14 <- function(sft, dfm){
  # sft <- dst
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_98:f_108)
  int <- raster::intersect(as(sft, 'Spatial'), as(shp, 'Spatial'))
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Labels
  lbl <- data.frame(nameCol = paste0('f_', 98:108),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'comuna', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio, -comuna) %>% 
    drop_na() %>% 
    group_by(barrio, dificultad) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('14_barreras_regreso_articulado_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_14(sft = dst, dfm = tbl)

# Mapa 15. Barreras ida MIO complementario --------------------------------
makeMap_15 <- function(dfm){
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_120:f_130)
  lbl <- data.frame(nameCol = paste0('f_', 120:130),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio) %>% 
    drop_na() %>% 
    filter(tipo %in% c('Alto', 'Muy alto')) %>% 
    group_by(barrio, dificultad) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('15_barreras_ida_MIOcomplementario_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_15(dfm = tbl)

# Mapa 16. Barreras regreso MIO complementario ----------------------------
makeMap_16 <- function(sft, dfm){
  # sft <- dst
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_120:f_130)
  int <- raster::intersect(as(sft, 'Spatial'), as(shp, 'Spatial'))
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Labels
  lbl <- data.frame(nameCol = paste0('f_', 120:130),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'comuna', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio, -comuna) %>% 
    drop_na() %>% 
    group_by(barrio, dificultad) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('16_barreras_regreso_articulado_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_16(sft = dst, dfm = tbl)

# Mapa 17. Barreras ida peaton -------------------------------------------
makeMap_17 <- function(dfm){
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, a_5, f_1:f_10)
  lbl <- data.frame(nameCol = paste0('f_', c(1:7, 9:10)),
                    barrera = c('idnt_clles', 'sbr_bjr', 'dspl_and', 'dspl_obst', 'prb_pav', 'acera_rmps', 'crzr_clls', 'puentes', 'leer'))
  nms <- as.character(lbl$barrera)
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio) %>% 
    drop_na() %>% 
    filter(tipo %in% c('Alto', 'Muy alto')) %>% 
    group_by(barrio, dificultad) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('17_barreras_ida_peaton_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_17(dfm = tbl)

# Mapa 18. Barreras retorno peaton  ---------------------------------------
makeMap_18 <- function(sft, dfm){
  # sft <- dst
  # dfm <- tbl
  dfm <- dfm %>% 
    dplyr::select(id_encuesta, f_1:f_10)
  int <- raster::intersect(as(sft, 'Spatial'), as(shp, 'Spatial'))
  int <- st_as_sf(int) %>% 
    dplyr::select(ID_ENCUEST, COMUNA, BARRIO) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Labels
  lbl <- data.frame(nameCol = paste0('f_', 120:130),
                    barrera = c('ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar'))
  nms <- as.character(lbl$barrera)
  dfm <- inner_join(int, dfm, by = c('ID_ENCUEST' = 'id_encuesta'))
  dfm <- dfm %>% 
    setNames(c('id_encuesta', 'comuna', 'barrio', nms)) %>% 
    dplyr::select(-id_encuesta) %>% 
    gather(dificultad, tipo, -barrio, -comuna) %>% 
    drop_na() %>% 
    group_by(barrio, dificultad) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(dificultad, count) %>% 
    NAer() %>% 
    as_tibble() %>% 
    retype()
  rsl <- inner_join(shp, dfm, by = c('BARRIO' = 'barrio')) 
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/barrios', layer = paste0('16_barreras_regreso_articulado_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_16(sft = dst, dfm = tbl)
