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
shp <- st_read('../data/shp/base/bcs_barrios_geo.shp') %>% 
  mutate(BARRIO = as.character(BARRIO))
dte <- 0731

# Mapa 3. Origen total de los viajes --------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/03_origenPcD_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/03_origen_viajes.shp')

makeMap_03 <- function(){
  own <- own %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, count)
  lfm <- lfm %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, TOT_VIAJES) %>% 
    rename(count = TOT_VIAJES)
  print(unique(colnames(own) == colnames(lfm)))
  rsl <- rbind(own, lfm) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    mutate(BARRIO = as.character(BARRIO))
  rsl <- inner_join(shp, rsl, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('03_origenviajes_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_03()

# Mapa 4. Destino total de los viajes -------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/04_destinoPcD_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/04_destino_viajes.shp')
makeMap_04 <- function(){
  own <- own %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, count)
  lfm <- lfm %>% 
    as.data.frame() %>% 
    dplyr::select(BARRIO, TOTAL_VIAJ) %>% 
    as_tibble() %>% 
    rename(count = TOTAL_VIAJ)
  print(unique(colnames(own) == colnames(lfm)))
  rsl <- rbind(own, lfm) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    mutate(BARRIO = as.character(BARRIO))
  rsl <- inner_join(shp, rsl, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('04_destino_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_04()

# Mapa 5. Origen de viajes por modo de transporte -------------------------
own <- st_read('../data/shp/own/movilidad/barrios/05_origenMTPcD_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/mapa5_6_origen_destino.shp')
makeMap_05 <- function(){
  lfm <- lfm %>% 
    dplyr::select(BARRIO, starts_with('ori', ignore.case = TRUE)) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    setNames(c('BARRIO', 'MIOp', 'buspubl', 'jeep', 'taxi', 'vpart', 'mpart', 'bici', 'camin_rod', 'otro'))
  own <- own %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, bici:vpart)
  rsl <- bind_rows(own, lfm) %>% 
    NAer() %>% 
    retype() %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup()
  rsl <- inner_join(shp, rsl, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('05_origenMDT_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_05()

# Mapa 6. Destino de viajes por modo de transporte ------------------------
own <- st_read('../data/shp/own/movilidad/barrios/06_destinoMTPcD_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/mapa5_6_origen_destino.shp')
makeMap_06 <- function(){
  lfm <- lfm %>% 
    dplyr::select(BARRIO, starts_with('dst', ignore.case = TRUE)) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    setNames(c('BARRIO', 'MIOp', 'buspubl', 'jeep', 'taxi', 'vpart', 'mpart', 'bici', 'camin_rod', 'otro'))
  own <- own %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, bici:vpart)
  rsl <- bind_rows(own, lfm) %>% 
    NAer() %>% 
    retype() %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup()
  rsl <- inner_join(shp, rsl, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('06_destinoMDT_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_06()

# Mapa 7. Barreras ida moto y bicicleta -----------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/07_barrera_ida_bicimoto_0731.shp')
lfm_mto <- st_read('../data/shp/own/movilidad/lfm/07_barreras_ida_moto.shp')
lfm_bcc <- st_read('../data/shp/own/movilidad/lfm/07_barreras_ida_bicicleta.shp')
makeMap_07 <- function(){
  
  own <- own %>% 
    as.data.frame() %>% 
    dplyr::select(BARRIO, b_abrdr:m_ubicr)
  sh1 <- lfm_mto %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B7) %>% 
    setNames(c('BARRIO', 'm_abrdr', 'm_ubicr', 'm_manjr', 'm_retr', 'm_leer', 'm_pr_pv', 'm_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  sh2 <- lfm_bcc %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B6) %>% 
    setNames(c('BARRIO', 'b_abrdr', 'b_ubicr', 'b_leer', 'b_movil', 'b_pr_pv', 'b_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  dfm <- inner_join(sh1, sh2, by = 'BARRIO')
  dfm <- bind_rows(dfm, own)
  dfm <- dfm %>% mutate(BARRIO = as.character(BARRIO))
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('07_idabicimotoPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_07()

# Mapa 8. Barreras regreso moto y bicicleta -------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/08_barrera_regreso_bicimoto_0731.shp')
lfm_mto <- st_read('../data/shp/own/movilidad/lfm/08_barreras_regreso_moto.shp')
lfm_bcc <- st_read('../data/shp/own/movilidad/lfm/08_barreras_regreso_bici.shp')
makeMap_08 <- function(){
  own <- own %>% 
    as.data.frame() %>% 
    dplyr::select(BARRIO, b_abrdr:m_ubicr)
  sh1 <- lfm_mto %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B7) %>% 
    setNames(c('BARRIO', 'm_abrdr', 'm_ubicr', 'm_manjr', 'm_retr', 'm_leer', 'm_pr_pv', 'm_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  sh2 <- lfm_bcc %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B6) %>% 
    setNames(c('BARRIO', 'b_abrdr', 'b_ubicr', 'b_leer', 'b_movil', 'b_pr_pv', 'b_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  dfm <- inner_join(sh1, sh2, by = 'BARRIO')
  dfm <- bind_rows(dfm, own)
  dfm <- dfm %>% mutate(BARRIO = as.character(BARRIO))
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('08_regresobicimotoPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_08()
# Mapa 9. Barreras ida taxi -----------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/09_barreras_ida_taxi_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/09_barreras_ida_taxi.shp')
makeMap_09 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B7) %>% 
    setNames(c('BARRIO', 'abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, abordar, a_priorit, ub_asiento, comu_dest, pagar, leer, bajar, geometry) %>% 
    setNames(c('BARRIO', 'abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('09_idataxiPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_09()

# Mapa 10. Barreras regreso taxi ------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/10_barrera_regreso_taxi_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/10_barreras_regreso_taxi.shp')
makeMap_10 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B7) %>% 
    setNames(c('BARRIO', 'abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, abordar, a_pri, ub_asi, comu_dst, pagar, leer, bajar, geometry) %>% 
    setNames(c('BARRIO', 'abordar', 'a_priorit', 'ub_asiento', 'comu_dest', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('10_regresotaxiPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_10()

# Mapa 11. Barreras ida jeep -----------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/11_barreras_ida_guala_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/11_barreras_ida_jeepi.shp')
makeMap_11 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B9) %>% 
    setNames(c('BARRIO', 'ubic', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'com_dst', 'pgr_trn', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, ubic, a_prio, abrdr_vh, ubicar_asn, com_dst, pgr_trn, leer, bajar, geometry) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('11_idajeepPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_11()

# Mapa 12. Barreras regreso jeep -----------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/12_barrera_regreso_guala_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/12_barreras_destino_jeepi.shp')
makeMap_12 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B9) %>% 
    setNames(c('BARRIO', 'ubic', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'com_dst', 'pgr_trn', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, ubic, llgr_pard, a_prio, abrdr_vh, ubicar_asn, com_dst, pgr_trn, leer, bajar, geometry) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('12_regresojeepPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_12()

# Mapa 13. Barreras ida MIO articulado ------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/13_barreras_ida_articulado_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/13_barreras_ida_MIOarticulado.shp')
makeMap_13 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B11) %>% 
    setNames(c('BARRIO', 'ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, ubicar, llgr_pard, a_prio, abrdr_vh, ubicar_asn, uso_prio, dcd_rcrrd, com_dst, pagar, leer, bajar, geometry) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('13_idaMIOarticuladoPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_13()

# Mapa 14. Barreras regreso MIO articulado ---------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/14_barreras_regreso_articulado_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/14_barreras_regreso_MIOarticulado.shp')
makeMap_14 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B11) %>% 
    setNames(c('BARRIO', 'ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, ubicar, llgr_pard, a_prio, abrdr_vh, ubicar_asn, uso_prio, dcd_rcrrd, com_dst, pagar, leer, bajar, geometry) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('14_regresoMIOarticuladoPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_14()

# Mapa 15. Barreras ida MIO complementario --------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/15_barreras_ida_MIOcomplementario_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/15_barreras_ida_MIOcomplementario.shp')
makeMap_15 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B11) %>% 
    setNames(c('BARRIO', 'ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, ubicar, llgr_pard, a_prio, abrdr_vh, ubicar_asn, uso_prio, dcd_rcrrd, com_dst, pagar, leer, bajar, geometry) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('15_idaMIOcomplementarioPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_15()

# Mapa 16. Barreras regreso complementario --------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/16_barreras_regreso_articulado_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/16_barreras_regreso_MIOcomplementario.shp')
makeMap_16 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B11) %>% 
    setNames(c('BARRIO', 'ubicar', 'llgr_pard', 'a_prio', 'abrdr_vh', 'ubicar_asn', 'uso_prio', 'dcd_rcrrd', 'com_dst', 'pagar', 'leer', 'bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, ubicar, llgr_pard, a_prio, abrdr_vh, ubicar_asn, uso_prio, dcd_rcrrd, com_dst, pagar, leer, bajar, geometry) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('16_regresoMIOcomplementarioPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_16()

# Mapa 17. Barreras ida peaton --------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/17_barreras_ida_peaton_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/17_barreras_ida_peaton.shp')
makeMap_17 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B9) %>% 
    setNames(c('BARRIO', 'idnt_clles', 'sbr_bjr', 'dspl_and', 'dspl_obst', 'prb_pav', 'acera_rmps', 'crzr_clls', 'puentes', 'leer', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, idnt_clles, sbr_bjr, dspl_and, dspl_obst, prb_pav, acera_rmps, crzr_clls, puentes, leer, geometry) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('17_idapeatonPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_17()

# Mapa 18. Barreras regreso peaton --------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/18_barreras_regreso_peaton_0731.shp')
lfm <- st_read('../data/shp/own/movilidad/lfm/18_barreras_regreso_peaton.shp')
makeMap_18 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B9) %>% 
    setNames(c('BARRIO', 'idnt_clles', 'sbr_bjr', 'dspl_and', 'dspl_obst', 'prb_pav', 'acera_rmps', 'crzr_clls', 'puentes', 'leer', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, idnt_clles, sbr_bjr, dspl_and, dspl_obst, prb_pav, acera_rmps, crzr_clls, puentes, leer, geometry) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('18_regresopeatonPcD_', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_18()

# Mapa 19. Lineas de deseo ------------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/19_lineas_de_deseo_0731.shp')
lfm <- read_csv('../tbl/LFM/BASE DE DATOS VIAJES EDIT 2.csv') %>% 
  mutate(BARRIO = iconv(BARRIO, to = 'latin1'))
makeMap_19 <- function(){
  sh1 <- lfm %>% 
    dplyr::select(BARRIO, Tr:O) %>% 
    setNames(c('BARRIO', 'trabajar', 'estudiar', 'deporte', 'medica', 'sociales', 'asunt_per', 'otro')) %>% 
    mutate(BARRIO = as.character(BARRIO))
  sh2 <- own %>% 
    dplyr::select(BARRIO, asunt_per, deporte, estudiar, medica, sociales, trabajar, geometry) %>%  # compras, otro
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    mutate(BARRIO = as.character(BARRIO))
  dfm <- bind_rows(sh1, sh2) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum)
  rsl <- inner_join(shp, dfm, by = 'BARRIO')
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('19_lienasdeseo', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_19()

# Mapa 20. Causas de discapacidad ------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/20_causa_discapacidad_0731.shp')
lfm <- read_excel('../tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx') %>% 
  filter(fuente == 'LFM')
makeMap_20 <- function(){
  lbl <- data.frame(value = c(1:8, 98), category = c('Nacio asi', 'Enfermedad', 'Accidente', 'Hechos violentos', 'Edad avanzada', 'Enfermedad profesional', 'Otra causa', 'No sabe', 'No responde')) %>% 
    mutate(category = as.character(category))
  lfm <- lfm %>% 
    dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia, causadiscapacidad) %>% 
    inner_join(., lbl, by = c('causadiscapacidad' = 'value')) %>% 
    group_by(category, barrio_residencia) %>% 
    dplyr::summarize(count = n()) %>% 
    ungroup()
  lfm <- lfm %>% 
    mutate(category = gsub(' ', '_', category)) %>% 
    spread(category, count) %>% 
    setNames(c('barrio', 'accidnt', 'edad', 'enferm', 'violenc', 'nacim', 'nosabe', 'otra')) %>% 
    NAer() %>% 
    retype()
  own %<>% 
    as.data.frame %>% 
    as_tibble %>% 
    dplyr::select(BARRIO, 8:ncol(.)) %>% 
    dplyr::select(-geometry) %>% 
    rename(barrio = BARRIO)
  own <- bind_rows(own, lfm) %>% 
    group_by(barrio) %>% 
    summarize_all(.funs = sum) %>% 
    mutate(barrio = as.character(barrio))
  rsl <- inner_join(shp, own, by = c('BARRIO' = 'barrio'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('20_causasdiscapacidad', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_20()

# Mapa 21. Sexo de las PcD ----------------------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/21_sexo_0731.shp')
lfm <- read_excel('../tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx') %>% 
  filter(fuente == 'LFM')
makeMap_21 <- function(){
  lbl <- data.frame(values = c(1, 2, 'HOMBRE', 'MUJER'),
               category = c('Hombre', 'Mujer', 'Hombre', 'Mujer')) %>% 
    mutate(values = as.character(values))
  tbl <- lfm %>% 
    dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia, sexo_dis) %>% 
    inner_join(., lbl, by = c('sexo_dis' = 'values'))
  smm <- tbl %>% 
    group_by(barrio_residencia, category) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(category, count) %>% 
    NAer() %>% 
    retype() %>% 
    rename(BARRIO = barrio_residencia)
  own <- own %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, Hombre, Mujer)
  own <- rbind(own, smm) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    mutate(BARRIO = as.character(BARRIO))
  rsl <- inner_join(shp, own, by = c('BARRIO' = 'BARRIO'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('21_sexo', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_21()

# Mapa 22. Rangos edad  ---------------------------------------------------
lfm <- read_excel('../tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx')
own <- st_read('../data/shp/own/movilidad/barrios/22_rango_edad_0731.shp')
makeMap_22 <- function(){
  tbl <- lfm %>% 
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
    spread(category, count) %>% 
    NAer() %>% 
    retype() %>% 
    rename(BARRIO = barrio_residencia)
  own <- own %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, c_10:c_90) %>% 
    mutate(BARRIO = as.character(BARRIO))
  smm <- bind_rows(smm, own) %>% 
    NAer() %>% 
    retype() %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    mutate(BARRIO = as.character(BARRIO)) 
  rsl <- inner_join(shp, smm, by = c('BARRIO' = 'BARRIO'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('22_rangosedad', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_22()

# Mapa 23. Tiempo promedio espera -----------------------------------------
vjs <- read_csv('../tbl/join/tiempo_lfm.csv')
own <- st_read('../data/shp/own/movilidad/barrios/23_tiempo_promedio_espera_0731.shp')
makeMap_23 <- function(){
  tbl <- vjs %>% 
    dplyr::select(barrio_residencia, transporte_viaje, tespera) %>% 
    group_by(barrio_residencia) %>% 
    summarise(tespera = mean(tespera, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(barrio_residencia = iconv(barrio_residencia, to = 'latin1')) %>% 
    rename(BARRIO = barrio_residencia)
  own <- own %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, t_espera) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    rename(tespera = t_espera)
  rsl <- rbind(own, tbl) %>%   
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    mutate(BARRIO = as.character(BARRIO))
  rsl <- inner_join(shp, rsl, by = c('BARRIO' = 'BARRIO'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('23_tiempoespera', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_23()

# Mapa 24. Tiempo promedio trayecto ---------------------------------------
vjs <- read_csv('../tbl/join/tiempo_lfm.csv')
own <- st_read('../data/shp/own/movilidad/barrios/24_tiempo_promedio_trayecto_0731.shp')
makeMap_24 <- function(){
  tbl <- vjs %>% 
    dplyr::select(barrio_residencia, transporte_viaje, tviaje) %>% 
    group_by(barrio_residencia) %>% 
    summarise(tviaje = mean(tviaje, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(barrio_residencia = iconv(barrio_residencia, to = 'latin1')) %>% 
    rename(BARRIO = barrio_residencia)
  own <- own %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, t_trayecto) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    rename(tviaje = t_trayecto)
  print(unique(colnames(own) == colnames(tbl)))
  rsl <- rbind(own, tbl) %>%   
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    mutate(BARRIO = as.character(BARRIO))
  rsl <- inner_join(shp, rsl, by = c('BARRIO' = 'BARRIO'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('24_tiempoviaje', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_24()

# Tiempo total de viaje ---------------------------------------------------
vjs <- read_csv('../tbl/join/tiempo_lfm.csv')
own <- st_read('../data/shp/own/movilidad/barrios/25_tiempo_promedio_desplazamiento_0731.shp')
makeMap_25 <- function(){
  tbl <- vjs %>% 
    dplyr::select(barrio_residencia, transporte_viaje, tiempototal) %>% 
    group_by(barrio_residencia) %>% 
    summarise(tiempototal = mean(tiempototal, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(barrio_residencia = iconv(barrio_residencia, to = 'latin1')) %>% 
    rename(BARRIO = barrio_residencia)
  own <- own %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(BARRIO, t_promedio) %>% 
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    rename(tiempototal = t_promedio)
  print(unique(colnames(own) == colnames(tbl)))
  rsl <- rbind(own, tbl) %>%   
    group_by(BARRIO) %>% 
    summarize_all(.funs = sum) %>% 
    ungroup() %>% 
    mutate(BARRIO = as.character(BARRIO)) %>% 
    setNames(c('BARRIO', 'ttotal'))
  rsl <- inner_join(shp, rsl, by = c('BARRIO' = 'BARRIO'))
  rsl <- as(rsl, 'Spatial')
  writeOGR(obj = rsl, dsn = '../data/shp/own/movilidad/final', layer = paste0('25_tiempototal', dte), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  print('Done!')
}
makeMap_25()


