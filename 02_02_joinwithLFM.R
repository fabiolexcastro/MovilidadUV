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
shp <- st_read('../data/shp/base/bcs_barrios_geo.shp') %>% 
  mutate(BARRIO = as.character(BARRIO))
dte <- 0731

# Mapa 7. Barreras ida moto y bicicleta -----------------------------------
own <- st_read('../data/shp/own/movilidad/barrios/07_barrera_ida_bicimoto_0731.shp')
lfm_mto <- st_read('../data/shp/own/movilidad/lfm/07_barreras_ida_moto.shp')
lfm_bcc <- st_read('../data/shp/own/movilidad/lfm/07_barreras_ida_bicicleta.shp')
makeMap_07 <- function(){
  sh1 <- lfm_mto %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B7) %>% 
    setNames(c('BARRIO', 'm_abordar', 'm_ubicar', 'm_manejar', 'm_retr', 'm_leer', 'm_pr_pav', 'm_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  sh2 <- lfm_bcc %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B6) %>% 
    setNames(c('BARRIO', 'b_ubicar', 'b_manejar', 'b_leer', 'b_movili', 'b_pr_pav', 'b_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  dfm <- inner_join(sh1, sh2, by = 'BARRIO')
  colnames(dfm)
  own <- own %>% 
    as.data.frame() %>% 
    dplyr::select(BARRIO, m_abrdr, m_ubicr, m_manjr, m_retr, m_leer, m_pr_pv, m_bajar, b_ubicr, b_leer, b_movil, b_pr_pv, b_bajar) 
    # setNames(c('BARRIO', 'm_abrdr', 'b_bajar', 'b_dcdrr', 'b_leer', 'b_movil', 'b_pr_pv', 'b_ubicr', 'm_abrdr', 'm_bajar', 'm_leer', 'm_manjr', 'm_pr_pv', 'm_retr', 'm_ubicr'))
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
  sh1 <- lfm_mto %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B7) %>% 
    setNames(c('BARRIO', 'm_abordar', 'm_ubicar', 'm_manejar', 'm_retr', 'm_leer', 'm_pr_pav', 'm_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  sh2 <- lfm_bcc %>% 
    dplyr::select(BARRIO, Ave_B1:Ave_B6) %>% 
    setNames(c('BARRIO', 'b_ubicar', 'b_manejar', 'b_leer', 'b_movili', 'b_pr_pav', 'b_bajar', 'geometry')) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  sh3 <- own %>% 
    dplyr::select(BARRIO, abordar, ubicar, bajar, dcdrrecor, leer, retr, pr_pav, movili, manejar) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  dfm <- bind_rows(dfm, sh3) 
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

# Mapa 178 Barreras regreso peaton --------------------------------------------
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
lfm <- read_excel('../tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx')
makeMap_18 <- function(){
  
  tbl_lfm <- tbl_lfm[,c('No.',
                        'COMUNA',
                        'GÉNERO',
                        "MOTIVO DE VIAJE: PARA IR A TRABAJAR",
                        "MOTIVO DE VIAJE: PARA IR A ESTUDIAR",
                        "MOTIVO DE VIAJE: PARA REALIZAR ACTIVIDADES DEPORTIVAS O CULTURALES",
                        "MOTIVO DE VIAJE: PARA IR A ATENCIÓN MÉDICA",
                        "MOTIVO DE VIAJE: PARA REALIZAR ACTIVIDADES SOCIALES",
                        "MOTIVO DE VIAJE: PARA ASUNTOS PERSONALES",
                        "MOTIVO DE VIAJE: OTRO")]
  colnames(tbl_lfm) <- gsub(' ', '_', colnames(tbl_lfm))
  tbl_lfm <- tbl_lfm %>% 
    setNames(c('Numero', 'Comuna', 'Genero', 'Tr', 'E', 'D', 'M', 'S', 'P', 'O')) 
  tbl_lfm <- inner_join(tbl_lfm, idf_lfm, by = c('Numero' = 'No'))
  tbl_lfm <- tbl_lfm %>% 
    group_by(Comuna, BARRIO) %>% 
    summarize(Tr = sum(Tr),
              E = sum(E),
              D = sum(D),
              M = sum(M),
              S = sum(S),
              P = sum(P),
              O = sum(O)) %>% 
    ungroup()
  
  
  
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



