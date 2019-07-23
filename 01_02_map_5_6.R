
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Label -------------------------------------------------------------------
nms_dst_out <- data.frame(name = paste0('desout_', 6:14),
                          labl = c('SITM-MIO', 'TPC', 'Camperos', 'Taxi', 'Vehiculo particular', 'Moto', 'Bicicleta', 'Caminata', 'Otro'))
nms_dst_ins <- data.frame(name = paste0('ori_', 6:14),
                          labl = c('SITM-MIO', 'TPC', 'Camperos', 'Taxi', 'Vehiculo particular', 'Moto', 'Bicicleta', 'Caminata', 'Otro'))

# Load data ---------------------------------------------------------------
shp_cds <- st_read('../data/gdb/order/match/maps/mapa5_6_origen_destino_viajes_pcd_CIDSE.shp')
shp_ori_lfm <- st_read('../data/gdb/order/match/maps/mapa5_origen_viajes_pcd_MACEA.shp')
shp_dst_lfm <- st_read('../data/gdb/order/match/maps/mapa6_destino_viajes_pcd_CIDSE.shp')

# Cleaning LFM--- ----------------------------------------------------------
shp_ori_lfm <- shp_ori_lfm %>% 
  dplyr::transmute(BARRIO, ID_COMUNA, ID_BARRIO,
                   ori_MIO = MTV15 + MTV16 + MTV17 + MTV18,
                   ori_TPC = MTV11,
                   ori_Camperos = MTV8,
                   ori_Taxi = MTV8,
                   ori_vpart = MTV2,
                   ori_moto = MTV6,
                   ori_bici = MTV4,
                   ori_camn = MTV1,
                   ori_otro = MTV19)

shp_dst_lfm <- shp_dst_lfm %>% 
  dplyr::transmute(BARRIO, ID_COMUNA, ID_BARRIO,
                   dst_MIO = MTVR15 + MTVR16 + MTVR17 + MTVR18,
                   dst_TPC = MTVR11,
                   dst_Camperos = MTVR8,
                   dst_Taxi = MTVR8,
                   dst_vpart = MTVR2,
                   dst_moto = MTVR6,
                   dst_bici = MTVR4,
                   dst_camn = MTVR1,
                   dst_otro = MTVR19) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

shp_lfm <- inner_join(shp_ori_lfm, shp_dst_lfm, by = c('BARRIO' = 'BARRIO', 'ID_COMUNA' = 'ID_COMUNA', 'ID_BARRIO' = 'ID_BARRIO'))

# Cleaning CIDSE ----------------------------------------------------------
shp_cds <- shp_cds %>% 
  dplyr::select(BARRIO_1, 
                COMUNA_1, 
                ID_BARRIO,
                as.character(pull(nms_dst_out, 1)),
                as.character(pull(nms_dst_ins, 1))) %>% 
  mutate(dst_MIO = desout_6,
         dst_TPC = desout_7,
         dst_Camperos = desout_8,
         dst_Taxi = desout_9,
         dst_vpart = desout_10,
         dst_moto = desout_11,
         dst_bici = desout_12,
         dst_camn = desout_13,
         dst_otro = desout_14,
         ori_MIO = ori_6,
         ori_TPC = ori_7,
         ori_Camperos = ori_8,
         ori_Taxi = ori_9,
         ori_vpart = ori_10,
         ori_moto = ori_11,
         ori_bici = ori_12,
         ori_camn = ori_13,
         ori_otro = ori_14) %>% 
  dplyr::select(BARRIO_1, COMUNA_1, ID_BARRIO, dst_MIO:ori_otro)

# Join all into only one shapefile ----------------------------------------
shp_cds <- shp_cds %>% 
  rename(BARRIO = BARRIO_1,
         ID_COMUNA = COMUNA_1)
shp_cds <- shp_cds %>% dplyr::select(sort(colnames(shp_cds)))
shp_lfm <- shp_lfm %>% dplyr::select(sort(colnames(shp_lfm)))
shp_all <- rbind(shp_lfm, shp_cds)

plot(st_geometry(shp_all), col = 'green')

shp_all <- as(shp_all, 'Spatial')

writeOGR(obj = shp_all,
         dsn = '../data/gdb/order/match/ok',
         layer = 'mapa5_6_origen_destino',
         driver = 'ESRI Shapefile')
