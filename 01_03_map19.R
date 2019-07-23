
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
brr <- st_read('../data/shp/base/bcs_barrios.shp') %>% 
  dplyr::filter(COMUNA %in% c(18, 17, 10, 19, 22))
shp_cds <- st_read('../data/gdb/order/match/maps/mapa19_lineas_deseo_CIDSE.shp') %>% 
  dplyr::select(ID_BARRI_1, BARRIO_1, COMUNA_1, T:O) %>% 
  rename(Tr = T)
tbl_lfm <- read_excel('../tbl/LFM/BASE DE DATOS VIAJES EDIT.xlsx') 
idf_lfm <- read_excel('../tbl/LFM/BASE DE DATOS SUJETOS.xlsx') %>% 
  dplyr::select(No, BARRIO, COMUNA)

# Luis Fernando Macea
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

write.csv(tbl_lfm, '../tbl/LFM/BASE DE DATOS VIAJES EDIT 2.csv', row.names = FALSE)

tbl_lfm <- read_csv('../tbl/LFM/BASE DE DATOS VIAJES EDIT 2.csv') %>% 
  group_by(BARRIO) %>% 
  summarize(Tr = sum(Tr),
            E = sum(E),
            D = sum(D),
            M = sum(M),
            S = sum(S),
            P = sum(P),
            O = sum(O)) %>% 
  ungroup() %>% 
  mutate(BARRIO = iconv(BARRIO, to = 'latin1'))
brr <- brr %>% 
  mutate(BARRIO = toupper(BARRIO))

shp_lfm <- inner_join(brr, tbl_lfm, by = c('BARRIO' = 'BARRIO'))

colnames(shp_cds)
colnames(shp_lfm)

shp_cds <- shp_cds %>% 
  setNames(c('ID_BARRIO', 'BARRIO', 'COMUNA', 'Tr', 'E', 'D', 'M', 'C', 'S', 'P', 'O', 'geometry')) %>% 
  dplyr::select(-C)
shp_lfm <- shp_lfm %>% 
  dplyr::select(ID_BARRIO, BARRIO, COMUNA, Tr:geometry)
fnl <- rbind(shp_cds, shp_lfm)
fnl <- as(fnl, 'Spatial')

writeOGR(obj = fnl,
         dsn = '../data/gdb/order/match/ok',
         layer = 'mapa_19_lineas_deseo',
         driver = 'ESRI Shapefile')
         


