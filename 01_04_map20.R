
###########################################################################
# Mapa de las causa de discapacidad de la población objeto
###########################################################################

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tbl <- read_excel('../tbl/join/base_identificacion_CIDSE_LFM_v3.xlsx')
brr <- st_read('../data/shp/base/bcs_barrios.shp')
lbl <- data.frame(value = c(1:8, 98), category = c('Nacio asi', 'Enfermedad', 'Accidente', 'Hechos violentos', 'Edad avanzada', 'Enfermedad profesional', 'Otra causa', 'No sabe', 'No responde')) %>% 
  mutate(category = as.character(category))

# Cleaning the shappefile -------------------------------------------------
brr <- brr %>% 
  mutate(BARRIO = iconv(BARRIO, from = 'UTF-8', to = 'latin1'))
       
# Cleaning the dataframe --------------------------------------------------
smm <- tbl %>% 
  dplyr::select(num_encuesta, fuente, dir_residencia, barrio_residencia, causadiscapacidad) %>% 
  inner_join(., lbl, by = c('causadiscapacidad' = 'value')) %>% 
  group_by(category, barrio_residencia) %>% 
  summarize(count = n()) %>% 
  ungroup()
write.csv(smm, '../tbl/maps/map_20_causadiscapacidad.csv', row.names = TRUE)


smm <- smm %>% 
  mutate(category = gsub(' ', '_', category)) %>% 
  spread(category, count) %>% 
  setNames(c('barrio', 'accidente', 'edad_av', 'enf', 'enf_prf', 'violenc', 'nacimi', 'nr', 'ns', 'otros'))

fnl <- inner_join(brr, smm, by = c('BARRIO' = 'barrio'))
fnl <- as(fnl, 'Spatial')
writeOGR(obj = fnl, dsn = '../data/gdb/order/match/ok', layer = 'mapa20_causa_discapacidad', driver = 'ESRI Shapefile')


