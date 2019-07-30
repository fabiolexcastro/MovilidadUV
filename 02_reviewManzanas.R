

# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
tbl <- read_excel('../tbl/MUESTRA_piloto_manzana_comunas_30Julio.xlsx')
shp <- st_read('../data/shp/base/bcs_manzanas_comunas.shp')

# Join between the shapefile and the table (sample)
fnl <- inner_join(shp, tbl, by = c('IDMANZANA' = 'IDMANZANA'))
fnl <- as(fnl, 'Spatial')
writeOGR(obj = fnl, dsn = '../data/shp/manzanas', layer = 'manzanas_muestra_v2', driver = 'ESRI Shapefile')
