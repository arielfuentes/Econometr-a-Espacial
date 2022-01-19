library(readxl)
library(dplyr)
library(sf)
library(tmap)
library(spgwr)
prop <- read_xlsx("lab/data/data.xlsx")
summary(prop)
prop <- filter(prop, !is.na(latitud) & longitud > -80 & longitud < -70 & latitud < -34) %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
map <- tm_shape(prop) +
  tm_dots(col = "tot_constr_m2")
tmap_leaflet(map)
##############darle centroide comuna
##############probar modelo lineal
####################aplicar técnicas de vaidación cruzada
######################modelo elástico