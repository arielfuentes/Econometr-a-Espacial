# Dependencias
library(zoom)
library(spdep)
library(classInt) 
library(RColorBrewer)
library(tmap)
library(maptools)

# Datos
fcDir = 'C:/Users/claud/Downloads/moran/random_points.shp'
fcp <- readShapeSpatial(fcDir)

fcDirPoly = 'C:/Users/claud/Downloads/moran/comunas_moran.shp'
fcPoly = readShapeSpatial(fcDirPoly)

# Vista
coords <- coordinates(fcPoly)
par(mar=c(0,0,0,0))
plot(fcPoly, border="grey")
plot(fcp, cex=0.4, pch= 20, col = 'red', add=TRUE)
zm()


# Moran

# 1) Definir Vecinos

## Para el caso de Puntos
## Necesario definir vecinos
nN = knearneigh(fcp, k = 4)
w = knn2nb(nN)
## Mostrar vecinos
coords <- coordinates(fcp)
par(mar=c(0,0,0,0))
plot(fcPoly, border="grey")
plot(w, coords, add=TRUE)


## Para polígonos
## Definir dependencia espacial
nb = poly2nb(fcPoly, queen = TRUE)
## Mostrar vecinos
coords = coordinates(fcPoly)
par(mar=c(0,0,0,0))
plot(fcPoly, border="grey")
plot(nb, coords, add=TRUE)

# 2) Asignar pesos

## Para el caso de puntos
lw.point = nb2listw(w, style="W", zero.policy=TRUE)

## Para polígonos
lw.pol = nb2listw(nb, style="W", zero.policy=TRUE)


# 3) Moran
# Los valores oscilan entre 
# -1 (indicando dispersión perfecta) a 
# 1 (correlación perfecta / Clustered Data). 
# Un valor 0 indica distribución aleatoria

moran(x = fcp$random2, 
      listw = lw.point,
      n = length(w), 
      Szero(lw.point))[1]

# La hipótesis que se plantea es que los valores asociados a una variable
# se distribuyen de forma aleatorea a lo largo de un territorio
# siguiendo un proceso completamente aleatorio

# Para probar esta hipótesis se utilizan dos métodos:
# a) Método analítico (Global Moran)

moran.test(fcp$random,lw.point)
moran.test(fcp$random2,lw.point)
moran.test(fcp$random3,lw.point)

# b) Método de Monte Carlo (Global Moran) 

moran.mc = moran.mc(fcp$random2, lw.point, nsim=999)
plot(moran.mc)

# Local
mLocal = localmoran(fcp$random2,lw.point)
intervalos = quantile(mLocal[,1], probs = seq(0,1,0.125)) + c(rep(0,8),0.001)
listQ = round(intervalos, 2)

INT = classIntervals(mLocal[,1], style="fixed",
                      fixedBreaks=listQ)
COLORES = brewer.pal(9, "Reds")
COL = findColours(INT, COLORES)

par(mar=c(0,0,0,0))
plot(fcPoly, border="grey")
plot(fcp, pch=20, col=COL, add=TRUE)

TB = attr(COL, "table")
legtext = paste(names(TB))
legend("bottomright", fill=attr(COL, "palette"), legend=legtext, bty="n", cex=1)

# Otra forma de mostrar resultados
# Unir valores a data original
moran.map <- cbind(fcp, mLocal)
tm_shape(moran.map) + 
  tm_dots(col = "Ii",
          style = "quantile",
          title = "local moran statistic") #si es poligono se puede utilizar tm_fill

# 4) Graficar
# El diagrama de esparcimiento de Moran es una manera 
# adicional de visualizar la dependencia espacial. 
# Los cuadrantes (Q1 - superior derecho,
# Q2 - superior izquierdo, 
# Q3 -inferior izquierdo y 
# Q4 - inferior derecho) 
# pueden ser interpretados como:

# Q1 y Q3 - indican puntos de asociación espacial positiva, 
# en el sentido de que una localización posee vecinos con valores semejantes;

# Q2 y Q4 - indican puntos de asociación espacial negativa, 
# en el sentido de que una localización posee vecinos con valores distintos, 
# indicando puntos de transición entre diferentes patrones espaciales.

moran.plot(fcp$random2,lw.point)


# Links de interés
# https://rpubs.com/quarcs-lab/spatial-autocorrelation
# https://mgimond.github.io/simple_moransI_example/#step_5:_performing_a_hypothesis_test
# https://rstudio-pubs-static.s3.amazonaws.com/432208_e867e9a0aa454dca8cbf9df9d746cd11.html#i_de_moran_global


## Ejemplo a poligono
moran(x = fcPoly$DENS_HOGAR, 
      listw = lw.pol,
      n = length(nb), 
      Szero(lw.pol))[1]

moran.test(fcPoly$DENS_HOGAR,lw.pol)

moran.plot(fcPoly$DENS_HOGAR,lw.pol)

mLocal = localmoran(fcPoly$DENS_HOGAR,lw.pol)
moran.map <- cbind(fcPoly, mLocal)
tm_shape(moran.map) + 
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic") #si es poligono se puede utilizar tm_fill
