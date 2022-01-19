#Aplicación GWR

# 1)Importar información

#Ver base de datos
library(readr)
directory = "C:/Users/claud/Documents/gwr/data/zurich_data.csv"
data <- read_csv(directory)
#Eliminar primer campo de indexación
data = data[,c(2:ncol(data))]
#ver nombres
names(data)

#"rent"       "size"       "lift"       "balcony"    "terrace"    "is_house"  
#"slope"      "highway"    "rail_stati" "sun_eve"    "X"          "Y"


# 2) Trabajar como dato geografico
#Cargar libreria de mapas
library(maptools)
#Cargar shape limite del area de estudio senalando nombre y extension de capa
directory = "C:/Users/claud/Documents/gwr/data/gemeinden_seen_f_250.shp"
shp = readShapeSpatial(directory) 
#Visualizar limite geografico
plot(shp)
#-------------------------
#-------------------------

# 3) Preparar datos para calibración de modelo
# 3.1) Crear y preparar grilla de trabajo

#Crear grilla en función de la capa cargada anteriormente y designando un tamaño de celda
grid_shp=makegrid(shp,cellsize = 1000)
#Generar un Clip (mantener puntos de grilla que se encuentren dentro del limite geografico)
clip_grid_shp=over(SpatialPoints(grid_shp),shp)
#Visualizar limite geografico y puntos de grilla dentro de el
plot(shp)
index_clip=which(is.na(clip_grid_shp$BFS)==F)
points(grid_shp[index_clip,])

# 3.2) Crear puntos aleatorios dentro del limite geografico
random_points = spsample(shp,n=300,"random")
plot(shp)
points(random_points)

# 3.3) Utilizar los propios puntos observados
# Se consideran las mismas coordenadas X,Y del dataset


#--------------------------
#--------------------------

# 4) Preparando información y el modelo

## Seleccionar datos del modelo
model_data = data[,c(-11,-12)]
## Formula
model = "rent~size + lift + balcony + terrace + is_house + slope + highway + rail_stati + sun_eve"

# 4.1) Trabajar con grilla
# 4.1.1) Crear matriz espacial en función de campos de coordenadas y 
# campos que contienen registros
spdf=SpatialPointsDataFrame(coords=data[,c(11,12)],data=model_data)

library(spgwr)
# 4.1.2) Ancho de banda y GWR (ver log en anexo 1)
bw = gwr.sel(model,spdf,adapt = T,RMSE=T) # 0.005248066

# 4.1.3) Estimar coeficientes con libreria gwr
# Considerando: 
# - Modelo
# - Ancho de banda calculado
# - Puntos de la grilla que se encuentran dentro del limite geografico

model_gwr=gwr(model,spdf,adapt = bw,fit.points = SpatialPoints(grid_shp[index_clip,]))

# 4.1.4) Obtener betas y visualizacion
# Obtener todos los betas
# coef = data.frame(model_gwr$SDF)
# Graficar los betas
coef_data = data.frame(model_gwr$SDF)
coef_list = names(coef_data[,c(-12,-13,-14)])
for (coef in coef_list){
  coef_spdf = SpatialPointsDataFrame(grid_shp[index_clip,],coef_data[coef])
  print(spplot(coef_spdf, colorkey=T, main = coef))
}

## Revisar
# https://edzer.github.io/sp/#using-lattice-plot-spplot

#--------------------------
#--------------------------

# 4.2) Trabajar con puntos aleatorios
# 4.2.1)
# -
# 4.2.2)
# -
# 4.2.3) Estimar coeficientes con libreria gwr
# Considerando: 
# - Modelo
# - Ancho de banda calculado
# - Puntos aleatorios que se encuentran dentro del limite geografico

model_gwr=gwr(model,spdf,adapt = bw,fit.points = random_points)

# 4.2.4) Obtener betas y visualizacion
coef_data = data.frame(model_gwr$SDF)
coef_list = names(coef_data[,c(-12,-13,-14)])
for (coef in coef_list){
  coef_spdf = SpatialPointsDataFrame(random_points,coef_data[coef])
  print(spplot(coef_spdf, colorkey=T, main = coef))
}

#--------------------------
#--------------------------

# 4.2) Trabajar con propias observaciones
# 4.2.1)
# -
# 4.2.2)
# -
# 4.2.3) Estimar coeficientes con libreria gwr
# Considerando: 
# - Modelo
# - Ancho de banda calculado
# - Puntos observados

model_gwr=gwr(model,spdf,adapt = bw,fit.points = SpatialPoints(data[,c(11,12)]))

# 4.2.4) Obtener betas y visualizacion
coef_data = data.frame(model_gwr$SDF)
coef_list = names(coef_data[,c(-15:-27)])
for (coef in coef_list){
  coef_spdf = SpatialPointsDataFrame(SpatialPoints(data[,c(11,12)]),coef_data[coef])
  print(spplot(coef_spdf, colorkey=T, main = coef))
}

#-------------------------
# Anexo
#-------------------------

# 1) Log bw (ancho de banda)

# Adaptive q: 0.381966 CV score: 491.732 
# Adaptive q: 0.618034 CV score: 510.467 
# Adaptive q: 0.236068 CV score: 475.0068 
# Adaptive q: 0.145898 CV score: 455.2891 
# Adaptive q: 0.09016994 CV score: 430.556 
# Adaptive q: 0.05572809 CV score: 406.7512 
# Adaptive q: 0.03444185 CV score: 387.2917 
# Adaptive q: 0.02128624 CV score: 371.1502 
# Adaptive q: 0.01315562 CV score: 359.6094 
# Adaptive q: 0.008130619 CV score: 352.6749 
# Adaptive q: 0.005024999 CV score: NA 
# Adaptive q: 0.01005 CV score: 355.1833 
# Adaptive q: 0.006944377 CV score: 351.3871 
# Adaptive q: 0.00621124 CV score: 349.9996 
# Adaptive q: 0.005758136 CV score: 349.5688 
# Adaptive q: 0.00538593 CV score: 349.044 
# Adaptive q: 0.0055281 CV score: 349.198 
# Adaptive q: 0.005248066 CV score: 348.793 
# Adaptive q: 0.005162862 CV score: NA 
# Adaptive q: 0.005300726 CV score: 348.9064 
# Adaptive q: 0.005207376 CV score: NA 
# Adaptive q: 0.005248066 CV score: 348.793 
