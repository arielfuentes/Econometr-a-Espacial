# CASEN
library(haven)
#cargar casen2017 con variables de:
#ingreso
#nivel educacional
#ocupacion
#codigo comuna
#codigo region
#edad
#sexo
#tipo de atencion de salud
ruta_casen = "lab/data/Casen 2017.sav"
data = read_sav(ruta_casen)[,c("s12","expc","ypch","e6a","s28","oficio1","edad","sexo","comuna","region")]
#data=data_resp
#limitar area de estudio a la rm
data = data[data$region==13,]
summary(data)
#eliminar vacios
data = na.omit(data)
#eliminar quienes no declaran ocupacion
data = data[-which(data$oficio1==999),]
#eliminar quienes no declaran nivel educacional
data = data[-which(data$e6a==99),]
#eliminar los que no saben que sistema de salud pertenecen
data = data[-which(data$s12==99),]
summary(data)
#ver escolaridad
summary(as.factor(data$e6a))
#ver sistema de salud
summary(as.factor(data$s12))

################

resumen=aggregate(data$expc ~ data$region,data=data,FUN=sum)
colnames(resumen)=c("Region","Total")

resumen=aggregate(data$expc ~ data$region + data$comuna,data=data,FUN=sum)
colnames(resumen)=c("Region","Comuna","Total")

#-- analisis de pivote
library(reshape)
library(reshape2)
#install.packages("rpivotTable")
library(rpivotTable)

pivot = cast(data, # data de entrada
             region~oficio1, # filas y columnas
             value="expc", # valor
             fun=mean) # funcion de agrupacion

reverse = melt(pivot, # data de entrada
               c("region"), # filas 
               variable="oficio1", # columnas 
               value.name="expc") #nombre que tendrá el campo de valores

summary(as.factor(data$oficio1))
summary(as.factor(reverse$oficio1))

mean(data$expc)
mean(reverse$expc)

# visualización gráfica
rpivotTable(data)


# Merging
A=data[c(0:10),]
B=data[c(1000:1010),]

C=rbind(A,B)

data$index = 1:nrow(data)
A=data[c(0:10),c(9,1)]
B=data[c(0:10),c(9,2,3,4)]
C=cbind(A,B)

rbind()
cbind()
