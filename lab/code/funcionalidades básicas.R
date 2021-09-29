library(tibble)
#objetivo: simular escenario de … [Definir]
#Crear datos
dataFrame = data.frame(matrix(ncol=6,nrow=10000))
colnames(dataFrame) = c("id","cantidad_pers","tipo_choque",
                        "hora","tipo_veh","peaton")

dataFrame$id = c(1:10000)
dataFrame$cantidad_pers = sample(1:50, nrow(dataFrame), replace = T)

# tipo choque
# 1: alcance
# 2: lateral 
# 3: frontal
# 4: vuelco
dataFrame$tipo_choque = sample(1:4, nrow(dataFrame), replace = T)
#tarea: agregar un nuevo campo llamado descr_choque donde se indique 
#a que tipo de choque pertenece cada código proveniente 
#desde tipo_choque

#ayuda: definir campo
dt_choque <- tibble(tipo_choque = 1:4, 
                    descr_choque = c("alcance", "lateral", "frontal", "vuelco"))

dataFrame <- merge(dataFrame, dt_choque, by = "tipo_choque")
  
rm(dt_choque)

dataFrame$hora = round(runif(nrow(dataFrame), 1, 4),0)

dataFrame$`3` = rnorm(nrow(dataFrame), 5, 7)
#tarea: ¿Qué sucede con la semilla?
#semilla (seed) es un vector entero que contiene un generador de número aleatorios
#explorar lo que sigue hasta antes de los ciclos / condiciones
#----
#Análisis exploratorio previo de datos
dim(data)
class(data)
names(data)
head(data)
summary(data)
#----
plot(dataFrame$`2`)
hist(dataFrame$`2`)
boxplot(dataFrame$`2`)
plot(density(dataFrame$`2`))
#----
#Manejo de datos
subData = dataFrame[c(1:10),]
subData = dataFrame[,c(2,3)]
attach(subData)
#Explorar: aggregate(data$campo,by=list(),FUN=)
#----
#Explorar que significan
#rbind()
#cbind()
#explorar as.
#Realizar operaciones
#----
#Ciclos
#Determinar condición y cambios en base a datos generados
#While
n = #condicion de borde
  i = #iterador
  data$campo = #nuevocalculo
  while (condicion){
    aux = data$campo[i] #auxiliar
    if (condicion) {
      data$campo[i] = "string"
      print(1)
    } 
    if (condicion) {
      data$campo[i] = "string"
      print(2)
    }
    i=i+1
  }

#for in
list = unique(data$campo)
for (variable in vector) {
  
}
