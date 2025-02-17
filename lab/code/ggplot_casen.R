#casen2015
#pobreza_multi_5d : Situaci�n de pobreza multidimensional con Entorno y Redes  (5 Dimensiones) {1:"No pobre",2:"Pobre}
#pobreza_multi_4d : Situaci�n de pobreza multidimensional (4 Dimensiones) {1:"No pobre",2:"Pobre}
#ypc : Ingreso per c�pita del hogar
#pobreza : Situaci�n de pobreza por ingresos {1:"Pobres extremos",2:"Pobres no extremos",3:"No pobres"}
#region : Regi�n
#comuna : Comuna
#zona : Zona {1:"Urbano",2:"Rural"}
#expr : Expansi�n regional
#expc : Expansi�n comunas auto_representadas
#expr_div : Expansi�n orientaci�n sexual (r21) e identidad de g�nero (r22 )
#sexo : Sexo {1:"Hombre",2:"Mujer"}
#edad : Edad
#numper : N�mero de personas en el hogar (excluye SDPA)
#ypch : Ingreso total per c�pita del hogar
#tot_hog : Total hogares en la vivienda
#tot_per : Total de personas (incluye SDPA)
#
#casen2013
#
#pobreza_MN : Pobreza asignado por ingresos {1:"Pobres extremos",2:"Pobres no extremos",3:"No pobres"}


setwd("C:/Users/claud/Downloads/ggplot")
#cargar dependencias
library(readr)
library(ggplot2)
# agrupar graficos ggplot
library(gridExtra)


#Base CASEN preprocesada
base_casen <- read_csv("base_casen.csv")

###########
##graficos
###########
#conservar solo informaci�n con situaci�n en pobreza

base_casen=base_casen[base_casen$pobreza2=="Pobres extremos" |
                        base_casen$pobreza2=="Pobres no extremos",]

#Resumir base
df=aggregate(base_casen$p_reg,by=list(base_casen$region,
                                      base_casen$pobreza2,
                                      base_casen$pobreza_multi2,
                                      base_casen$anne),FUN=sum)

colnames(df)=c("region","pobreza2","pobreza_multi2","anne","p_reg")
#crear objeto, con la base asignar las regiones como eje x.
#Cada barra ser� rellenada seg�n el nivel de pobreza
grafico_1=ggplot(df,
                 aes(x=region,fill=as.factor(pobreza2))) + 
  #Como se trabaja a nivel regional, el factor de expansi�n considera
  #el correspondiente al de regi�n (p_reg) bara determinar la altura de barras
  geom_bar(aes(weight=p_reg)) +
  #Cambiar elementos est�ticos del gr�fico
  labs(title= paste("Nivel de pobreza porcentual"), 
       subtitle="Seg�n resultado de situaci�n asignada en CASEN 2011, 2013 y 2015",
       x="Regi�n",
       y="% de poblaci�n",
       fill= "Situaci�n de pobreza") + 
  #cambiar est�tica del eje x
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  #asegurar que aparezcan todas las etiquetas del eje x (regiones)
  #generando una lista que contenga como registro �nico el c�digo
  #censal regional
  scale_x_continuous(breaks = unique(df$region)) +
  scale_y_continuous(breaks = seq(round(min(df$p_reg),0), round(max(df$p_reg)+15,0), by = 2)) +
  #separar por a�o
  facet_wrap(~anne,ncol=1) +
  geom_hline(yintercept = c(5,10,15,20,25), color= "black",alpha=0.3)
grafico_1

#repetir pero con pobreza multidimensional
grafico_2=ggplot(df,
                 aes(x=region,fill=as.factor(pobreza_multi2))) + 
  #Como se trabaja a nivel regional, el factor de expansi�n considera
  #el correspondiente al de regi�n (p_reg) bara determinar la altura de barras
  geom_bar(aes(weight=p_reg)) +
  #Cambiar elementos est�ticos del gr�fico
  labs(title= paste("Nivel de pobreza multidimensional porcentual"), 
       subtitle="Seg�n resultado de situaci�n asignada en CASEN 2011, 2013 y 2015",
       x="Regi�n",
       y="% de poblaci�n",
       fill= "Situaci�n de pobreza") + 
  #cambiar est�tica del eje x
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  #asegurar que aparezcan todas las etiquetas del eje x (regiones)
  #generando una lista que contenga como registro �nico el c�digo
  #censal regional
  scale_x_continuous(breaks = unique(df$region)) +
  scale_y_continuous(breaks = seq(round(min(df$p_reg),0), round(max(df$p_reg)+15,0), by = 2)) +
  #separar por a�o
  facet_wrap(~anne,ncol=1) +
  geom_hline(yintercept = c(5,10,15,20,25), color= "black",alpha=0.3)
grafico_2

grid.arrange(grafico_1, grafico_2,
             nrow = 1)


#######
#calcular variaci�n de pobreza entre periodos
#crear nueva tabla
df_var=aggregate(base_casen$p_reg,by=list(base_casen$region,
                                          base_casen$pobreza2,
                                          base_casen$anne,
                                          base_casen$zona2),FUN=sum)

colnames(df_var)=c("region","pobreza2","anne","zona2","p_reg")

df_var=cbind(df_var[df_var$anne=="2013",],
             df_var[df_var$anne=="2015",],
             df_var[df_var$anne=="2011",])

df_var=df_var[,c(1,2,4,5,10,15)]
colnames(df_var)=c("region","Pobreza","zona2","2013","2015","2011")
df_var$var=((df_var$`2015`-df_var$`2013`)+(df_var$`2013`-df_var$`2011`))/2

grafico_3 = ggplot(data=df_var, aes(x=region, y=var, group=Pobreza)) +
  geom_hline(yintercept = c(0,-0.5,-1,-1.5,0.5), color= "red",alpha=0.3) +
  geom_line(aes(color=Pobreza,linetype=Pobreza)) +
  geom_point(aes(color=Pobreza,shape=Pobreza)) +
  scale_color_brewer(palette="Dark2") +
  facet_wrap(~zona2) +
  scale_x_continuous(breaks = unique(df_var$region)) +
  scale_y_continuous(breaks = seq(round(min(df_var$var/2)-1.5,0),
                                  round(max(df_var$var/2)+1,0), by = 0.5)) +
  labs(title= paste("Variaci�n del nivel de pobreza promedio por regi�n y zona."), 
       subtitle="Seg�n resultado de situaci�n asignada en CASEN 2011, 2013 y 2015",
       x="Regi�n",
       y="% de variaci�n (2011 - 2013 - 2015)")
grafico_3

#calcular variaci�n de pobreza multidimensional entre periodos
#crear nueva tabla
df_var=aggregate(base_casen$p_reg,by=list(base_casen$region,
                                          base_casen$pobreza_multi2,
                                          base_casen$anne,
                                          base_casen$zona2),FUN=sum)

colnames(df_var)=c("region","pobreza_multi2","anne","zona2","p_reg")

df_var=cbind(df_var[df_var$anne=="2013",],
             df_var[df_var$anne=="2015",],
             df_var[df_var$anne=="2011",])

df_var=df_var[,c(1,2,4,5,10,15)]
colnames(df_var)=c("region","Pobreza","zona2","2013","2015","2011")
df_var$var=((df_var$`2015`-df_var$`2013`)+(df_var$`2013`-df_var$`2011`))/2

grafico_4 = ggplot(data=df_var, aes(x=region, y=var, group=Pobreza)) +
  geom_hline(yintercept = c(0,-0.5,-1,-1.5,0.5), color= "red",alpha=0.3) +
  geom_line(aes(color=Pobreza,linetype=Pobreza)) +
  geom_point(aes(color=Pobreza,shape=Pobreza)) +
  scale_color_brewer(palette="Dark2") +
  facet_wrap(~zona2) +
  scale_x_continuous(breaks = unique(df_var$region)) +
  scale_y_continuous(breaks = seq(round(min(df_var$var/2)-1.5,0),
                                  round(max(df_var$var/2)+1,0), by = 0.5)) +
  labs(title= paste("Variaci�n del nivel de pobreza multidimensional promedio por regi�n y zona."), 
       subtitle="Seg�n resultado de situaci�n asignada en CASEN 2011, 2013 y 2015",
       x="Regi�n",
       y="% de variaci�n (2011 - 2013 - 2015)",
       fill= "Situaci�n de pobreza")
grafico_4


grid.arrange(grafico_1, grafico_2,
             nrow = 1)
grid.arrange(grafico_3, grafico_4,
             nrow = 2)



### piramide etaria

grafico_5=ggplot(base_casen, aes(x = edad2, fill = sexo2)) + 
  geom_bar(data= subset(base_casen,sexo2=="Mujer"),aes(weight=p_reg)) + 
  geom_bar(data= subset(base_casen,sexo2=="Hombre"),aes(weight=-p_reg)) +
  coord_flip()  + 
  scale_y_continuous(breaks = seq(-100, 100, 20), 
                     labels = as.character(abs(seq(-100,100,20)))) + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw() +
  facet_wrap(region~zona2) +
  labs(title= paste("Porcentaje de pobreza regional seg�n rango etario por zona y sexo"), 
       subtitle="Seg�n resultado de situaci�n asignada en CASEN 2015",
       x="Regi�n",
       y="Edad",
       fill= "Sexo")

grafico_5 
