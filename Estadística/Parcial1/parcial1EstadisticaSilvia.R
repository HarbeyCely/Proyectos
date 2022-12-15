library(readxl)
DATOS <- read_excel("DATOS.xlsx", col_types = c("text", 
                                                "text", "numeric", "numeric"))
View(DATOS)
attach(DATOS)#cada columna es una variable
names(DATOS)
  
#ANALISIS DE FRECUENCIAS (TABLAS DE FRECUENCIAS)


##variables categoricas(cualitativas)
library(fdth)
DATOS$SEXO=as.factor(DATOS$SEXO)#declara que la variables es categórica
#as.character(DATOS$PAGAN)#declara la varaible numérica
tabla1=fdt_cat(DATOS$SEXO,sort=FALSE)
tabla1 #para mostrar tabla
plot(tabla1)

##variables numericas(cuantitativas)(ABAJOOO)
library(fdth)
max(PAGAN,na.rm=T)
fdt(DATOS$PAGAN,na.rm=T,start=0, end=3000000,h=500000))

  #MEDIDAS DE TENDENCIA CENTRAL


#media(promedio simple)
mean(PAGAN,na.rm = T)
#promedio recortado
mean(PAGAN,na.rm = T,trim=0.01)#trim=VALOR: porcentaje de recorte
#moda
library(modeest)
mlv(PAGAN,na.rm=T)
mfv(PAGAN,na.rm=T)
#mediana
median(PAGAN,na.rm=T)
  
  #MEDIDAS DE DISPERSIÓN


#rango
range(PAGAN,na.rm=T)
max(CUARTOS,na.rm=T)
min(CUARTOS,na.rm=T)
#varianza
var(PAGAN,na.rm=T)
#desviación estandar
sd(PAGAN,na.rm=T)
#coeficiente de variación
sd(PAGAN,na.rm=T) / mean(PAGAN,na.rm=T)
hist(PAGAN,na.rm=T, xlab ="cuartos", ylab ="frecuencia", main ="histograma de cuartos",col ="pink")
#coeficiente de asimetria (horizontal)
library(moments)
skewness(PAGAN,na.rm=T)##puede ser: positivo, negativo o cero
#coeficiente de curtosis (vertical)
kurtosis(PAGAN,na.rm=T)##puede ser: positivo, negativo o cero

  #ESTADISTICAS DESCRIPTIVA BIVARIADA


#coefiente de correlacion
library(ggplot2)
ggplot(DATOS, aes(x = CUARTOS,na.rm=T, y = PAGAN,na.rm=T)) + geom_point(color = "red",size = 3, shape = 1)#con geom_point() se escojen las caracteristicas de la grafica
cor(PAGAN,na.rm=T,CUARTOS,na.rm=T) # cor(x, y), No importa el orden
#RELACION ENTRE VARIABLES CATEGORICAS
library(gmodels)
CrossTable(SEXO,LYE, expected = F, prop.c = F, prop.t = F, prop.chisq = F)#FILAS
CrossTable(SEXO,LYE, expected = F, prop.r = F, prop.t = F, prop.chisq = F)#COLUMNAS
CrossTable(SEXO,LYE, expected = F, prop.t = F, prop.chisq = F)#COLUMNAS FILAS Y TOTAL
   #TABLAS DE CONTINGENCIA
   # expected =valor esperado
   # prop.c = proporcion columnas
   # prop.r = proporcion filas

#RELACION ENTRE VARIABLES CONTINUAS Y CATEGORICAS
library(crosstable)
library(flextable)#tablas bonitas
library(dplyr)
class(DATOS$SEXO)
class(DATOS$PAGAN)
crosstable(DATOS, c(SEXO, PAGAN), by=SEXO) %>%
  as_flextable(keep_id=TRUE)


#PARA CAMBIAR NOMBRES_colnames(Dates)[c(5,22)]=c("Q","P")
