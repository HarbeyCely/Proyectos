# Importar datos
library(readxl)
Datos <- read_excel("C:/Users/hecm-/Downloads/Datos.xlsx", 
                    sheet = "Variables", col_types = c("numeric", 
                                                       "text", "text", "text", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric"))
attach(Datos)
names(Datos)
# Tabla de contingencia categorica vs numerica

library(crosstable)
#library(flextable)
#library(dplyr)
class(Datos$ZONA)
class(Datos$MortalidadMenores)

crosstable(Datos, c(ZONA, MortalidadMenores), by=ZONA) %>%
  as_flextable(keep_id=TRUE)

# Tabla de contingencia categorica vs categorica

library(gmodels)

t1=CrossTable(Departamento, ZONA, prop.c = F, prop.t = F,
           expected = F, prop.chisq = F) # Columna

CrossTable(Departamento, ZONA, prop.r = F, prop.t = F,
           expected = F, prop.chisq = F) # Fila

table(Departamento,ZONA)
library(xlsx)
write.xlsx(t1, "t2.xlsx")
getwd()

# Coeficiente de correlacion
library(ggplot2)
ggplot(Datos, aes(x = CobAcueducto, y = Saber11Mate)) + 
  geom_point(color = "black", size = 3, shape = 3)
cor(CobAcueducto,Saber11Mate)

ggplot(Datos, aes(x = Saber11Mate, y = CobAcueducto)) + 
  geom_point(color = "orange", size = 3, shape = 3)
cor(Saber11Mate,CobAcueducto)

#MEDIDAS DE TENDENCIA CENTRAL
#promedio
mean(Saber11Mate,na.rm = T)
#promedio recortado
mean(Saber11Mate,na.rm = T,trim=0.1)
#moda
library(modeest)
mfv(Saber11Mate,na.rm=T)
#mediana
median(Saber11Mate,na.rm=T)

#MEDIDAS DE DISPERSIÓN
#rango
range(Saber11Mate,na.rm=T)
max(Saber11Mate,na.rm=T)
min(Saber11Mate,na.rm=T)
#varianza
var(Saber11Mate,na.rm=T)
#desviación estandar
sd(Saber11Mate,na.rm=T)
#coeficiente de variación
sd(Saber11Mate,na.rm=T) / mean(Saber11Mate,na.rm=T)
hist(Saber11Mate,na.rm=T, xlab ="Notas 0-100", ylab ="frecuencia", main ="histograma de Notas",col ="orange")
#coeficiente de asimetria (horizontal)
library(moments)
skewness(Saber11Mate,na.rm=T)
#coeficiente de curtosis (vertical)
kurtosis(Saber11Mate,na.rm=T)
