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

## PUNTO2
#En la tabla se puede ver que, aunque en varios departamentos el porcentaje de
#municipios clasificados como Zonas futuro es 0%, no todos lo son como es el caso
#de Nariño donde el porcentaje de municipios clasificados como Zonas futuro es de
#12,5 %, o el caso de Norte de Santander con 8 municipios clasificados como Zonas
#futuro y eso equivale al 20% de sus municipios o un caso extremo como Guaviare
#donde el 75% de sus municipios es considerado Zona futuro.
#Por tanto, se puede decir que la afirmación es errónea ya que no todos los
#Departamentos tienen el mismo porcentaje de sus municipios clasificados como
#Zonas Futuro.

## PUNTO4
#Para hacer Muestreo aleatorio Simple se deben seguir los siguientes pasos:
#1) En una maquina o Software designar un número aleatorio a todas las personas o posibles encuestados.
#2) Una vez hecho esto Ordenar de mayor a menor los números aleatorios asignados a cada persona
#3) Finalmente se selecciona a las personas que han quedado con los números mayores de acuerdo con la cantidad
#de personas que va a tener la muestra, este dato de personas que va a tener la muestra ya se conoce previamente.
#Es decir que si tengo 100 potenciales encuestados pero solo puedo encuestar a 10 entonces escojo a las 10 personas
#que quedaron con los 10 números mayores.

