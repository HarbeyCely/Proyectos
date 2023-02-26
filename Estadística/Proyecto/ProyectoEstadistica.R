# Librerias
library(TeachingSampling)
library(modeest)
library(moments)
library(fdth)
# Lectura de datos
library(readxl)
df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT9eHtUFHecxTt_JizLjH20yHtp0dqpUxiZNYe3htgk-b6Vm9daEUzG7q0flP5IcR5CaZsZDtbD9mBc/pub?output=csv")
#df <- read_excel("C:/Users/hecm-/Desktop/EncuestaEstadistica (Respuestas).xlsx", 
#                                              col_types = c("date", "numeric", "numeric", 
#                                                            "text", "text", "numeric", "text", 
#                                                            "numeric", "numeric", "numeric", 
#                                                            "numeric", "text", "text"))
attach(df)
names(df)

## Prueba piloto
colnames(df) [c(11)]=c("promedio")
colnames(df) [c(2)]=c("codigo")
set.seed(0905)
pp=S.SI(11,8) # MAS
table(pp)
piloto=df$codigo[pp]
v_p=var(piloto$promedio)
d_p=sd(piloto$promedio)
desv_deseada=(d_p)/4
var_deseada=desv_deseada^2
awdad=v_p/4
N=6612
## tamaño de muestra
n=(N*v_p)/((N*var_deseada)+v_p)
#                          MUESTREO POR CONGLOMERADO
# Aleatorizacion con MAS
set.seed(90)
# PARA CARRERA
# 1 -> Ingenieria civil
# 2 -> Diseño
# 3 -> E3T
# 4 -> Ingenieria industrial
# 5 -> Ingenieria mecánica
# 6 -> Ingenieria de sistemas

a=sample(1:6,1)
if (a==3){
  print("1 -> electrica")
  print("2 -> electronica")
  sample(1:2,1)
}
# Para semestre
b=sample(1:10,1)

# Para asignatura
c=sample(1:3,1)

# Para grupo
d=sample(1:5,1)

sprintf("Carrera %d",a)
sprintf("Semestre %d",b)
sprintf("Asignatura %d",c)
sprintf("Grupo %d",d)

#                       PROCESAMIENTO DE DATOS
# Estadistica descriptiva para cada variable

#                  EDAD
# Tendencia central
sprintf("Promedio: %d", round(mean(df$Edad), 0))
median(df$Edad)
# Medidas de dispersión
sprintf("Desviación: %f", sd(df$Edad))
sprintf("Varianza: %f", var(df$Edad))
limSup = mean(df$Edad) + sd(df$Edad)
limInf = mean(df$Edad) - sd(df$Edad)
rango = limSup-limInf
sprintf("Rango: %f", rango)
hist(df$Edad, xlab = "Edades", ylab = "frecuencia",
     main = "Histograma de Edad", col = "orange")
# Medidas de distrubución
sprintf("Coeficiente de variación: %f", sd(df$Edad)/mean(df$Edad))
sprintf("Coeficiente de asimetría: %f", skewness(df$Edad))
sprintf("Coeficiente de curtosis: %f", kurtosis(df$Edad))

#                  SEXO
# Tendencia central
sprintf("Moda: %s", mfv(df$Sexo))
# Tabla de frecuencias
TablaSexo=fdt_cat(df$Sexo,sort=FALSE)
print("Tabla de frecuencias")
TablaSexo
#                  CARRERA
# Tendencia central
colnames(df) [c(5)]=c("Carrera")
sprintf("Moda: %s", mfv(df$Carrera))
# Tabla de frecuencias
TablaCarrera=fdt_cat(df$Carrera,sort = F)
print("Tabla de carreras")
TablaCarrera

#             ESTUDIO INDEPENDIENTE
# Tendencia central
colnames(df) [c(6)]=c("EstudioIndependiente")
sprintf("Promedio: %d", round(mean(df$EstudioIndependiente), 0))
# Medidas de dispersión
sprintf("Desviación: %f", sd(df$EstudioIndependiente))
sprintf("Varianza: %f", var(df$EstudioIndependiente))
limSup = mean(df$EstudioIndependiente) + sd(df$EstudioIndependiente)
limInf = mean(df$EstudioIndependiente) - sd(df$EstudioIndependiente)
rango = limSup-limInf
sprintf("Rango: %f", rango)
hist(df$EstudioIndependiente, xlab = "Horas", ylab = "frecuencia",
     main = "Histograma de Horas de estudio independiente", col = "cyan")
# Medidas de distrubución
sprintf("Coeficiente de variación: %f", sd(df$EstudioIndependiente)/mean(df$EstudioIndependiente))
sprintf("Coeficiente de asimetría: %f", skewness(df$EstudioIndependiente))
sprintf("Coeficiente de curtosis: %f", kurtosis(df$EstudioIndependiente))

#                  Dispositivos electrónicos
# Tendencia central
colnames(df) [c(7)]=c("Dispositivos")
sprintf("Moda: %s", mfv(df$Dispositivos))
# Tabla de frecuencias
TablaDispositivos=fdt_cat(df$Dispositivos,sort = T)
print("Tabla de Dispositivos")
TablaDispositivos

#             IMPORTANCIA DE LOS MEDIOS DIGITALES
# Tendencia central
colnames(df) [c(8)]=c("MediosDigitales")
sprintf("Moda: %s", mfv(df$MediosDigitales))
# Tabla de frecuencias
md=as.factor(df$MediosDigitales)
TablaMediosDigitales=fdt_cat(md,sort = T)
print("Tabla de importancia de los medios digitales")
TablaMediosDigitales

#                  HORAS DE SUEÑO
# Tendencia central
colnames(df) [c(9)]=c("Sueño")
sprintf("Promedio: %d", round(mean(df$Sueño), 0))
# Medidas de dispersión
sprintf("Desviación: %f", sd(df$Sueño))
sprintf("Varianza: %f", var(df$Sueño))
limSup = mean(df$Sueño) + sd(df$Sueño)
limInf = mean(df$Sueño) - sd(df$Sueño)
rango = limSup-limInf
sprintf("Rango: %f", rango)
hist(df$Sueño, xlab = "Horas de sueño", ylab = "frecuencia",
     main = "Histograma de horas de sueño promedio", col = "purple")
# Medidas de distrubución
sprintf("Coeficiente de variación: %f", sd(df$Sueño)/mean(df$Sueño))
sprintf("Coeficiente de asimetría: %f", skewness(df$Sueño))
sprintf("Coeficiente de curtosis: %f", kurtosis(df$Sueño))

#                  HORAS DE DISTRACCION
# Tendencia central
colnames(df) [c(10)]=c("Distraccion")
sprintf("Promedio: %d", round(mean(df$Distraccion), 0))
# Medidas de dispersión
sprintf("Desviación: %f", sd(df$Distraccion))
sprintf("Varianza: %f", var(df$Distraccion))
limSup = mean(df$Distraccion) + sd(df$Distraccion)
limInf = mean(df$Distraccion) - sd(df$Distraccion)
rango = limSup-limInf
sprintf("Rango: %f", rango)
hist(df$Distraccion, xlab = "Horas de distracción", ylab = "frecuencia",
     main = "Actividades diferentes al estudio", col = "red")
# Medidas de distrubución
sprintf("Coeficiente de variación: %f", sd(df$Distraccion)/mean(df$Distraccion))
sprintf("Coeficiente de asimetría: %f", skewness(df$Distraccion))
sprintf("Coeficiente de curtosis: %f", kurtosis(df$Distraccion))

#                  PROMEDIO PONDERADO ACUMULADO
# Tendencia central
sprintf("Promedio: %d", round(mean(df$promedio), 0))
# Medidas de dispersión
sprintf("Desviación: %f", sd(df$promedio))
sprintf("Varianza: %f", var(df$promedio))
limSup = mean(df$promedio) + sd(df$promedio)
limInf = mean(df$promedio) - sd(df$promedio)
rango = limSup-limInf
sprintf("Rango: %f", rango)
hist(df$promedio, xlab = "promedio", ylab = "frecuencia",
     main = "Promedio Ponderado Acumulado", col = "yellow")
# Medidas de distrubución
sprintf("Coeficiente de variación: %f", sd(df$promedio)/mean(df$promedio))
sprintf("Coeficiente de asimetría: %f", skewness(df$promedio))
sprintf("Coeficiente de curtosis: %f", kurtosis(df$promedio))

#             EL FACTOR AFECTA O NO EL RENDIMIENTO
# Tendencia central
colnames(df) [c(12)]=c("Rendimiento")
sprintf("Moda: %s", mfv(df$Rendimiento))
# Tabla de frecuencias
md=as.factor(df$Rendimiento)
TablaRendimiento=fdt_cat(md,sort = T)
print("Tabla de frecuencias rendimiento")
TablaRendimiento

#             FACTOR QUE AFECTE EL RENDIMIENTO
# Tendencia central
colnames(df) [c(13)]=c("Factores")
sprintf("Moda: %s", mfv(df$Factores))
# Tabla de frecuencias
md=as.factor(df$Factores)
TablaFactores=fdt_cat(md,sort = T)
print("Tabla de factores que afectan el rendimiento")
TablaFactores
        

# Cruce de las variables Sexo y Carrera (categorica vs categorica)
library(gmodels)

CrossTable(df$Sexo, df$Carrera, prop.r = T, prop.t = F,prop.c = F,
           expected = F, prop.chisq = F) # Fila

# Cruce de las variables Carrera y Dispositivos (categorica vs categorica)

CrossTable(df$Carrera, df$Dispositivos, prop.r = T, prop.t = F,prop.c = F,
           expected = F, prop.chisq = F) # Fila

# Cruce de las variables Carrera y Promedio (categorica vs Continua)

library(crosstable)
crosstable(df, c(Carrera, promedio), by=Carrera) %>%
  as_flextable(keep_id=TRUE)

library(DT)
kb  
