library(readxl)
Mm <- read_excel("C:/Users/hecm-/Desktop/Muestreo.xlsx", 
                       sheet = "Hoja1")
View(Mm)
attach(Mm)

### Muestreo Aleatorio Simple (MAS) ###
library(TeachingSampling)
n=10
muestra=S.SI(nrow(Mm), n) # S.SI --->  MAS sin reemplazo
Mm[muestra,]

# Otra forma con libreria base
library(base)
muestra2=sample(Mm$ID,size = 10) # replace=F
Mm[muestra2,]

# Otra forma x2
library(sampling)
library(dplyr)
muestra3 = slice_sample(Mm, n=10, replace = F)
muestra3

# Numeros random del 0 al 100
sample(0:100, replace = T, size=10)

