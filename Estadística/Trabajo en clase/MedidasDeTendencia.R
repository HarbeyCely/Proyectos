library(readr)
TallerHK <- read_delim("C:/Users/CENTIC/Escritorio/taller/TallerHK.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
library(modeest)
library(moments)
View(TallerHK)
attach(TallerHK)
names(TallerHK)
              # MEDIDAS DE TENDENCIA CENTRAL
# Promedio
promedio = mean(TallerHK$P5140, na.rm=T)
# Mediana
median(TallerHK$P5140, na.rm=T)


            # MEDIDAS DE TENDENCIA DISPERSION
# Coeficiente de variacion
sd(TallerHK$P5140, na.rm=T) / promedio
# Coeficiente de asimetria
skewness(TallerHK$P5140, na.rm=T)
# Coeficiente de curtosis
kurtosis(TallerHK$P5140, na.rm=T)