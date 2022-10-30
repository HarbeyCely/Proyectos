library(readr)
Educacion <- read_delim("S:/Videos/ing. economica/Educacion.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

attach(Educacion)
names(Educacion)
library(fdth)
Educacion$P6210=as.factor(Educacion$P6210)

str(Educacion$P6210)
## Educacion=factor(Educacion$P6210, levels=c("2", "1", "3", "5", "6"))
tabla2=fdt_cat(Educacion$P6210,sort=FALSE)

library(crayon)
cat(cyan$bgMagenta$bold("Tabla de frecuencias categorica Ordinal"))

tabla2
