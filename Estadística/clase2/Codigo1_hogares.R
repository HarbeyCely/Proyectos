library(readxl)
hogares <- read_excel("S:/Videos/ing. economica/hogares.xlsx", 
                      sheet = "AREA")

attach(hogares)
names(hogares)

library(fdth)
hogares$P4000=as.factor(hogares$P4000)
tabla1=fdt_cat(hogares$P4000,sort=FALSE)

library(crayon)
cat(cyan$bgMagenta$bold("Tabla de frecuencias categorica Nominal"))


tabla1



