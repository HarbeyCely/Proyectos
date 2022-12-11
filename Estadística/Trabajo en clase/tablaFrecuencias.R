library(readr)
TallerHK <- read_delim("C:/Users/CENTIC/Escritorio/taller/TallerHK.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(TallerHK)
attach(TallerHK)
names(TallerHK)

library(fdth)
TallerHK$P5020=as.factor(TallerHK$P5020)

str(TallerHK$P5020)

tabla=fdt_cat(TallerHK$P5020,sort=FALSE)
tabla