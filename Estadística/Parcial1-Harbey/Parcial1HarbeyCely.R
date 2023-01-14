library(readxl)
Datos <- read_excel("Datos.xlsx", sheet = "Variables", 
                    col_types = c("numeric", "text", "text", 
                                  "text", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric"))
attach(Datos)
names(Datos)

#PUNTO1
limiteSuperior = mean(Deficit_cuanti) + 2*sd(Deficit_cuanti)
limiteInferior = mean(Deficit_cuanti) - 2*sd(Deficit_cuanti)  
(limiteSuperior-limiteInferior)/1101      

limiteSuperior2 = mean(Deficit_cuanti) + 3*sd(Deficit_cuanti)
limiteInferior2 = mean(Deficit_cuanti) - 3*sd(Deficit_cuanti)  
(limiteSuperior2-limiteInferior2)/1101
hist(Deficit_cuanti, col = "yellow")
#PUNTO2
library(crosstable)
class(Datos$AseguraSaludIPM)
class(Datos$CobAlcantarillado)
class(Datos$CobEnergia)

crosstable(Datos, c(AseguraSaludIPM, CobAlcantarillado, CobEnergia), by=ZONA) %>%
  as_flextable(keep_id=TRUE)
#PUNTO3
library(ggplot2)
ggplot(Datos, aes(x = Deficit_cuanti, y = Deficit_cuali)) + 
  geom_point(color = "orange", size = 3, shape = 3)
ggplot(Datos, aes(x = Deficit_cuali, y = Deficit_cuanti)) + 
  geom_point(color = "blue", size = 3, shape = 3)
cor(Deficit_cuanti,Deficit_cuali)