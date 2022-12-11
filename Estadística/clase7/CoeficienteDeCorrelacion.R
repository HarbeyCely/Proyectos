# coeficiente de correlacion
library(readxl)
df <- read_excel("C:/Users/CENTIC/Escritorio/clase7/VariablesContinuas.xlsx", 
                                 sheet = "Hoja1")
attach(df)
names(df)
colnames(df) = c("peso", "altura")
attach(df)
names(df)

library(ggplot2)
ggplot(df, aes(x = peso, y = altura)) + geom_point(color = "red",
                                                   size = 5, shape = 3)
# con geom_point() se escojen las caracteristicas de la grafica


## calcular coeficiente de correlacion

cor(peso,altura) # cor(x, y), No importa el orden