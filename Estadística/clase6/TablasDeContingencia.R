library(readxl)
df <- read_excel("C:/Users/CENTIC/Escritorio/clase6/DF.xlsx", 
                 sheet = "Hoja1")
attach(df)
names(df)
colnames(df)= c("Sexo", "Cerveza")    # C sirve para declarar un vector
attach(df)
names(df)
## tabla de contingencia sin paquete
t1=table(sexo, cerveza)
t1
prop.table(t1,1) # relativa fila
prop.table(t1,2) # relativa Columna

library(gmodels)
CrossTable(cerveza, sexo, prop.c = F, prop.t = F,
           expected = F, prop.chisq = F)  # Contingencia relativa fila

CrossTable(cerveza, sexo, prop.r = F, prop.t = F,
           expected = F, prop.chisq = F)  # Contingencia relativa Columna
