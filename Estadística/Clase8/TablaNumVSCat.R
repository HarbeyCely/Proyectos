library(readxl)
df <- read_excel("C:/Users/CENTIC/Escritorio/Clase8/Datos.xlsx", 
                    col_types = c("numeric", "numeric", "numeric", 
                                  "text", "numeric", "numeric"))

attach(df)
names(df)

colnames(df) [c(1,2,3,4,5,6)]=c("ID", "Horas","Edad", "Sexo", "Q","P")
                                          #   Q: Cantidad
                                          #   P: Precio
library(crosstable)

class(df$Sexo)

class(df$Q)

crosstable(df, c(Q, P ), by=Sexo) %>%
  as_flextable(keep_id=TRUE)

crosstable(df, c(Edad), by=c(ID,Sexo)) %>%
  as_flextable(keep_id=TRUE)
