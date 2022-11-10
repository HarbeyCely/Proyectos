# Lectura de datos
df = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTBZBdQIo7gLUdnLdhZZTtoiIQafo8w4eochKMFH9sRNr0X9y3_jjUhvsLZTnm1_Sm2yvP_6GYjkZyz/pub?gid=1901692048&single=true&output=csv")
View(df)
attach(df)
names(df)
colnames(df)=c("tiempo","edad","papa")
attach(df)

# Estadistica descriptiva
limiteSuperior = mean(papa) + sd(papa)  #sd -> standard derivation
limiteInferior = mean(papa) - sd(papa)   #mean() -> promedio
nrow(df[ df$papa<4.26, ])
nrow(df[ df$papa<3.33, ])
24/30                                   #   (ls - li ) / n

limiteSuperior2 = mean(papa) + 2*sd(papa)
limiteInferior2 = mean(papa) - 2*sd(papa)
nrow(df[ df$papa<4.72, ])
nrow(df[ df$papa<2.87, ])

27/30

limiteSuperior3 = mean(papa) + 3*sd(papa)
limiteInferior3 = mean(papa) - 3*sd(papa)
nrow(df[ df$papa<5.18, ])
nrow(df[ df$papa<2.41, ])

27/30
#promdio recortado
mean(papa, trim = 0.05)    # 5 %
mean(papa, trim = 0.10)    # 10 %

median(edad)

library(modeest)
mlv(edad)



help(crayon)

library(crayon)
cat(yellow$bold("Holi"),magenta$bold("Karo"))
