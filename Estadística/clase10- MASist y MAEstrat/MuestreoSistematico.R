###  Muestreo Sistematico  ###
library(TeachingSampling)
data(Lucy)
set.seed(1)

a = 2396/100       # a = N/n
a = round(a)
muestra = S.SY(2396, a)
Lucy[muestra]

###  Muestreo estratificado  ###
table(Lucy$Zone)
Nh=c(307, 727, 974, 223, 165)
sum(307, 727, 974, 223, 165)
nh= (Nh/2396)*100
#(307/2396)*100
nh= round(nh)
muestra2=S.STSI(Lucy$Zone, Nh, nh)
m2=Lucy$Zone[muestra2]
table(m2)
