help("cars")

library(cars)
str(cars)
attach(cars)
names(cars)

# Coeficiente de asimetria
cv_speed = sd(cars$speed) / mean(cars$speed)
cv_distance = sd(cars$dist) / mean(cars$dist)  # varia mas la distancia, segun el CV
cars
hist(cars$dist, xlab = "Distancia", ylab = "frecuencia",
     main = "Histograma de velocidad", col = "gold")
hist(cars$speed, xlab = "Velocidad", ylab = "frecuencia",
     main = "Histograma de Distancia", col = "gold")

# Coeficiente de asimettria

library(moments)
skewness(cars$dist)  # skweness: sesgo, asimetria
skewness(cars$speed)

# Coeficiente de curtosis

kurtosis(speed)
kurtosis(dist)
