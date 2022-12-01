
# Instructions: -----------------------------------------------------------


#Reto 01: Regresión Lineal y predicción

"
Una aseguradora de automóviles sueca está interesada en un modelo predictivo que le permite 
establecer el pago que sus clientes deben hacer por el seguro (Payment), explicado por el número 
de casos (Claims) y el número de asegurados (Insured).

Tu trabajo es determinar el mejor modelo de predicción

A) Selecciona solo las variables de interés y realiza una matriz de correlaciones.

B) Estima un modelo de regresión lineal de acuerdo con lo solicitado por la aseguradora. No olvides 
   interpretar tus resultados y realizar el diagnóstico sobre los residuos.

C) Con el primer modelo, estima uno nuevo quitando la variable Insured. No olvides interpretar tus 
   resultados y realizar el diagnóstico sobre los residuos.

D) Con el primer modelo, estima uno nuevo quitando la variable Claims. No olvides interpretar tus 
   resultados y realizar el diagnóstico sobre los residuos.

E) ¿Cuál de los 3 modelos tiene un mejor poder predictivo?
"


# Solution: ---------------------------------------------------------------

library('dplyr')
library('ggplot2')

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/SwedishMotorInsurance.csv", header = TRUE)

head(df, 10)
str(df)

nc <- 0.05

# A) ----------------------------------------------------------------------

# Selecciona solo las variables de interés y realiza una matriz de correlaciones.

sample <- select(df, Payment, Insured, Claims) # Seleccion de variables
round(cor(sample),4) # Creacion de matriz de relaciones

x <- sample$Payment
y <- sample$Insured
z <- sample$Claims
  
cor.test(x, y)
plot(x, y)

# B) ----------------------------------------------------------------------

# Estima un modelo de regresión lineal de acuerdo con lo solicitado por la aseguradora. 
# No olvides interpretar tus resultados y realizar el diagnóstico sobre los residuos.

pairs(~ Payment + Insured + Claims, 
      data = df, gap = 0.4, cex.labels = 1.5)

# Se visualiza una relación positiva entre Payments y Claims
attach(df)
m1 <- lm(Payment ~ Insured + Claims)

summary(m1)
# Hipotesis
# Ho: beta_i = 0
# Ha: beta_i != 0

# La R2 ajustada es 0.9951, que 
# indica un buen ajuste del modelo (próximo a 1).

# C) ----------------------------------------------------------------------

# Con el primero modelo, estima uno nuevo quitando la variable Insured. No olvides 
# interpretar tus resultados y realizar el diagnóstico sobre los residuos.

m2 <- update(m1, Payment ~.- Insured) # Actualizar Modelo

summary(m2)

cor.test(x, z)
plot(x, z)


# Hipotesis
# Ho: beta_i = 0
# Ha: beta_i != 0

# La R2 ajustada es 0.9908, que 
# indica un buen ajuste del modelo (próximo a 1).

plot(x ~ z,
     xlab = "Payment", 
     ylab = "Claims" )
abline(m2, col = "#e74c3c" ) # añadir la recta


# D) ----------------------------------------------------------------------

# Con el primer modelo, estima uno nuevo quitando la variable Claims. No olvides 
# interpretar tus resultados y realizar el diagnóstico sobre los residuos.

m3 <- update(m1, ~.- Claims)
summary(m3)


plot(x ~ y,
     xlab = "Payment", 
     ylab = "Insured" )
abline(m3, col = "#e74c3c" ) # añadir la recta


# D) ----------------------------------------------------------------------

m1.r = summary(m1)$adj.r.squared
m2.r = summary(m2)$adj.r.squared
m3.r = summary(m3)$adj.r.squared

sample.r2 <- rbind(df, data.frame(x = m1.r, y = toString(i)))
