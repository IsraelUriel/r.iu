
# Instructions: -----------------------------------------------------------

"
Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como mejorar las ventas de un producto particular, y el conjunto de datos con el que disponemos son datos de publicidad que consisten en las ventas de aquel producto en 200 diferentes mercados, junto con presupuestos de publicidad para el producto en cada uno de aquellos mercados para tres medios de comunicación diferentes: TV, radio, y periódico. No es posible para nuestro cliente incrementar directamente las ventas del producto. Por otro lado, ellos pueden controlar el gasto en publicidad para cada uno de los tres medios de comunicación. Por lo tanto, si determinamos que hay una asociación entre publicidad y ventas, entonces podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad, y así indirectamente incrementar las ventas.

En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda ser usado para predecir las ventas sobre la base de los tres presupuestos de medios de comunicación. Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y elija el modelo más adecuado siguiendo los procedimientos vistos

Considera:

Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)
"


# Solution: ---------------------------------------------------------------

library('dplyr')

df <- read.csv('advertising.csv', header = TRUE)
str(df)
glimpse(df)

# Data Clean Up
# Selecting Variables
sample <- select(df, Sales, TV, Radio, Newspaper)

# Using Correlation

# Matrix Correlation
round(cor(sample),4) # -TV- var presenta el valor mas cercano a 1

pairs(~ Sales + TV + Radio + Newspaper,
      data = sample, gap = 0.4, cex.labels = 1.5)
# La gráfica muesta que la relación Sales con TV presenta correlacion positiva 

# Model
attach(sample)
m1 <- lm(Sales ~ TV + Radio + Newspaper)
summary(m1)

# Mediana cercana a 1 = Modelo confiable
# R2 Ajustada cercana a 1 = Indica buen ajuste del Modelo

nc <- 0.05 # Usando nivel de confianza = 0.05


# Get Sig Variables < sig
sig.m1 <- summary(m1)$coeff[-1,4] < nc
names(sig.m1)[sig.m1 == TRUE]
# Variables TV y Radio favorables al nivel de confianza

# Model Relations for selected sigs

y <- sample$Sales
x1 <- sample$TV
x2 <- sample$Radio

# M2: Sales ~ TV
m2 <- update(m1, Sales ~.- Radio - Newspaper)
summary(m2)
