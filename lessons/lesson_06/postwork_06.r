
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
library('ggplot2')
library('ggthemes')

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

# Mediana proxima a 0 y R2 ajustada de 0.8112 cercana a 1
# indica buen ajuste del modelo

# Plot m2
sample %>%
  ggplot(aes(x = x1, y = y)) +
  geom_point(aes(color = resid(m2)),
             alpha = 0.25,
             show.legend = FALSE) +
  geom_abline(intercept = m2$coeff[1],
              slope = m2$coeff[2],
              color = '#9A35CD',
              alpha = 0.5,
              lwd = 1) +

  labs(x = 'Sales', y = 'TV') +
  theme_tufte() +
  geom_rangeframe() + 
  theme(panel.grid = element_line(color = '#9FDCE4',
                                        size = .25,
                                        linetype = 2),
        plot.margin = margin(1,1,1,1, 'cm'))

# M3
m3 = update(m1, Sales ~.- TV - Newspaper)
summary(m3)

# Mediana proxima a lejana a 0 y R2 ajustada de 0.1178 cercana a 0
# indica No muy buen ajuste del modelo

sample %>%
  ggplot(aes(x = x2, y = y)) +
  geom_point(aes(color = resid(m3)),
             alpha = 0.25,
             show.legend = FALSE) +
  geom_abline(intercept = m3$coeff[1],
              slope = m3$coeff[2],
              color = '#9A35CD',
              alpha = 0.5,
              lwd = 1) +
  
  labs(x = 'Sales', y = 'Radio') +
  theme_tufte() +
  geom_rangeframe() + 
  theme(panel.grid = element_line(color = '#9FDCE4',
                                  size = .25,
                                  linetype = 2),
        plot.margin = margin(1,1,1,1, 'cm'))

# rstandard: residuales estandarizados

StanRes2 <- rstandard(m1)

par(mfrow = c(2, 2))

plot(TV, StanRes2, ylab = "Residuales Estandarizados")
plot(Radio, StanRes2, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes2, ylab = "Residuales Estandarizados")


# Gráfica de quantiles, para ver si la distribucion se acerca a una normal teorica

qqnorm(StanRes2)
qqline(StanRes2)

dev.off()

p.value <- shapiro.test(StanRes2)$p.value

# Hipotesis

# Ho: La variable distribuye como una normal
# Ha: La variable no distribuye como una normal
# Sig = 0.05

if (p.value < nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '<', nc)
} 

# El modelo tiende a una distribución Normal
# El supuesto se aprueba

# Predicción

prediction.data <- data.frame(
  TV = c(151.5, 180.8, 18.7, 170.5, 240.9)
)

# a un nivel de confianza de 0.95
# Fit: Pronostica las Ventas

prediction.data <- predict(m2, newdata = prediction.data, interval = "confidence", level = 0.95)

# Conclusion
# De acuerdo con el modelo de Predicción; si se aumenta la inversión de propaganda
# por la TV, se aumenta las unidades de venta entre 16.65 a 17.34 unidades 

prediction_m <- cbind(sample, prediction.data)

prediction_m %>%
  ggplot(aes(x = x1, y = y)) +
  geom_point(aes(color = resid(m2)),
             alpha = 0.25,
             show.legend = FALSE) +
  geom_abline(intercept = m2$coeff[1],
              slope = m2$coeff[2],
              color = '#9A35CD',
              alpha = 0.5,
              lwd = 1) +
  
  labs(x = 'Sales', y = 'TV') +
  theme_tufte() +
  geom_rangeframe() + 
  theme(panel.grid = element_line(color = '#9FDCE4',
                                  size = .25,
                                  linetype = 2),
        plot.margin = margin(1,1,1,1, 'cm')) +
  geom_smooth(method = lm, se = TRUE, level = 0.95)
