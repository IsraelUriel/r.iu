"
RETO 01: TABLAS DE DISTRIBUCIÓN DE FRECUENCIAS
Objetivo
Utilizar tablas de distribución de frecuencias para analizar variables

Para este reto vamos a hacer uso del dataframe diamonds, disponible en la librería de ggplot2. 
Realiza los siguientes incisos y responde a las preguntas:
  
  ¿Qué tipo de variable y escala de medición tiene la variable cut?
  
  Reliza una tabla de frecuencias absolutas y relativas

Para esta variable, ¿es posible calcular la frecuencia relativa acumulada? En caso afirmativo, 
agrégala a tu tabla anterior

Con base en tu tabla, responde:
  
  ¿Cuál es el porcentaje de diamantes que tienen un corte Very Good?
  ¿Cuál es el porcentaje de diamantes que tienen un corte entre Fair y Very Good?
  ¿Cuál es el porcentaje de diamantas que tienen un corte al menos Very Good?
  Crea una tabla de distribución de frecuencias para el precio de los diamantes. Para ello 
  determina el número de clases con base en la regla de Sturges k = 1+3.3Log10(n)

Con base en tu tabla, responde:
  
  ¿Cuál es el porcentaje de diamantes que tienen un precio entre 3590.17 y 4678.23?
  ¿Cuál es el porcentaje de diamantes que tienen un precio menor a 7942.41?
  ¿Cuál es el porcentaje de diamantes que tienen un precio mayor a 11206.58?
"

library(ggplot2)
library(dplyr)

# Analizar BD
str(diamonds)

# Analizar atributo CUT
class(diamonds$cut) # Ordered Factor
typeof(diamonds$cut) # Integer
attributes(diamonds$cut) # Fair, GOod, Very Good, Premium, Ideal


# Tabla de Frecuencias Relativa
freq <- table(diamonds$cut) 

table.freq <- transform(freq, rel = prop.table(Freq))

# Frecuencia Relativa Acumulada
table.freq.acum <- transform(freq, cumsum(prop.table(Freq)))

#Agregarla a la tabla anterior
diamond.freqs <- transform(freq, 
                           Relativa = table.freq,
                           Acumulada = table.freq.acum)


# Preguntas:
diamond.freqs %>%
  filter(Relativa.Var1 == 'Very Good') %>%
  summarise(round(sum(Relativa.rel) * 100, 2))
# Very Good % = 22.4 %

diamond.freqs %>%
  filter(Relativa.Var1 %in% c('Fair','Good','Very Good')) %>%
  summarise(round(sum(Relativa.rel) * 100, 2))
# Very Good % = 34.48 %

diamond.freqs %>%
  filter(Relativa.Var1 %in% c('Very Good','Premium','Ideal')) %>%
  summarise(round(sum(Relativa.rel) * 100, 2))
# Very Good % = 87.92 %


# Determinar Número de Clases

# Formula Sturges
Sturges = ceiling(1 + 3.3 * log10(length(diamonds$price)))

# Ancho Columna
width = (max(diamonds$price) - min(diamonds$price)) / Sturges

# Numero de clases
bins <- seq(min(diamonds$price), max(diamonds$price), by = width)

diamonds.price <- cut(diamonds$price, breaks = bins, include.lowest = TRUE, dig.lab = 10)

# Tabla de Frecuencia
dist.freq <- table(diamonds.price)
diamond.freqs <-transform(dist.freq, 
                       Relativa = prop.table(Freq), 
                       Acumulada = cumsum(prop.table(Freq)))

# Preguntas

diamond.freqs %>%
  summarise(round(Relativa[4] * 100, 2))
# precio entre 3590.17 y 4678.23 : 9.31 %

diamond.freqs %>%
  summarise(round(Acumulada[7] * 100, 2))
# precio menor a 7942.41 : 85.75 %

diamond.freqs %>%
  summarise(round(sum(Relativa[11:17]) * 100 ,2))
# precio mayor a 11206.58 : 7.54 %












