
# Instructions: ----------------------------------------------------------------

"
El data frame iris contiene información recolectada por Anderson sobre 50 flores de 
3 especies distintas (setosa, versicolor y virginca), incluyendo medidas en centímetros 
del largo y ancho del sépalo así como de los pétalos.

Estudios recientes sobre las mismas especies muestran que:
  
En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm
En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm
En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el promedio 
del largo del pétalo de la especie versicolor.
En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.
Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente para 
concluir que los datos recolectados por Anderson están en línea con los nuevos estudios.

Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el planteamiento de hipótesis adecuado y concluye.
"

# Solution: ----------------------------------------------------------------

library('dplyr')
library('ggplot2')

summary(iris)
str(iris)
glimpse(iris)

# Nivel de Confianza: 99% -> 0.01
nc <- 0.01

# A)  ----------------------------------------------------------------------

# En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm

x <- iris$Sepal.Length

# Hipótesis :
# Ho: mu = 5.7
# Ha: mu != 5.7
mu <- 5.7

t.test(x, alternative = 'two.sided', mu)


p.value <- t.test(x, alternative = 'greater', mu)$p_value
