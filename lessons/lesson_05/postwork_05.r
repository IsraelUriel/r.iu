
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
View(iris)

# Nivel de Confianza: 99% -> 0.01
nc <- 0.01

# A)  ----------------------------------------------------------------------

# En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm

x <- iris$Sepal.Length

# Hipótesis :
# Ho: mu = 5.7
# Ha: mu != 5.7
mu <- 5.7

p.value <- t.test(x, alternative = 'two.sided', mu = mu)$p.value
t.mean <- t.test(x, alternative = 'two.sided', mu = mu)$estimate


if (p.value >= nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '>=', nc)
} else {
  cat('R: Se Rechaza: Ho', p.value, '<', nc)
} 

cat('Diferencia de medias:', 't.mean:', t.mean, '|', 'mu:', mu)

# A nivel de confianza estándar de 99%, no existe evidencia estadística 
# para rechazar Ho, es decir El promedio no es distinto a 5.7.


# B) ----------------------------------------------------------------------

# En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm

x <- iris$Petal.Width

# Hipótesis:
# Ho: mu >= 2.1
# Ha: mu < 2.1
mu <- 2.1

p.value <- t.test(x = x, alternative = 'less', mu = mu)$p.value
t.mean <- t.test(x = x, alternative = 'less', mu = mu)$estimate


if (p.value >= nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '>=', nc)
} else {
  cat('R: Se Rechaza: Ho', p.value, '<', nc)
} 

cat('Diferencia de medias:', 't.mean:', t.mean, '|', 'mu:',mu)

# A nivel de confianza estándar de 99%, existe evidencia estadística 
# para rechazar Ho, es decir El promedio es menor distinto a 2.1.


# C) ----------------------------------------------------------------------

# En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el promedio 
# del largo del pétalo de la especie versicolor.

x <- unlist(iris[iris$Species == 'virginica', 'Petal.Length'], use.names = FALSE) 
y <- unlist(iris[iris$Species == 'versicolor', 'Petal.Length'], use.names = FALSE)

# Hipótesis:
# Ho: mu <= 1.1
# Ha: mu > 1.1
mu <- 1.1

var.test(x, y, alternative = 'greater', ratio = 1.1)

p.value <- t.test(x, y, alternative = 'greater', mu = 0, var.equal = FALSE)$p.value
t.mean <- t.test(x, y, alternative = 'greater', mu = 0, var.equal = FALSE)$estimate

if (p.value >= nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '>=', nc)
} else {
  cat('R: Se Rechaza: Ho', p.value, '<', nc)
} 

cat('Diferencia de medias:', 't.mean:', t.mean, '|', 'mu:',mu)

# A nivel de confianza estándar de 99%, existe evidencia estadística 
# para rechazar Ho, es decir El promedio es mayor que 1.1.

# D) ----------------------------------------------------------------------

# En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.

# Hipótesis:
# mu0 = mu1 = mu2
# mu0 != mu1 != mu2

boxplot(iris$Sepal.Width ~ factor(iris$Species),
        xlab = "Species", ylab = "Width")

x <- unlist(iris[iris$Species == 'virginica', 'Sepal.Width'], use.names = FALSE) 
y <- unlist(iris[iris$Species == 'versicolor', 'Sepal.Width'], use.names = FALSE)
z <- unlist(iris[iris$Species == 'setosa', 'Sepal.Width'], use.names = FALSE)

samples <- data.frame(y = c(x,y,z),
  group = c('virginica', 'versicolor', 'setosa'))

m <- lm(y ~ group, data = samples)
anova(m)
summary(m)
p.value <- anova(m)$'Pr(>F)'[1]

if (p.value >= nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '>=', nc)
} else {
  cat('R: Se Rechaza: Ho', p.value, '<', nc)
} 

par(mfrow=c(2, 2))
plot(m)

# A nivel de confianza estándar de 99%, existe evidencia estadística 
# para No rechazar Ho, es decir El promedio es igual entre las 3 Especies.
