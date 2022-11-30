
# Instructions: -----------------------------------------------------------

"
Utilizando la base de datos de mpg (Fuel Economy Data) disponible en la librería de ggplot2, 
realiza los siguientes ejercicios indicando el juego de hipótesis y concluyendo de forma estadística 
a un nivel de confianza del 95%

library(ggplot2)
A) Con base en los datos, existe evidencia estadística para concluir que, en promedio, los coches 
producidos entre 1999 y 2008 podían recorrer más de 22.8 millas de carretera por galón (hwy)?
  
B) Con base en los datos, existe evidencia estadística para concluir que, en promedio, el desplazamiento 
del motor en litros (displ) para los coches producidos entre 1999 y 2008 era mayor o igual 3.7 litros?
  
C) Con base en los datos, existe evidencia estadística para concluir que, en promedio, los motores con 4 
cilindros (cyl = 4) tienen un mayor rendimiento en millas de carretera por galón (hwy) que los motores 
con 6 cilindros (cyl = 6)
"


# Solution: ---------------------------------------------------------------


library('dplyr')
library('ggplot2')

summary(mpg)
str(mpg)
glimpse(mpg)

# Nivel de Confianza: 95% -> 0.05
nc <- 0.05

# A) ----------------------------------------------------------------------

# Con base en los datos, existe evidencia estadística para concluir que, en promedio, los coches 
# producidos entre 1999 y 2008 podían recorrer más de 22.8 millas de carretera por galón (hwy)?

x <- mpg$hwy

#Hipótesis:
# Ho: mu <= 22.8
# Ha: mu > 22.8

mu <- 22.8

p.value <- t.test(x = x, alternative = 'greater', mu = mu)$p.value

if (p.value >= nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '>=', nc)
} else {
  cat('R: Se Rechaza Ho:', p.value, '<', nc)
} 

  
# B) ----------------------------------------------------------------------

# Con base en los datos, existe evidencia estadística para concluir que, en promedio, el desplazamiento 
# del motor en litros (displ) para los coches producidos entre 1999 y 2008 era mayor o igual 3.7 litros?

x <- mpg$displ

#Hipótesis:
# Ho: mu >= 3.7
# Ha: mu < 3.7

mu <- 3.7

p.value <- t.test(x = x, alternative = 'greater', mu = mu)$p.value

if (p.value >= nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '>=', nc)
} else {
  cat('R: Se Rechaza: Ho', p.value, '<', nc)
} 

# C) ----------------------------------------------------------------------

# Con base en los datos, existe evidencia estadística para concluir que, en promedio, los motores con 4 
# cilindros (cyl = 4) tienen un mayor rendimiento en millas de carretera por galón (hwy) que los motores 
# con 6 cilindros (cyl = 6)

#Hipótesis:
# Ho: (cyl = 4) <= (cyl = 6)
# Ha: (cyl = 4) > (cyl = 6)

x <- unlist(mpg[mpg$cyl == 4, 'hwy'], use.name = FALSE)
y <- unlist(mpg[mpg$cyl == 6, 'hwy'], use.name = FALSE)
var.test(x, y, alternative = 'greater', ratio = 1)

p.value <- t.test(x, y, alternative = 'greater', mu = 0, var.equal = TRUE)$p.value

if (p.value >= nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '>=', nc)
} else {
  cat('R: Se Rechaza: Ho', p.value, '<', nc)
} 


