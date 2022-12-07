
# Instructions: -----------------------------------------------------------
"
Utilizando el siguiente vector numérico, realiza lo que se indica:
  
A) Crea una objeto de serie de tiempo con los datos de Global. La serie debe ser mensual comenzado en Enero de 1856
  
B) Realiza una gráfica de la serie de tiempo anterior de 2005)

C) Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia:

D) ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?

E) Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial?
"


# Solution: ---------------------------------------------------------------

url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
global <- scan(url, sep="")
str(global)

# A) ----------------------------------------------------------------------

global.ts <- ts(global, start = c(1856,1), frequency = 12) 
class(global.ts)


# B) ----------------------------------------------------------------------

plot(global.ts, 
     main = "Global", 
     xlab = "Tiempo",
     sub = "Enero de 1856 - Diciembre de 2004")
plot(decompose(global.ts))


cycle(global.ts)
summary(global.ts)

# C) ----------------------------------------------------------------------

plot(diff(global.ts), type = "l", main = "Primera diferencia de Global", 
     xlab = "t", ylab = expression(x[t]), 
     sub = expression(x[t]==x[t-1]+w[t]))


plot(decompose(diff(global.ts)))
summary(diff(global.ts))


# D) ----------------------------------------------------------------------

# Si son estacionarias las dos

global.1900 <- ts(global, start = c(1900,1), end = c(1910,1), frequency = 12)

plot(decompose(global.1900))
plot(decompose(diff(global.1900)))

# E) ----------------------------------------------------------------------


acf(diff(global))
pacf(diff(global))
