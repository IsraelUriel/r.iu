
# Instructions: -----------------------------------------------------------
"
Un modelo autorregresivo es aquel donde el comportamiento en el tiempo de una serie es explicado por el primer rezago de su comportamiento, es decir: y[t]=b*y[t-1] + w[t], donde b es el coeficiente del término autorregresivo y w es una serie ruído blanco.

Coloca el número diez como semilla

A) Simula un proceso AR(1) de la forma x[t] = 0.8 * x[t-1] + w[t] para t = 1, 2, ..., 200 y muestra gráficamente la serie de tiempo obtenida

B) Obtén el correlograma y el correlograma parcial del proceso AR(1) simulado

C)Ajusta un modelo autorregresivo a la serie simulada utilizando la función ar, observa el orden del modelo y los parámetros estimados. ¿Coinciden con el modelo original?
"


# Solution: ---------------------------------------------------------------

set.seed(10)


# A) ----------------------------------------------------------------------

# A) Simula un proceso AR(1) de la forma x[t] = 0.8 * x[t-1] + w[t] para t = 1, 2, ..., 200 
# y muestra gráficamente la serie de tiempo obtenida

x <- rnorm(200)
for(i in 2:200) x[i] <- 0.8*x[i-1] + w[i]

plot(x, type = "l", 
     main = "Serie simulada de un modelo ARIMA(1, 1, 1)",
     xlab = "Tiempo",
     ylab = expression(x[t]),
     sub = expression(x[t] == 0.5*x[t-1] + x[t-1] - 0.5*x[t-2] + w[t] + 0.3*w[t-1]))


# B) ----------------------------------------------------------------------

# Obtén el correlograma y el correlograma parcial del proceso AR(1) simulado

acf(x)
pacf(x)

# C) ----------------------------------------------------------------------

# Ajusta un modelo autorregresivo a la serie simulada utilizando la función ar, 
# observa el orden del modelo y los parámetros estimados. ¿Coinciden con el modelo original?

ar(x, method = 'mle')



