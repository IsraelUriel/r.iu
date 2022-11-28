"
1.- El tiempo necesario para producir un determinado producto en una maquinaria 
tiene una distribución normal cuya media es 80 minutos con desviación estándar de 10 minutos.

set.seed(0202)

a) Grafica la función de distribución de la variable aleatoria.

b) ¿Cuál es la probabilidad de que la maquinaria termine el producto en una hora o menos?
  
c) ¿Cuál es la probabilidad de que el producto sea terminado en más de 1.5 horas, pero en menos 
   de 2.5 horas?
  
d) ¿Cuál es el intervalo de tiempo que deja exactamente al centro el 90% de probabilidad?
  

2.- Una institución de crédito informa a las autoridades que, en promedio, sus clientes mantienen 
    un saldo deudor en sus tarjetas de crédito igual a 12,500 pesos mensuales, con una desviación 
    estándar de 7,800 pesos.

a) ¿Cuál es la probabilidad de que un cliente tenga un saldo deudor mayor a $20,000?
  
b) ¿Cuál es la probabilidad de que un cliente tenga un saldo deudor menor a $11,000?
  
c) ¿Cuál es la probabilidad de que un cliente tenga un saldo deudor entre $13,000 y 15,000?
  
d) ¿Hasta qué monto se encuentra el 10% de clientes con saldo deudor más bajo?
"

set.seed(0202)

# 1:
# a)
m <- 80
std <- 10
  
curve(dnorm(x, mean = m, sd = std), from = 40, to = 120, 
      col='blue', main = "Densidad Normal:\nDiferente media",
      ylab = "f(x)", xlab = "X")
abline(v = m, lwd = 0.5, lty = 2)

# b)
p <- pnorm(q = 60, mean = m, sd = std)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# c)
p <- pnorm(150, m, std) - pnorm(90, m, std)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# d)
qnorm(p = 0.1/2, mean = m, sd = std); 
qnorm(p = 0.1/2, mean = m, sd = std, lower.tail = FALSE)

# 2:
# a)
m = 1250
dev.std = 7800

p <- pnorm(q = 20000, m, dev.std, lower.tail = FALSE)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# b)

p <- pnorm(q = 11000, m, dev.std)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)


# c)
p <- pnorm(15000, m, dev.std) - pnorm(13000, m, dev.std)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# d)
qnorm(p = 0.10, m, dev.std)


