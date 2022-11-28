"
Desarrollo
Un banco recibe, en promedio, 6 cheques sin fondo por día.

set.seed(0202)
a) Grafica la función de distribución de la variable aleatoria. (Asume que se obtienen 10,000 muestras)

b) ¿Cuál es la probabilidad de que reciba 4 cheques sin fondo en un dia

c) ¿Cuál es la probabilidad de que reciba más de 8 cheques sin fondo?
  
d) ¿Cuál es la probabilidad de que reciba entre 4 y 10 cheques sin fondo?
  
e) ¿Cuál es la probabilidad de que tengan que pasar 5 horas o menos hasta que se presente el siguiente 
   cheque sin fondos?
  
f) ¿Cuál es la probabilidad de que tengan que pasar entre 2 y 4 hasta que se presente el siguiente cheque 
   sin fondos?
  
g) Realiza la gráfica de distribución de probabilidad de la variable aleatoria anterior
"

set.seed(0202)

# a)
poisson <- rpois(n = 10000, lambda = 6)
barplot(table(poisson), length(poisson),
        main = 'Distribucion de Poisson',
        xlab = 'X = x')

# b)
p <- dpois(x = 4, lambda = 6)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# c) 
p <- ppois(q = 8, lambda = 6, lower.tail = FALSE)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# d) 
p <- ppois(q = 10, lambda = 6) - ppois(q = 4, lambda = 10)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# e)

rate.exp <- 6 / 24
rate.exp

p <- pexp(q = 5, rate = rate.exp)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# f)
p <- pexp(q = 4, rate = rate.exp) - pexp(q = 2, rate = rate.exp)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p)

# g)
curve(dexp(x, rate = rate.exp), from = 0, to = 20, 
      col='blue', main = "Distribución exponencial",
      ylab = "f(x)", xlab = "Tiempo entre eventos")

# h)

expon <- rexp(n = 10000, rate = rate.exp)
mean(expon)
sd(expon)
