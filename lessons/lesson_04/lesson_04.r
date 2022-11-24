############ SESIÓN 4: PROBABILIDAD Y FUNCIONES DE DISTRIBUCIÓN  ##############
###############################################################################
set.seed(2022)
​
## EJEMPLO 01: ENSAYO DE BERNOULLI Y DISTRIBUCIÓN BINOMIAL
"Un ensayo de Bernoulli es un experimento aleatorio en el que sólo se pueden 
obtener dos resultados: éxito o fracaso. La variable aleatoria X de este experimento 
tiene probabilidad p de resultar en éxito.
​
Pensemos en el caso simple de lanzar una moneda justa al aire:"
​
#
​
"Si repetimos el experimento de Bernoulli un número considerado de veces y registramos 
las veces que el experimento resultó en éxito como 1 y en fracaso como 0, podemos 
obtener gráficamente la distribución de la variable aleatoria.
​
Entre más grande sea el número de experimentos que realizamos, más nos acercamos 
a la distribución real de la variable aleatoria"
​
n <- 1000
p <- 0.5
count = c()
for (i in seq(n)) {
  x <- sample(c("Águila", "Sol"), size = 1, prob = c(0.5, 1-p))
  if (x == "Águila") {
    count[i] = 1
  }
  else
    count[i] = 0
  
}
​
barplot(table(count)/length(count), 
        main = "Experimento de Bernoulli", 
        xlab = "Resultado",
        names = c("Sol", "Águila"))
​
"Para un experimento de Bernoulli tenemos que:
    - E[X] = p
    - SD[X] = sqrt(p*(1-p))
Esto podemos comprobarlo con las funciones descriptivas que hemos visto:"
​
#
#
​
"Si nos decidimos a repetir n veces un experimento de Bernoulli y definimos la 
variable aleatoria X como el número de éxitos en n experimentos de Bernoulli, 
entonces X~Binomial(n,p).
​
Por ejemplo: Un cliente tiene una probabilidad de 0.3 de realizar una compra en 
nuestra tienda. Si al día tenemos 10 clientes, ¿cuál es la probabilidad de que 
exactamente 0, 2, 4 y 10 de ellos realizan una compra?"
​
#
​
"Como podemos observar, la probabilidad va incrementando y posteriormente disminuye. 
Esto nos da una idea de la cómo se comporta la distribución del número de clientes 
que realizan una compra por día. Veamos ahora la distribución real de X:"
​
binom <- #
​
barplot(table(binom)/length(binom),
        main = "Distribución Binomial", 
        xlab = "# de clientes que realizan una compra")
​
"¿Cuál es la probabilidad de que menos de 4 clientes realicen una compra? Para ello 
podríamos sumar todas las probabilidades desde x = 0 hasta x = 4, pero existe otra forma 
más fácil con la función de distribución acumulada:"
dbinom(x = 0, size = 10, prob = 0.3) +
  dbinom(x = 1, size = 10, prob = 0.3) +
  dbinom(x = 2, size = 10, prob = 0.3) +
  dbinom(x = 3, size = 10, prob = 0.3)
​
#
​
"¿Cuál es la probabilidad de que más de 5 clientes realicen una compra?"
dbinom(x = 6, size = 10, prob = 0.3) +
  dbinom(x = 7, size = 10, prob = 0.3) +
  dbinom(x = 8, size = 10, prob = 0.3) +
  dbinom(x = 9, size = 10, prob = 0.3) +
  dbinom(x = 10, size = 10, prob = 0.3)
​
#
​
"Para una distribución binomial tenemos que:
    - E[X] = size*p
    - SD[X] = sqrt(size*p*(1-p))
Esto podemos comprobarlo con las funciones descriptivas que hemos visto:"
​
mean(binom)
sd(binom)
​
"Como pudiste darte cuenta en la gráfica de distribución de nuestra variable aleatoria,
esta está sesgada hacia la derecha. El sesgo de la distribución depende del parámetro p"
{par(mfrow=c(1,3))
  binom.der <- rbinom(n = 10000, size = 10, prob = 0.25)
  barplot(table(binom.der)/length(binom.der),
          main = "Sesgo hacia la derecha", 
          xlab = "X=x")
  binom.sim <- rbinom(n = 10000, size = 10, prob = 0.5)
  barplot(table(binom.sim)/length(binom.sim),
          main = "Simétrica", 
          xlab = "X=x")
  binom.izq <- rbinom(n = 10000, size = 10, prob = 0.75)
  barplot(table(binom.izq)/length(binom.izq),
          main = "Sesgo hacia la izquierda", 
          xlab = "X=x")}
dev.off()
​
## EJEMPLO 03: DISTRIBUCIÓN NORMAL, NORMAL ESTÁNDAR Y VALORES Z
### Aproximación de la distribución normal a la binomial
"La distribución normal tiene un papel fundamental en muchas áreas de estudio, ya que, 
de forma natural muchas variables siguen o pueden aproximarse a esta distribución.
​
Algunos puntos importantes de esta distribución son:
    - Tiene dos parámetros: X~N(Media, SD)
    - Es simétrica y con forma de campana"
{
  curve(dnorm(x, mean = 170, sd = 10), from = 100, to = 200, 
        col='blue', main = "Densidad Normal:\nDiferente media",
        ylab = "f(x)", xlab = "X")
  abline(v = 170, lwd = 0.5, lty = 2)
  curve(dnorm(x, mean = 150, sd = 10), from = 100, to = 200, 
        col='red', add = TRUE)
  abline(v = 150, lwd = 0.5, lty = 2)
  curve(dnorm(x, mean = 130, sd = 10), from = 100, to = 200, 
        col='green', add = TRUE)
  abline(v = 130, lwd = 0.5, lty = 2)
}
​
{
  curve(dnorm(x, mean = 150, sd = 5), from = 120, to = 180, 
        col='blue', main = "Densidad Normal:\nDiferente desviación estándar",
        ylab = "f(x)", xlab = "X")
  curve(dnorm(x, mean = 150, sd = 10), from = 120, to = 180, 
        col='red', add = TRUE)
  curve(dnorm(x, mean = 150, sd = 15), from = 120, to = 180, 
        col='green', add = TRUE)
}
​
"La distribución binomial puede aproximarse a la distribución normal cuando p es 
aproximadamente 0.5 y la muestra es >= 10"
binom.aprox <- rbinom(n = 10000, size = 10, prob = 0.5)
(binom.mean <- mean(binom.aprox))
(binom.sd <- sd(binom.aprox))
​
barplot(table(binom.aprox)/length(binom.aprox),
        main = "Distribución Binomial", 
        xlab = "X=x")
curve(dnorm(x, mean = binom.mean, sd = binom.sd), from=0, to=10, 
      col='blue', main = "Densidad de Probabilidad Normal",
      ylab = "f(x)", xlab = "X")
​
"Ahora vamos a demostrar que la aproximación es buena, calculando el promedio y la 
desviación estándar"
normal.binom <- #
​
#
#
​
### Distribución normal
"La distribución normal es una distribución para variables aleatorias continuas, 
por lo que, a diferencia de las distribuciones para variables discretas, la 
probabilidad sólo puede calcularse para intervalos de valores.
​
Por ejemplo: dada la siguiente variable aleatoria normalmente distribuida:"
mean <- 175
sd <- 6
x <- seq(-4, 4, 0.01)*sd + mean
y <- dnorm(x, mean = mean, sd = sd) 
​
plot(x, y, type = "l", xlab = "X", ylab = "f(x)",
     main = "Densidad de Probabilidad Normal", 
     sub = expression(paste(mu == 175, " y ", sigma == 6)))
​
integrate(dnorm, lower = x[1], upper = x[length(x)], mean=mean, sd = sd)
​
"Calcula P(X <= 180):"
#
​
par(mfrow = c(2, 2))
plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))
​
polygon(c(min(x), x[x<=180], 180), c(0, y[x<=180], 0), col="red")
​
"Calcula P(X <= 165):"
#
​
plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))
​
polygon(c(min(x), x[x<=165], 165), c(0, y[x<=165], 0), col="yellow")
​
"Calcula P(165 <= X <= 180):"
#
​
plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))
​
polygon(c(165, x[x>=165 & x<=180], 180), c(0, y[x>=165 & x<=180], 0), col="green")
​
"Calcula P(X >= 182):"
#
​
plot(x, y, type = "l", xlab="", ylab="", xlim = c(150, 200))
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)))
​
polygon(c(182, x[x>=182], max(x)), c(0, y[x>=182], 0), col="blue")
​
dev.off()
​
"Como con cualquier otra distribución, también podemos calcular los cuantiles de la 
distribución, es decir podemos encontrar el valor b, tal que P(X <= b) = 0.75:"
b <- #
b
​
"Podemos combrar el resultaso anterior calculando P(X <= 179.0469):"
#
​
### Distribución normal estándar y valores Z
"La distribución normal estándar es un caso especial de la distribución normal 
con media 0 y desviación estándar 1: Z ~ N(0,1). Esta distribución es de particular 
interés ya que todas las variables aleatorias X ~ N(Media, SD) pueden transformarse a
Z, lo cual nos permite poder comparar variables normalmente distribuídas entre sí.
​
Para pasar de X a Z, es necesario 'estandarizar' la variable X de la siguiente forma:
Z = (X - media)/ SD
​
Por ejemplo: Sea X ~ N(120, 85). Calcula la probabilidad de que X sea menor a 100"
pnorm(q = 100, mean = 120, sd = 85)
​
"Estandarizando tenemos que Z ~ N(0,1), y el valor estandarizado de 100 es"
z <- #
z
​
"Por lo tanto, la probabilidad de que Z sea menor a -0.2352941 es"
