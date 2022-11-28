"
Una maquina de ensamblaje, tiene una probabilidad de 0.15 de ensamblar de forma defectuosa una una unidad. 
Si la producción de una unidad es totaltamente independiente de las demás y al día se producen 10 unidades:
  
set.seed(0202)
a) Grafica la función de distribución de la variable aleatoria. (Asume que se obtienen 10,000 muestras)
b) ¿Cuál es la probabilidad de que se produzcan dos unidades defectuosas?
  
c) ¿Cuál es la probabilidad de que a lo mucho 4 unidades sean defectuosas?
  
d) ¿Cuál es la probabiliad de que por lo menos tres unidades se encuentren defectuosa?
  
e) ¿Cuál es la probabilidad de que entre 2 y 4 unidades se encuentren defectuosas?
  
f) ¿Cuál es el número esperado de unidades defectuosas? ¿Con qué variación?
"

p <- 0.15
s <- 10
n <- 10000

set.seed(0202)

# a)

binom <- rbinom(n, s, p)

barplot(table(binom),length(binom),
        main = 'Distribución Aleatoria', 
        xlab = 'Unidades Producidas x Día',
        ylab = 'Muestras')

# b)

# pbinom(x = 2, size = 10, prob = 0.15)
x <- dbinom(2, s, p)
x <- round(x * 100, 2)
sprintf('R: %0.2f%%', x)
# 82.02%

# c)
x <- pbinom(4, s, p)
x <- round(x * 100, 2)
sprintf('R: %0.2f%%', x)

# d)
x <- pbinom(3, s, p, lower.tail = TRUE) 
# 1 - pbinom(2, s, p)
x <- round(x * 100, 2)
sprintf('R: %0.2f%%', x)

# e)
x <- pbinom(4, s, p, lower.tail = TRUE) - pbinom(1, s, p, lower.tail = TRUE)
# P(X <= b) - P(X <= a) = P(a < X <= b)
x <- round(x * 100, 2)
sprintf('R: %0.2f%%', x)

# g)
mean(bnimom)
sd(bnimom)
