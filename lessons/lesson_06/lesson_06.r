################ SESIÓN 6: REGRESIÓN LINEAL Y CLASIFICACIÓN  ##################
###############################################################################

## EJEMPLO 01: MODELO DE REGRESIÓN LINEAL
"La regresión lineal es una técnica de modelado estadístico que se emplea para 
describir una variable de respuesta continua como una función de una o varias 
variables predictoras. Puede ayudar a comprender y predecir el comportamiento de 
distintos fenómenos aleatorios

Este modelo describe la relación entre una variable dependiente como una función 
de una o varias variables independientes. El método más utlizado para su estimación 
es el de Mínimos Cuadrados Ordinarios, el cual consiste en minizar la suma de los 
residuos al cuadrado, siendo el residuo la diferencia que hay entre el valor observado 
y el valor pronosticado por una propuesta de modelo.

Estos modelos lineales tienen, como toda línea recta, una intersección y una pendiente 
asociada a cada variables explicativa.

Ahora vamos cómo hacer la estimación de un modelo y hagamos la interpretación de los 
resultados obtenidos:"
library(dplyr)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/nyc_restaurants.csv", header = TRUE)
str(df)

df$East <- factor(df$East, labels = c("No", "Yes"))

# Para una correlación, Y debe ser una variable continua

df.select <- select(df, Price, Food, Decor, Service)
round(cor(df.select),4)  

pairs(~ Price + Food + Decor + Service + East, 
      data = df, gap = 0.4, cex.labels = 1.5)

"Estimación por Mínimos Cuadrados Ordinarios (OLS)
Y = beta0 + beta1*Food + beta2*Decor + beta3*Service + beta4*East + e"

# Y = Price en este ejemplo
# Los betas son los coeficientes de regresión

# Toma todas las variables de DF y los va a enmascar para solo nombrarlas (Nombre de la variable)
attach(df)
m1 <- lm(Price ~ Food + Decor + Service + East) 

# lm = Linear Model

# Resumen
summary(m1)

# Las relaciones lineales se suman
# t-value: A cuantas desviaciones estandard estamos alejados
# t = beta_i/std.Error
# En el summary, los valores a comparar es: Multiple R-Squared y Adjusted R-Squared
# Intecept: Si no hay intercepto no hay regresión lineal.
# es un promedio condicional de la variable precio
# Estimate: Efecto marginal, incremento o decremento en relación con las variable de´pendiente
# Con variables cualitativas son efectos diferenciadores
# Cuando se presentan asteriscos : tienen un nivel cercano a cero en la t
# Los Pr son los p.values
# Significancia = 0.05: para la comparacion de hipotesis con Pr, debe reflejarse a nivel poblacional
# para comprobar el comportamiento, esto con Estimate.
# Tomar Adjusted R-Squared por que penaliza

# Hipotesis
# Ho: beta_i = 0
# Ha: beta_i != 0

# Para extraer los valores:
# m1$coefficients
# m1$coefficients[4]

# coef(summary(modelname))[, "Pr(>|t|)"]
# summary(m1)$  

# Cuando las variables presentan buena relación, usar 

"De los resultados anteriores, podemos concluir que el coeficiente de la variable 
Service no es significativo. Probemos nuestro modelo sin incluir dicha variable:
Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e"
# Actualizar modelo
# Equivale a m1 <- lm(Price ~ Food + Decor + Service)
m2 <- update(m1, ~.- Service)
summary(m2)


# Extracción de Variables
coef(summary(m1))[, "Pr(>|t|)"]
coef(summary(m1))[, "Pr(>|t|)"][3]
summary(m1)$coefficients[,4][3]

# TÉRMINOS DE INTERACCIÓN
"La variable East es una variable dicotómica que identifica si el restaurante está 
en el este de la 5ta Avenida o no. Con este tipo de variables, podemos evaluar 
efectos cruzados, es decir, podemos generar un efecto diferenciados en cada una de 
las variables continuas dependiendo de si miden a un restaurante que está o no en 
el este de la 5ta Avenida.

Con esto en mente, podemos considerar el siguiente modelo con efectos cruzados:
Y = beta0 + beta1*Food + beta2*Decor +  beta3*Service + beta4*East 
      + beta5*Food*East + beta6*Decor*East + beta7*Service*East + e (Completo)
El cual estiamos de la siguiente forma"
mfull <- lm(Price ~ Food + Decor + Service + East + 
              Food:East + Decor:East + Service:East)

summary(mfull)

"De forma individual, los coeficientes de los términos de interacción no son significativos.
Sin embargo, también debemos evaluar la significancia global del modelo, esto es, 
podemos comparar un modelo tomando en cuenta todos los efectos cruzados y compararlo 
contra otro modelo sin efectis cruzados.

Para ello, planteamos el siguiente juego de hipótesis:
H0: beta3 = beta5 = beta6 = beta7 = 0
(Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e)

H1: H0 no es verdad (AL MENOS UN COEFICIENTE ES DISTINTO DE 0)
(Y = beta0 + beta1*Food + beta2*Decor +  beta3*Service + beta4*East 
         + beta5*Food*East + beta6*Decor*East + beta7*Service*East + e)

Para este tipo de inferencia usamos el enfoque de análisis de varianza (ANOVA), 
ya que estamos comparando la variabilidad de un modelo no restringido contra la 
variabilidad de un modelo restringido."
#

"Con base en el p-value del estadístico de prueba, no podemos rechazar Ho, por lo 
existe al menos un coeficiente de los terminos de interacción que es distinto de 0 y,
por lo tanto, contribuyen en la explicación del precio:
Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e (Reducido)"

## EJEMPLO 02: SUPUESTOS DEL MODELO DE REGRESIÓN LINEAL Y PREDICCIÓN
"El modelo de regresión lineal clásico establece ciertos supuestos en el término 
de error:
1) Eltérmino de error no tiene correlación significativa con las variables 
explicativas. En caso contrario, tendríamos un problema de endogeneidad.
2) El término de error sigue una distribución normal"


# rstandard: residuales estandarizados


StanRes2 <- rstandard(m2)

par(mfrow = c(2, 2))

plot(Food, StanRes2, ylab = "Residuales Estandarizados")
plot(Decor, StanRes2, ylab = "Residuales Estandarizados")
plot(East, StanRes2, ylab = "Residuales Estandarizados")


# Gráfica de quantiles, para ver si la distribucion se acerca a una normal teorica

qqnorm(StanRes2)
qqline(StanRes2)

dev.off()


# Contrasta si la relation de distribucion del error hacua la normal teorica
shapiro.test(StanRes2)
"Ho: La variable distribuye como una normal
Ha: La variable no distribuye como una normal"

# sig: 0.1 = Rechazo Ho
# sig: 0.05 = No Rechazo Ho   
# sig: 0.01 = No Rechazo Ho

"Una vez validados estos supuestos, podemos realizar utilizar nuestro modelo estimado 
para realizar predicciones y obtener su intervalo de confianza"
data <- data.frame(
  Food = c(18.6, 30.3, 22.1, 24.9),
  Decor = c(20, 15, 10, 9),
  East = c("Yes", "No", "Yes", "No")
)

# a un nivel de confianza de 0.95
# Fit: Pronostica el precio
# lwr: limite inferior de la variable precio
# upr: limite superior de la variable precio

predict(m2, newdata = data, interval = "confidence", level = 0.95)

## EJEMPLO 03: MODELO DE REGRESIÓN LOGÍSTICA
"La regresión logística es un tipo de análisis de regresión utilizado para predecir 
el resultado de una variable categórica en función de las variables independientes 
o predictoras.

Es útil para modelar la probabilidad de un evento ocurriendo en función de otros factores."

# No es un modelo lineal
# Maxima Similutd
# Analizar Variables Cualitiva
# Dice la razon de probabilidad no el valor
# Lo expone en logaritmos
# Momio: Mientras grande sea mas es probable 
# Odds o nomio: Momio = probabilidad del exito / 1 - la probalidad del fracaso => p/(1-p)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")

attach(df)
# y = df$churn
# x = df$customer_service_calls

y = df$churn
x <- df$customer_service_calls

"Vamos a estimar la regresión logística y a interpretar los resultados:"
# logistic.1 <- glm(churn ~ customer_service_calls, family = binomial)
logistic.1 <- glm(y ~ x, family = binomial)
summary(logistic.1)

# Quitar logaritmo
exp(coef(logistic.1))

"Veamos una representación gráfica del problema para enteder su diferencia respecto 
a la regresión lineal:"

plot(churn ~ customer_service_calls, data=df, xlim = c(0,10))
curve(predict(logistic.1, newdata = data.frame(x), type = "response"),
      add = TRUE)



"A diferencia del modelo de regresión lineal, no es posible calcular el R2 de una 
regresión logística, sin embargo, se puede calcular la bondad de ajuste con base en 
la log-verosimilitud del modelo nulo y el modelo actual."
pseudo_r2.1 <- #
pseudo_r2.1

"Comparemos con otro modelo:"
logistic.2 <- update(logistic.1, ~.+ total_day_calls + total_day_charge)
summary(logistic.2)

pseudo_r2.2 <- (logistic.2$null.deviance - logistic.2$deviance)/logistic.2$null.deviance
pseudo_r2.2
