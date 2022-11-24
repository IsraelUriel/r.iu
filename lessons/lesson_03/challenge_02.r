"
Para este reto vamos a hacer uso del dataframe diamonds, disponible en la librería de ggplot2. 

Realiza los siguientes incisos y responde a las preguntas:
  
Calcula e interpreta las medidas de tendencia central de la variable price

Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo del precio?
  
Calcula e interpreta la desviación estándar y los cuartiles de la distribución:
  
Realiza un histograma de la variable precio. ¿Su distribución coincide con tu conclusión de la 
pregunta 2?
  
Realiza un boxplot de la variable precio. ¿Su forma se relaciona con la distribución mostrada 
por el histograma anteior? ¿Existen datos atípicos?
"

library(ggplot2)
library(DescTools)

# Interpreta las medidas de tendencia central de la variable price

Mode(diamonds$price) 
median(diamonds$price)
mean(diamonds$price)
# moda < mediana < media, sesgo a la derecha

# Interpreta la desviación estándar y los cuartiles

sd(diamonds$price) 
quantile(diamonds$price, probs = c(0.25, 0.50, 0.75))
# El precio tiene una dispersión respecto a la media

# Histograma de Precio
hist(diamonds$price)
# Sesgo a la derecha

# Realiza un boxplot de Precio

boxplot(diamonds$price)
# Coincide con el Histograma
