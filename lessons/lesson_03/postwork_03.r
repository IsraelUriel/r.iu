# Postwork Sesión 3

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe

#### Requisitos

#1. R, RStudio
#2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
#3. Curiosidad por investigar nuevos tópicos y funciones de R

#### Desarrollo

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. 
No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."

library(ggplot2)
library(dplyr)
library(DescTools)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")

# 1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`

str(df)
View(df)

df <- na.omit(df)

df$Categoria <- factor(df$Categoria)
df$Grupo <- factor(df$Grupo, labels = c('A', 'B'))

glimpse(df)

summary(df)

Mode(df$Mediciones)
median(df$Mediciones)
mean(df$Mediciones)
hist(df$Mediciones)

# 2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?
"Sesgo hacia la izquierda"

#3) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`

desv.std <- sd(df$Mediciones)
qrt <- quantile(df$Mediciones, probs = c(0.25, 0.5, 0.75))

"La mayor parte de los datos se encuentran en el tercer cuartil"

# 4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
# ¿Consideras que sólo una categoría está generando el sesgo?"

histogram_Mediciones <-
ggplot(df, aes(Mediciones)) +
  geom_histogram(aes(fill = Categoria), 
                 position = 'identity',
                 bins = 6) +
  geom_vline(aes(xintercept = mean(Mediciones)), 
             color = "#1F1949", 
             linetype = "dotted", size = .25) + 
  labs(title = "Mediciones por Categoría",
       x = "Mediciones", 
       y = "Frecuencia") + 
  theme_minimal()

ggsave("Histogram - Mediciones por Categoria.jpg", plot = histogram_Mediciones, dpi = 300)
# Sesgo Izquierda


# 5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
# y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? 
# ¿Los grupos al interior de cada categoría 
# podrían estar generando el sesgo?"

boxplot_Mediciones <-
ggplot(df, aes(Mediciones, 
               Categoria, 
               fill = Grupo), 
       color = '#1F1949') +
  geom_boxplot() +
  labs(title = "Mediciones por Categoría",
       x = "Mediciones", 
       y = "Categoría") + 
  theme_minimal()

ggsave("Boxplot - Mediciones por Categoria.jpg", plot = boxplot_Mediciones, dpi = 300)

"Hay Diferencia de distribucion entre las categorias, se presenta sesgo debido a la concenstracion de datos,
Tambien se denotan datos fuera de rango."


