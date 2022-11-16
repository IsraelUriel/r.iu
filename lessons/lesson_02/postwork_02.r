# Instructions: -----------------------------------------------------------
"
Desarrollo
Inspecciona el DataSet iris_meaniris` disponible directamente en R. Identifica las 
variables que contiene y su tipo, asegúrate de que no hayan datos faltantes y que los 
datos se encuentran listos para usarse.
Crea una gráfica de puntos que contenga Sepal.Lenght en el eje horizontal, Sepal.Width 
en el eje vertical, que identifique Species por color y que el tamaño de la figura está
representado por Petal.Width. Asegúrate de que la geometría contenga shape = 10 y 
alpha = 0.5.
Crea una tabla llamada iris_mean que contenga el promedio de todas las variables agrupadas 
por Species.
Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos para agregar los 
promedios en la visualización. Asegúrate que el primer argumento de la geometría sea el 
nombre de tu tabla y que los parámetros sean shape = 23, size = 4, fill = black y 
stroke = 2. También agrega etiquetas, temas y los cambios necesarios para mejorar 
tu visualización.
"


# Solution: ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Analizing Table
names(iris)
class(iris)
str(iris)
dim(iris)
View(iris)

# Checking NA Values
is.na(iris)

# Plotting: 
# Point Graph: x = Sepal.Length, y = Sepal.Width
#             Species by Color, Size by Peta.width
#             Shape = 10 and alpha = 0.5

initial_plot <-
      ggplot(iris, aes(Sepal.Length, Sepal.Width, size = Petal.Width, 
      color = factor(Species))) +
      geom_point(shape = 10, alpha = 0.5) +
      scale_color_manual(values=c('#2B8EC7', '#7EC72B', '#C72BA8'))

ggsave("initial_plot.png", plot = initial_plot, dpi = 300)

# last_plot()

# Create Table: Mean by Species

iris_mean <- iris %>%
             select_all() %>%
             group_by(Species) %>%
             summarize(m_SL = mean(Sepal.Length),
                       m_SW = mean(Sepal.Width),
                       m_PL = mean(Petal.Length),
                       m_PW = mean(Petal.Width))

# Plotting:
# Add Means Plot to Previous Plot.
# Shape = 23, Size = 4, fill = black, stroke = 2

mean_plot <-
    ggplot(iris_mean, aes(m_SL, m_SW, m_SW, color = factor(Species))) +
    geom_point(shape = 23, size = 4, fill = 'black', stroke = 2) +
    scale_color_manual(values=c('#2B8EC7', '#7EC72B', '#C72BA8'))


# Combining Plots

final_plot <- 
    ggplot() +
    geom_point(data = iris, 
               aes(Sepal.Length, Sepal.Width, 
                   size = Petal.Width, 
                   color = factor(Species)), 
               shape = 10, 
               alpha = 0.5) +
    geom_point(data = iris_mean, 
               aes(m_SL, m_SW,
                   color = factor(Species)),
               shape = 23, 
               size = 4, 
               fill = 'black', 
               stroke = 2) +
    scale_color_manual(values=c('#2B8EC7', '#7EC72B', '#C72BA8')) +
    labs(title = 'Iris Species',
         x = 'Sepal Length',
         y = 'Sepal Width',
         colour = 'Species',
         size = 'Petal Width') +
    theme_minimal()

ggsave('final_plot.png', plot = final_plot, dpi = 300)

