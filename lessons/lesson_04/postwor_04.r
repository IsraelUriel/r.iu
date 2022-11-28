
# Development -------------------------------------------------------------
"
Utilizando la variable total_intl_charge de la base de datos telecom_service.csv 
de la sesión 3, realiza un análisis probabilístico. Para ello, debes determinar 
la función de distribución de probabilidad que más se acerque el comportamiento 
de los datos. Hint: Puedes apoyarte de medidas descriptivas o técnicas de visualización.

Una vez que hayas seleccionado el modelo, realiza lo siguiente:
  
Grafica la distribución teórica de la variable aleatoria total_intl_charge

¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?
  
¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?
  
¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?
  
Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?
  
¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 
80% de probabilidad?
"


# Solution ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(DescTools)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
summary(df)
str(df)

intl.charge <- df$total_intl_charge
m <- mean(intl.charge) # 2.76
s <- sd(intl.charge) # 0.75


# A) ----------------------------------------------------------------------

# Plot Distribution
initial_plot <- df %>%
  ggplot(aes(intl.charge)) +
  geom_histogram(bins = 10,
                 fill = '#304F6D',
                 alpha = 0.5,
                 color = '#304F6D',
                 linewidth = 1,
                 position = 'identity') +
  geom_density(aes(y = ..count.. * 0.6),
               color = '#36C9CE',
               linewidth = 0.75,
               bw = 0.25,
               lty = 2) +
  geom_vline(aes(xintercept = mean(intl.charge)), 
             color = "#6DCE36", 
             linetype = "dotted", 
             linewidth = 1) +
  labs(title = "International Charges",
       x = "Charges", 
       y = "Count") 
  theme_minimal()
  
  ggsave("inital_plot_intl_charge.jpg", plot = initial_plot, dpi = 300)

# B) ----------------------------------------------------------------------

p <- pnorm(1.85, m, s)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p) # R: 11.25%

# C) ----------------------------------------------------------------------

p <- pnorm(3, m, s, lower.tail = FALSE)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p) # R: 37.74%

# D) ----------------------------------------------------------------------

p <- pnorm(4.85, m, s) - pnorm(2.35, m, s)
p <- round(p * 100, 2)
sprintf('R: %0.2f%%', p) # R: 70.60%

# E) ----------------------------------------------------------------------

p <- qnorm(0.48, m, s)
sprintf('R: %0.2f$', p) # R: 2.73%

# F) ----------------------------------------------------------------------

q <- qnorm(p = 0.1, m, sd = s); 
q <- round(q, 2)
sprintf('R: %0.2f$', q) # R: 1.80%

q <- qnorm(p = 0.1, m, sd = s, FALSE)
q <- round(q, 2)
sprintf('R: %0.2f$', p) # R: 2.73%
