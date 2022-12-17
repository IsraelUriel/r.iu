
"
Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente
los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en
su nivel socioeconómico, en si el hogar tiene recursos financieros extrar al ingreso y en si presenta
o no inseguridad alimentaria. Además, está interesado en un modelo que le permita identificar los
determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por
el Instituto Nacional de Salud Pública en México. La mayoría de las personas afirman que los hogares
con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas
con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar
presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:
"

# A) nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 Alto
# B) area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
# C) numpeho (Número de persona en el hogar)
# D) refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
# E) edadjef (Edad del jefe/a de familia)
# F) sexojef (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
# G) añosedu (Años de educación del jefe de familia)
# H) ln_als (Logarítmo natural del gasto en alimentos saludables)
# I) ln_alns (Logarítmo natural del gasto en alimentos no saludables)
# J) IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"


# Desarrollo : ------------------------------------------------------------

# Planteamiento del Problema"
# 1) Comprobar la relación entre el nivel socioeconómico con los gastos en alimentos saludables.
# 2) Se cuentan con recursos financieros extras?
# 3) Desarrollar modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria


# Librerias :
library('dplyr')
library('ggthemes')
source('modutil.r')

# Carga de Datos :

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

str(df) # Tipos de Datos de las Variables
head(df, 10) # 10 Primeros Elementos del Conjunto de Datos
dim(df) # Conocer la dimensiones del Conjunto de Datos
View(df) # Visualización Completa del DataFrame


# Limpieza : --------------------------------------------------------------

df_unfactored <- df <- cleanData(df) # Omitir Valores Nulos (28299 Filas de Datos)

# Transformación de Variables :

df$nse5f <- factor(df$nse5f, labels = c('Muy Bajo', 'Bajo', 'Medio', 'Medio Alto', 'Alto'))
df$area <- factor(df$area, labels = c('Zona Urbana', 'Zona Rural'))
df$refin <- factor(df$refin, labels = c('No', 'Si'))
df$sexojef <- factor(df$sexojef, label = c('Hombre', 'Mujer'))
df$IA <- factor(df$IA, labels = c('No Presenta IA', 'Presenta IA'))

glimpse(df) # Inspeccionar la estructura del Contenido del Conjunto de Datos


# Análisis Descriptivo ----------------------------------------------------

summary(df) # Resumen Estadístico

# Tabla Descriptiva - Variables Cualitativas y Cuantitativas
printDescriptiveTable(df)

# Distribución de Frecuencias de Variables Cualitativas:

df_cat <- df %>%
   select(nse5f, area, refin, sexojef, IA)

calculate_rel_freqs(df_cat)

# Nivel Socioeconómico
# Muy Bajo      Bajo      Medio    Medio Alto    Alto 
# 0.1751972  0.1936391  0.2031065  0.2151874  0.2128698 

# Area
# Zona Urbana  Zona Rural 
#  0.6883136   0.3116864 

# Refin
#    No       Si 
# 0.809714 0.190286 

# Sexojef
#    Hombre     Mujer 
#  0.7833826  0.2166174

# IA
# No Presenta IA  Presenta IA 
#   0.2886095      0.7113905  

# Ploteo de Variables Cualitativas
plot_bar2(df_cat, 2, FALSE)

# Distribución de Frecuencias de Variables Cuantitativas:

df_cal <- df %>%
  select(numpeho, edadjef, añosedu, ln_als, ln_alns)

source('modutil.r')
plot_histogram(df_cal, 8, 2, FALSE)


# 1) Comprobar la relación entre el nivel socioeconómico con los g --------

# Seleccionando Variables a analizar
sample <- df %>% 
  select(nse5f, ln_als, ln_alns) %>%
  mutate(nse5f = as.numeric(nse5f))
  
# Matriz de Correlación
pairs(~ nse5f + ln_als + ln_alns, 
      data = sample, gap = 0.4, cex.labels = 1.5)

round(cor(exp(sample)), 4)
# Valor ln_als mas cercano a 1 


y <- sample$nse5f
x1 <- sample$ln_als
x2 <- sample$ln_alns


# Model

attach(sample)
m1 <- lm(nse5f ~ ln_als + ln_alns)
summary(m1)

nc <- 0.05 # Usando nivel de confianza = 0.05
# Get Sig Variables < sig
sig.m1 <- summary(m1)$coeff[-1,4] < nc
names(sig.m1)[sig.m1 == TRUE]

m2 <- update(m1, nse5f ~.- ln_alns )
summary(m2)

m3 <- update(m1, nse5f ~.- ln_als )
summary(m3)
# Modelo 3
# Mediana proxima a lejana a 0 y R2 ajustada de 0.1178 cercana a 0
# indica No muy buen ajuste del modelo
