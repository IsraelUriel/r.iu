
# Introduction ------------------------------------------------------------

"
El Dataframe 'inseguridad_alimentaria' contiene información de la Encuesta Nacional de Salud y 
Nutrición (ENSANUT) 2012, específicamente del módulo de condiciones socieconómicas del hogar y 
del módulo de percepción de inseguridad alimentaria en el hogar.

Dentro del dataframe se encuentran las siguientes variables, entre otras:
  
IA: 1 si el hogar presenta inseguridad alimentaria, 0 en otro caso
numpeho: Número de personas en el hogar
edadjef: Edad del jefe de familia
sexojef: 0 si el jefe familia es Hombre, 1 si es Mujer
añosedu: Años de educación del jefe de familia
alsa: Gasto semanal en gastos alimentos saludables

El objetivo es construir un modelo que nos permita estimar el impacto que tienen variables socieonómicas 
en la probabilidad de que un hogar presente seguridad alimentaria.

Asegurate que tu modelo final sea aquel con el pseudo_r2 más alto e interpreta los log odds del modelo estimado.

NOTA: No olvides probar efectos de interacción
"


# A) ----------------------------------------------------------------------

library('dplyr')
library('ggplot2')

df <- read.csv('https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/inseguridad_alimentaria.csv', header = TRUE)
str(df)
summary(df)
sig <- 0.1

# Data Cleanup:

# Select objective columns
sample <- select(df, 'IA', 'numpeho', 'edadjef', 'sexojef', 'añosedu', 'alsa')

# Transforming Data
sample$IA <- factor(sample$IA, labels = c('No','Si'))
sample$sexojef <- factor(sample$sexojef, labels = c('H', 'M'))

levels(sample$IA)
levels(sample$sexojef)

# Deleting N/A Data
# N/A Values?
if (sum(is.na(sample)) > 0) {
  na.omit(sample)
}

glimpse(sample)


# B) ----------------------------------------------------------------------

# El objetivo es construir un modelo que nos permita estimar el impacto que tienen variables socieonómicas 
# en la probabilidad de que un hogar presente seguridad alimentaria.

attach(sample)

# Model
logistic.m1 <- glm(IA ~ numpeho + edadjef + sexojef + añosedu + alsa +
                  edadjef:sexojef + añosedu:sexojef, 
                  data = sample, family = binomial)

summary(logistic.m1)
exp(coef(logistic.m1))

# Get Sig Variables < sig
sig.m1 <- summary(logistic.m1)$coeff[-1,4] < sig
names(sig.m1)[sig.m1 == TRUE]


# R2

pseudo_r2.m1 <- (logistic.m1$null.deviance - logistic.m1$deviance)/logistic.m1$null.deviance
pseudo_r2.m1







# Plot Relations
#y <- sample$IA
#x <- sample$numpeho

#rel.m1 <- glm(y ~ x, family = binomial)












"
par(mfrow = c(2, 3))

ggplot(df, aes(x=numpeho, y=IA)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),
              col="red", lty=2)

ggplot(df, aes(x=edadjef, y=IA)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),
              col="red", lty=2)

ggplot(df, aes(x=añosedu, y=IA)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),
              col="red", lty=2)

ggplot(df, aes(x=alsa, y=IA)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),
              col="red", lty=2)

ggplot(df, aes(x=sexojef, y=IA)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),
              col="red", lty=2)


dev.off()
"
