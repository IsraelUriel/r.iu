

# Proyecto Integrador - Equipo 6

## Problema

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

### Diccionario de Datos:

 A) nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 Alto
 
 B) area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
 
 C) numpeho (Número de persona en el hogar)
 
 D) refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
 
 E) edadjef (Edad del jefe/a de familia)
 
 F) sexojef (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
 
 G) añosedu (Años de educación del jefe de familia)
 
 H) ln_als (Logarítmo natural del gasto en alimentos saludables)
 
 I) ln_alns (Logarítmo natural del gasto en alimentos no saludables)
 
 J) IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"


### Planteamiento del Problema

#### Librerias empleadas:
```R
library(dplyr)
library(DescTools)
library(ggplot2)
library(zoo)
```
### Carga de Informacion
```R
f <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

head(df,10) # Analizar los primeros 10 observaciones del conjunto de datos
str(df) # Analizar el tipo de datos de las variables del conjunto de datos```
```

### Limpieza de Datos

#### Transformación de datos:
#####  Aplicación de factores

```R
df$nse5f <- factor(df$nse5f, labels = c("Bajo", "Medio Bajo", "Medio", "Medio Alto", "Alto")) <br>
df$area <- factor(df$area, labels = c("Zona Urbana", "Zona Rural")) <
df$refin <- factor(df$refin, labels = c("No", "Si"))
df$sexojef <- factor(df$sexojef, labels = c("Hombre", "Mujer"))
df$IA <- factor(df$IA, labels = c("No Presenta IA", "Presenta IA"))

glimpse(df) # Analizar los factores aplicados
```

### 1.Plantea el problema del caso

Comprobar la relación entre el nivel socioeconómico de los hogares con los gastos en
alimentos saludables y no saludables, así como si cuentan con recursos financieros extra,
teniendo en cuenta si se presenta inseguridad alimentaria y los determinantes socioeconómicos asociados a ella

```R
#reemplazo de valores NA en als y alns por los promedios sin casos NA
df$ln_als[is.na(df$ln_als)]<-mean(df$ln_als,na.rm=TRUE)
df$ln_alns[is.na(df$ln_alns)]<-mean(df$ln_alns,na.rm=TRUE)
```

### 2.Realiza un análisis descriptivo de la información

```R
summary(df) # Resumen estadístico del conjunto de datos
"
nse5f               area          numpeho       refin         edadjef       sexojef         añosedu                   IA       
 Bajo      :8858   Zona Urbana:26591   Min.   : 1.000   No:33046   Min.   : 18    Hombre:26957   Min.   : 0.00   No Presenta IA:10781  
 Medio Bajo:8560   Zona Rural :14218   1st Qu.: 3.000   Si: 7763   1st Qu.: 37    Mujer : 8861   1st Qu.: 9.00   Presenta IA   :30028  
 Medio     :8323                       Median : 4.000              Median : 47    NA's  : 4991   Median : 9.00                         
 Medio Alto:7903                       Mean   : 3.941              Mean   : 49                   Mean   :10.36                         
 Alto      :7165                       3rd Qu.: 5.000              3rd Qu.: 60                   3rd Qu.:12.00                         
                                       Max.   :19.000              Max.   :111                   Max.   :24.00                         
                                                                   NA's   :5017                                                        
     ln_als          ln_alns     
 Min.   :0.6931   Min.   :0.000  
 1st Qu.:5.7104   1st Qu.:3.912  
 Median :6.1506   Median :4.125  
 Mean   :6.0665   Mean   :4.125  
 3rd Qu.:6.5439   3rd Qu.:4.248  
 Max.   :8.9699   Max.   :8.403"
```

```R
#Análisis de la Variable Nivel Socioeconómico
freq <- table(df$nse5f) # Función que obtiene una tabla de frecuencias del conjunto de datos
#Grafíca de barras que visualiza la distribución de la variable Nivel Socioeconómico
ggplot(df, aes(x = nse5f, fill = nse5f)) +
  geom_bar(aes(color = nse5f, fill = nse5f), alpha = 0.4) +
  labs(title = "Nivel socioeconómico de los hogares",x = "Nivel Socioeconómico", y = "Frecuencia") + 
  theme_light()
" Bajo        Medio Bajo    Medio   Medio Alto   Alto 
  8858           8560       8323       7903      7165"
#El nivel Socioeconómico Bajo es la variable que presenta la mayor frecuencia del conjunto de datos    

#Análisis de la Variable Área
(freq <- table(df$area))
ggplot(df, aes(x = area, fill = area)) +
  geom_bar(aes(color = area, fill = area), alpha = 0.4) +
  labs(title = "Zona de los hogares",x = "Zona", y = "Frecuencia") + 
  theme_light()
"Zona Urbana  Zona Rural 
      26591       14218 "

#Análisis de la variable Recursos Financieros
(freq <- table(df$refin))
ggplot(df, aes(x = refin, fill = refin)) +
  geom_bar(aes(color = refin, fill = refin), alpha = 0.4) +
  labs(title = "Recursos financieros extra en los hogares",x = "¿Cuenta con recursos financieros extra?", y = "Frecuencia") + 
  theme_light()
"No     Si 
33046  7763"

#Análisis de la variable Sexo Jef@
(freq <- table(df$sexojef))
ggplot(df, aes(x = sexojef, fill = sexojef)) +
  geom_bar(aes(color = sexojef), alpha = 0.4) +
  scale_color_manual(values = c("#97DB4F", "#E55381")) +
  scale_fill_manual(values = c("#97DB4F", "#E55381")) +
  labs(title = "Sexo del jefe del hogar",x = "Sexo", y = "Frecuencia") + 
  theme_light()
"Hombre  Mujer 
  26957   8861"

#Análisis de la variable Inseguridad Alimentaria
(freq <- table(df$IA))
ggplot(df, aes(x = IA, fill = IA)) +
  geom_bar(aes(color = IA, fill = IA), alpha = 0.4) +
  scale_color_manual(values = c("#97DB4F", "#E55381")) +
  scale_fill_manual(values = c("#97DB4F", "#E55381")) +
  labs(title = "Hogares con inseguridad alimentaria",x = "¿Presenta inseguridad alimentario?", y = "Frecuencia") + 
  theme_light()
"No Presenta IA    Presenta IA 
        10781         30028 "
```
####  Medidas de tendencia central sobre las variables Alimentos Saludables y No Saludables
```R
(mean.als <- mean(df$ln_als)) #6.066521
(sd.als <- sd(df$ln_als)) #0.7387856
(Mode(df$ln_als)[1]) # 6.066521

(mean.alns <- mean(df$ln_alns)) # 4.124941
(sd.alns <- sd(df$ln_alns)) # 0.7896813
(Mode(df$ln_alns)[1]) #4.124941

#Por nivel socioeconómico
(nivel.mean.sd <- df %>%
    select(nse5f, ln_als, ln_alns) %>%
    group_by(nse5f) %>%
    summarize(mean_ln_als = mean(ln_als),
              sd_ln_als = sd(ln_als),
              mean_ln_alns = mean(ln_alns),
              sd_ln_alns = sd(ln_alns)))
" nse5f      mean_ln_als sd_ln_als mean_ln_alns sd_ln_alns
1 Bajo              5.70     0.791         3.93      0.668
2 Medio Bajo        5.93     0.706         4.01      0.698
3 Medio             6.08     0.656         4.09      0.743
4 Medio Alto        6.24     0.644         4.20      0.817
5 Alto              6.47     0.627         4.46      0.925"

boxplot(ln_als ~ nse5f,data = df)
boxplot(ln_alns ~ nse5f,data = df)

#De acuerdo a la interpretación de los bloxplots nos muestra una disperción
#elevada de los datos con respecto a la media, lo cual nos da un indicativo
#de que por su elevada variabilidad la correlación podría ser baja.
```


### 3. Calcula probabilidades que nos permitan entender el problema en México

```R
#Función que nos permite generar un correlograma asociando las variables: 
#Nivel Socioencónomico y gastos en alimentos saludables y no saludables.
pairs(~ nse5f + ln_als + ln_alns, 
      data = df, gap = 0.4, cex.labels = 1.5)
# Con base en estas gráficas se puede inferir que no hay correlación fuerte entre el gasto en alimentos
# saludables y no saludables con relación al nivel socioeconómico, ya que se ven gráficas con valores
# muy dispersos.    

#Tranformación del conjunto de datos:
#Se seleccionan las variables a utilizarse,
#Se transforma variable nse5f a tipo numérico
sample <- df %>% 
  select(nse5f, ln_als, ln_alns)  %>%
 mutate(nse5f = as.numeric(nse5f))

#Se convierte a valor logarítmico a exponente, 
#se realiza correlación del conjunto de datos sample,
#se redondea el resultado a cuatro digitos
round(cor(exp(sample)), 4)
"--------- nse5f -- --ln_als ----   ln_alns
nse5f  -- 1.0000 -- 0.3459 	--  0.2175
ln_als  --- 0.3459 -- 1.0000 -- 0.2742
ln_alns -- 0.2175 -- 0.2742 -- 1.0000
"
# En este análisis se puede ver que las variables del gasto en alimentos saludables y no saludables 
# tienen una correlación positiva, donde tenemos que el 34.59 % del gasto en alimentos saludables
# se explica por el nivel socioeconómico y el 21.75 % del gasto en alimentos no saludables se explica 
# con base en el nivel socioeconómico.


#Función que nos permite relacionar las probabilidades de correlación entre las variables
prop.table(table(df$nse5f, df$refin),1)
"                  No        Si
  Bajo       0.7964552 0.2035448
  Medio Bajo 0.7929907 0.2070093
  Medio      0.8016340 0.1983660
  Medio Alto 0.8166519 0.1833481
  Alto       0.8481507 0.1518493"

#En el nivel Bajo existe un 79.6% de probabilidad que el hogar no cuente con recursos extra pero un 20.35% de que si exista
#En el nivel Medio Bajo existe un 79.3% de probabilidad que el hogar no cuente con recursos extra pero un 20.7% de que si exista
#En el nivel Medio existe un 80.16% de probabilidad que el hogar no cuente con recursos extra pero un 19.83% de que si exista
#En el nivel Medio Alto existe un 81.66% de probabilidad que el hogar no cuente con recursos extra pero un 18.33% de que si exista
#En el nivel Alto existe un 84.81% de probabilidad que el hogar no cuente con recursos extra pero un 15.18% de que si exista

#Función que nos permite obtener las frecuencias de las variables con respecto a la muestra. 
#Se analiza la relación entre la variable de nivel socioecnómico y el recursos financiero
transform(table(df$nse5f, df$refin),
          rel.freq=prop.table(Freq), 
          cum.freq=cumsum(prop.table(Freq)))
"
         Var1 Var2 Freq   rel.freq  cum.freq
1        Bajo   No 7055 0.17287853 0.1728785
2  Medio Bajo   No 6788 0.16633586 0.3392144
3       Medio   No 6672 0.16349335 0.5027077
4  Medio Alto   No 6454 0.15815139 0.6608591
5        Alto   No 6077 0.14891323 0.8097724
6        Bajo   Si 1803 0.04418143 0.8539538
7  Medio Bajo   Si 1772 0.04342179 0.8973756
8       Medio   Si 1651 0.04045676 0.9378323
9  Medio Alto   Si 1449 0.03550687 0.9733392
10       Alto   Si 1088 0.02666079 1.0000000
"

#Se analiza la relación entre la variable de nivel socioecnómico y la inseguridad alimentaria
prop.table(table(df$nse5f, df$IA),1)
"              No Presenta IA Presenta IA
  Bajo            0.1290359   0.8709641
  Medio Bajo      0.1810748   0.8189252
  Medio           0.2255196   0.7744804
  Medio Alto      0.3246868   0.6753132
  Alto            0.5087230   0.4912770"
#En el nivel Bajo existe un 12.9% de probabilidad que no tengan Inseguridad Alimentaria pero un 87.09% de que si exista
#En el nivel Medio Bajo existe un 18.10% de probabilidad que no tengan Inseguridad Alimentaria pero un 81.89% de que si exista
#En el nivel Medio existe un 22.55% de probabilidad no tengan Inseguridad Alimentaria pero un 77.44% de que si exista
#En el nivel Medio Alto existe un 32.46% de probabilidad que no tengan Inseguridad Alimentaria pero un 67.53% de que si exista
#En el nivel Alto existe un 50.87% de probabilidad que no tengan Inseguridad Alimentaria pero un 49.12% de que si exista

#Se analiza la relación entre la variable de nivel socioecnómico y la inseguridad alimentaria
transform(table(df$nse5f, df$IA),
          rel.freq=prop.table(Freq), 
          cum.freq=cumsum(prop.table(Freq)))

"
         Var1           Var2 Freq   rel.freq   cum.freq
1        Bajo No Presenta IA 1143 0.02800853 0.02800853
2  Medio Bajo No Presenta IA 1550 0.03798182 0.06599035
3       Medio No Presenta IA 1877 0.04599476 0.11198510
4  Medio Alto No Presenta IA 2566 0.06287829 0.17486339
5        Alto No Presenta IA 3645 0.08931853 0.26418192
6        Bajo    Presenta IA 7715 0.18905143 0.45323336
7  Medio Bajo    Presenta IA 7010 0.17177583 0.62500919
8       Medio    Presenta IA 6446 0.15795535 0.78296454
9  Medio Alto    Presenta IA 5337 0.13077998 0.91374452
10       Alto    Presenta IA 3520 0.08625548 1.00000000
"

#Gráfica que nos permite visualizar el comportamiento de la distribución de las
"variables del gasto en los alimentos.
{curve(dnorm(x, mean = mean.als, sd = sd.als), from = 0, to = 10, 
       col='blue', main = "Densidad Normal:\nln_als y ln_alns",
       ylab = "f(x)", xlab = "X")
  legend(x = 8.5, y = 0.5, legend=c("ln_als", "ln_alns"),
         col=c("blue", "red"), lty = 1, bty = "n", cex=0.8)
  curve(dnorm(x, mean = mean.alns, sd = sd.alns), from = 0, to = 10, 
        col='red', add = TRUE)
}
#Se observa que ambas variables presentan una distribución normal

```


### 4. Planteamiento de hipótesis estadísticos y concluye sobre ellos para entender el problema en México

```R
df_2$ln_als[is.na(df_2$ln_als)]<-mean(df_2$ln_als,na.rm=TRUE)
df_2$ln_alns[is.na(df_2$ln_alns)]<-mean(df_2$ln_alns,na.rm=TRUE)

"La mayoría de las personas afirman que los hogares con menor nivel socioeconómico () tienden a
gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros
determinantes, lleva a que un hogar presente cierta inseguridad alimentaria"
var.test(df_2[df_2$nse5f < 3, "ln_alns"],
         df_2[df_2$nse5f > 3, "ln_alns"],
         ratio = 1, alternative = "two.sided")

"Planteamiento de hipótesis:
Ho: ln_alns_nse5f1-2 <= ln_alns_nse5f4-5
Ha: ln_alns_nse5f1-2 > ln_alns_nse5f4-5"

t.test(x = df_2[df_2$nse5f > 3, "ln_alns"],
       y = df_2[df_2$nse5f < 3, "ln_alns"],
       alternative = "greater", mu = 0, var.equal = FALSE) #p-value < 2.2e-16
#A nivel de confianza estándar, EEE para rechazar la Ho, el gasto en productos no saludables en los hogares
#con menor nivel socioeconómico es mayor al gasto de los hogares con mayor nivel socioeconómico

```

### 5.Estima un modelo de regresión, lineal o logístico, para identificiar los determinantes de la inseguridad alimentaria en México
attach(df_2)
#Regresión logística
#IA en relación con años de educación
y = df_2$IA
x = df_2$añosedu

logistic.1 <- glm(y ~ x, data = df.b, family = binomial)
plot(logistic.1)
summary(logistic.1)

par(mfrow = c(1, 1))
plot(IA ~ añosedu, data=df.b, xlim = c(0,50))
curve(predict(logistic.1, newdata = data.frame(x), type = "response"),add = TRUE)

#IA en relación con número de personas que viven en el hogar
x = df.b$numpeho
logistic.1 <- glm(y ~ x, data = df.b, family = binomial)

summary(logistic.1)

plot(IA ~ numpeho, data=df.b, xlim = c(0,20))
curve(predict(logistic.1, newdata = data.frame(x), type = "response"),add = TRUE)
```

### 6.Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github.



" Comprobar : La mayoría de las personas afirman que los hogares
con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas
con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar
presente cierta inseguridad alimentaria"

dfc.summ <- df %>%
  select(nse5f, ln_als, ln_alns, IA) %>%
  mutate(sumaing = ln_als + ln_alns) %>%
  group_by(nse5f) %>%
  summarize(total_as = sum(ln_als),
            total_ans = sum(ln_alns),
            pctg_ans = (total_ans / ( total_as + total_ans )))
head(dfc.summ)
"nse5f      total_as total_ans pctg_ans
  <fct>         <dbl>     <dbl>    <dbl>
1 Bajo         50478.    34804.    0.408
2 Medio Bajo   50752.    34345.    0.404
3 Medio        50617.    34036.    0.402
4 Medio Alto   49330.    33169.    0.402
5 Alto         46391.    31981.    0.408"

# En esa zona del país la gente dedica dos quintas partes de su gasto en alimentos a comprar alimentos no saludables (pctg_ans).
# Y en proporción sobre el gasto toal (total_as + total_ans), se puede ver que no hay diferencia entre lo que se destina a la compra
# de Alimentos No Saludables







