
# Notes: ------------------------------------------------------------------

# Objetivo
# El Postwork tiene como objetivo que practiques los comandos básicos aprendidos 
# durante la sesión, de tal modo que sirvan para reafirmar el conocimiento. 
# Recuerda que la programación es como un deporte en el que se debe practicar, 
# habrá caídas, pero lo importante es levantarse y seguir adelante. Éxito
# 
# Requisitos
# Concluir los retos
# Haber estudiado los ejemplos durante la sesión
# Desarrollo
# El siguiente postwork, te servirá para ir desarrollando habilidades como si se 
# tratara de un proyecto que evidencie el progreso del aprendizaje durante el módulo, 
# sesión a sesión se irá desarrollando. A continuación aparecen una serie de objetivos 
# que deberás cumplir, es un ejemplo real de aplicación y tiene que ver con datos 
# referentes a equipos de la liga española de fútbol (recuerda que los datos provienen 
# siempre de diversas naturalezas), en este caso se cuenta con muchos datos que se pueden 
# aprovechar, explotarlos y generar análisis interesantes que se pueden aplicar a otras áreas. 
# Siendo así damos paso a las instrucciones:
#   
# Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga 
# española a R, los datos los puedes encontrar en el siguiente enlace: 
# https://www.football-data.co.uk/spainm.php
# 
# Del data frame que resulta de importar los datos a R, extrae las columnas que 
# contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los 
# goles anotados por los equipos que jugaron como visitante (FTAG)
# 
# Consulta cómo funciona la función table en R al ejecutar en la consola ?table
# 
# Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
#   
# La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
# La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
# La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que 
# juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
# Notas para los datos de soccer: https://www.football-data.co.uk/notes.txt


# Challenge: --------------------------------------------------------------

# Importar Datos de la Liga Española 2019 - 2022 Primera División
spanish_league <- read.csv(paste(getwd(),'SP1.csv', sep = '/'))

# Extraer columnas en respectivo vector
FTHG <- c(spanish_league$FTHG)
FTAG <- c(spanish_league$FTAG)

# Tabla de frecuencia 
# ?table
table_F_H <- table(FTHG)
table_F_H = t(table_F_H)
rownames(table_F_H) <- c("Home")
table_F_H

table_F_A <- table(FTAG)
table_F_A <- t(table_F_A)
rownames(table_F_A) <- c("Away")
table_F_A

# La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
prop.table(table_F_H, margin=NULL)

# La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
prop.table(table_F_A, margin=NULL)

# La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que 
# juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

table_F_AH <- table(FTHG,FTAG)
table_F_AH = prop.table(table_F_AH, margin=NULL)
table_F_AH = addmargins(table_F_AH * 100)

table_F_AH

# Ver Datos
View(spanish_league)
