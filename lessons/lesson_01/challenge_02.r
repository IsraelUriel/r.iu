## RETO 02: LECTURA DE DATAFRAMES

"En la gran mayoría de las sesiones estaremos utilizando distintas tablas de datos,
por lo que es importante a prender a leerlas.

Como recordarás, el formato .csv es uno de los más utilizados, por lo que nos 
enfocaremos en ello, sin embargo, la mayoría de funciones para leer archivos 
siguen la misma lógica.

La función read.csv() toma un argumento principal, que establece la ruta de sistema 
o url al archivo .csv.

Si vas a leer un archivo que se encuentra localmente en tu computadora, es importante
que este se encuentre en la dirección actual de trabajo, la cual puedes consultar 
con la función getwd(). Para cambiar la dirección de trabajo puedes usar la función 
setwd(path)"

"A) Usando la función de lectura adecuada, lee el archivo que se encuentra en la 
siguiente url"
# local file:
# Set directory to working folder
# setwd(path)
netflix <- read.csv(paste(getwd(),"netflix_titles.csv",sep = "/"))

# remote file:


"B) Verifica la clase de la tabla, el número de variables y de observaciones, así 
como el nombre de las variables"
class(netflix)
str(netflix)
names(netflix)
head(netflix)
dim(netflix)

"C) Usando indexación, selecciona sólo los datos de películas que fueron estrenadas 
desde 2015 y hasta antes del 2019"

net.2015 <- netflix[netflix$release_year >= 2015 & netflix$release_year < 2019,]
# Ordenar Datos por año de estreno:
net.2015 = net.2015[order(net.2015$release_year),]

"D) Por último, averigua que hace la siguiente función"
# Mostrar vista de un dataframe en RStudio
View(net.2015)
