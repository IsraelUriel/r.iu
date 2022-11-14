
"Imaginemos que tenenos una columna de datos con información del sexo de nuestros
clientes:"
  
clientes <- c("M", "H", "NA", "M", "M", "H", "NA", "M", "H",
              "M", "M", "M", "H", "M", "H", "H", "NA", "M",
              "NA", "NA", "M", "H", "NA", "M", "M", "H", "H",
              "M", "H", "H", "H", "M", "NA", "H", "M", "M")

"Para comenzar a hacer análisis y calcular algunos estadísticos, necesitamos contar 
cuántos clientes son mujeres y hombres, así como aquellos con los que aún no contamos información.
Utilizando loops y pseudocódigo, desarrolla una forma rápida de lograr el objetivo mencionado."

clientes.H <- 0
clientes.M <- 0
clientes.NA <- 0
for (i in 1:length(clientes)) {
  if (clientes[i] == 'H') {
    clientes.H = clientes.H + 1
  } else if (clientes[i] == 'M') {
    clientes.M = clientes.M + 1
  } else {
    clientes.NA = clientes.NA + 1
  }
}

print(paste('Hombres:', clientes.H))
print(paste('Mujeres:', clientes.M))
print(paste('N/A:', clientes.NA))
