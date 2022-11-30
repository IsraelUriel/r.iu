production <- read.table('production.txt', header = TRUE)

head(production, 10)
str(production)
dim(production)

# To be able to manipulate the Data Frame with the names of the columns.
attach(production)

plot(RunTime, RunSize,
     xlab = 'Tamaño de Ejecución',
     ylab = 'Tiempo de Ejecución',
     pch = 16)

m1 = lm(RunTime ~ RunSize)

summary(m1)

plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
     ylab = "Tiempo de ejecución", pch = 16)
abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresión estimada
mtext(expression(paste('Modelo de regresión lineal simple:',
                       ' ',
                       y[i] == beta[0] + beta[1]*x[i] + e[i])),
      side = 3, adj=1, font = 2)

# Recta de regresión poblacional

text(x = 200, y = 240, expression(paste('Recta de regresión:',
                                        ' ',
                                        y[i] == beta[0] + beta[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresión estimada

text(x = 350, y = 180, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresión estimada

text(x = 350, y = 160, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == 149.74770 + 0.25924*x[i])),
     adj = 1, font = 2)

# Residuales

points(189, 215, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 189 # Valor y sobre la recta estimada
## [1] 198.7441
lines(c(189, 189), c(198.7441, 215), col = "red")

points(173, 166, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 173 # Valor y sobre la recta estimada
## [1] 194.5962
lines(c(173, 173), c(166, 194.5962), col = "red")


  
  
  round(confint(m1, level = 0.95), 3)
  ##               2.5 %  97.5 %
  ## (Intercept) 132.251 167.244
  ## RunSize       0.181   0.337
  
  
  
  RunSize0 <- c(50,100,150,200,250,300,350) # Algunos posibles valores de RunSize
  
  (conf <- predict(m1, newdata = 
                     data.frame(RunSize = RunSize0), 
                   interval = "confidence", level = 0.95))
  ##        fit      lwr      upr
  ## 1 162.7099 148.6204 176.7994
  ## 2 175.6720 164.6568 186.6872
  ## 3 188.6342 179.9969 197.2714
  ## 4 201.5963 193.9600 209.2326
  ## 5 214.5585 206.0455 223.0714
  ## 6 227.5206 216.7006 238.3407
  ## 7 240.4828 226.6220 254.3435
  # Podemos visualizar gráficamente estos intervalos de confianza
  
  plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
       ylab = "Tiempo de ejecución", pch = 16)
  abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresión estimada
  
  lines(RunSize0, conf[, 2], lty = 2, lwd = 2, col = "green") # límites inferiores
  lines(RunSize0, conf[, 3], lty = 2, lwd = 2, col = "green") # límites superiores

  
  
  
  (pred <- predict(m1, newdata = 
                     data.frame(RunSize = RunSize0), 
                   interval = "prediction", level = 0.95))
  ##        fit      lwr      upr
  ## 1 162.7099 125.7720 199.6478
  ## 2 175.6720 139.7940 211.5500
  ## 3 188.6342 153.4135 223.8548
  ## 4 201.5963 166.6076 236.5850
  ## 5 214.5585 179.3681 249.7489
  ## 6 227.5206 191.7021 263.3392
  ## 7 240.4828 203.6315 277.3340
  # Podemos visualizar gráficamente estos intervalos de predicción
  
  plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
       ylab = "Tiempo de ejecución", pch = 16)
  abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresión estimada
  
  lines(RunSize0, conf[, 2], lty = 2, lwd = 2, col = "green") # límites inferiores
  lines(RunSize0, conf[, 3], lty = 2, lwd = 2, col = "green") # límites superiores
  
  lines(RunSize0, pred[, 2], lty = 2, lwd = 2, col = "blue") # límites inferiores
  lines(RunSize0, pred[, 3], lty = 2, lwd = 2, col = "blue") # límites superiores
  
  
  
  dev.off()
  ## null device 
  ##           1
  
  
  
  library('mlbench')

  
  model <- glm(diabetes ~ glucose, data = train.data, family = binomial)
  summary(model)$coef
  ##             Estimate Std. Error z value Pr(>|z|)
  ## (Intercept)  -6.3267     0.7241   -8.74 2.39e-18
  ## glucose       0.0437     0.0054    8.09 6.01e-16
  
  
