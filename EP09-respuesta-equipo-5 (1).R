library("tidyverse")
library("ggpubr")
library("car")
datos <- read.csv2("EP09 Datos.csv", sep= ";")

set.seed(8440)
datos <- datos %>% filter(Gender == 0)
muestra <- sample.int(n=nrow(datos) , size=50, replace=FALSE)
datos2 <- datos[muestra, ] # Muestra de 50 mujeres
prueba <- datos[-muestra, ] # Conjunto de prueba (Datos fuera de la muestra)

# Se escogen 8 variables aleatorias
datos_columnas_aleatorias <- sample(datos2[-grep("Gender", names(datos2))], 8)

# La variable distinta de las 8 anteriores que fue escogida para la realización
# del modelo de regresión lineal simple fue "Navel.Girth", el cual se refiere
# al grosor a la altura del ombligo.
# Se piensa que esta variable es útil para la predicción de la variable "Weight"
# porque generalmente una acumulacion de tejido adiposo al nivel del ombligo
# puede significar un peso mayor al habitual.

# Se realiza el modelo inicial con la variable predictora "Navel.Girth"
modelo <- lm(Weight ~ Navel.Girth, datos2)
AIC(modelo)
print(summary(modelo))

# Verificación de Condiciones:
# 1. Con p-value de 1.38e-12, menor a 0.5, se tiene que el coeficiente
# asociado a la variable predictora es distinto de 0, por lo que existe
# una relación lineal entre las variables.

shapiro.test(modelo$residuals)
# 2. Con un p-value > 0.05 se concluye que la distribución de los residuos es
# cercana a una distribución normal.

ncvTest(modelo)
# 3. Con un p-value > 0.05 se concluye que las varianzas de los residuos son
# son iguales, por lo que la variabilidad de estos es constante.

# 4. Las observaciones son independientes entre sí debido a que la toma de
# mediciones de una mujer no afecta de ninguna manera la toma de mediciones
# de otra.

# Se realiza una prueba del modelo RLS con los datos sobre mujeres no
# escogidos para la realización de este.
mse_entrenamiento <- mean(modelo$residuals**2) 
predicciones <- predict(modelo, prueba)
error <-prueba[["Weight"]] - predicciones
mse_prueba <- mean(error**2)
print(mse_entrenamiento)
print(mse_prueba)
# Se observa que el MSE de prueba baja con respecto al MSE de entrenamiento,
# por lo que (Y considerando el R cuadrado de 0.65) se concluye que este
# modelo a pesar de ser generalizable, no explica un gran porcentaje de la
# variable de respuesta.


# Con las condiciones anteriores verificadas, se procede a expandir el modelo
# a una regresión lineal multivariable, agregando variables de entre las 8
# escogidas aleatoriamente a través de una "Selección hacia adelante" hecha
# manualmente.

# Se Prueba agregar una de las 8 variables escogidas previamente
modelotest <- add1(modelo, scope = .~. + Height + Chest.Girth + Biiliac.diameter +
                     Calf.Maximum.Girth + Bicep.Girth + Bitrochanteric.diameter +
                     Ankles.diameter + Knee.Girth)
print(modelotest)

# Se encuentra que la variable que provoca un menor AIC es Knee.Girth, 
# por lo que se agrega y se repite el proceso
modelo <- update(modelo, .~. + Knee.Girth)

print(summary(modelo))

aic_1 <- AIC(modelo)
bic_1 <- BIC(modelo)
print(aic_1)
print(bic_1)

modelotest <- add1(modelo, scope = .~. + Height + Chest.Girth + Biiliac.diameter +
                     Calf.Maximum.Girth + Bicep.Girth + Bitrochanteric.diameter +
                     Ankles.diameter)
print(modelotest)

# Se encuentra que la variable que provoca un menor AIC es Chest.Girth,
# por lo que se agrega y se repite el proceso
modelo <- update(modelo, .~. + Chest.Girth)

print(summary(modelo))

aic_1 <- AIC(modelo)
bic_1 <- BIC(modelo)
print(aic_1)
print(bic_1)

modelotest <- add1(modelo, scope = .~. + Height + Biiliac.diameter +
                     Calf.Maximum.Girth + Bicep.Girth + Bitrochanteric.diameter +
                     Ankles.diameter)
print(modelotest)

# Se encuentra que la variable que provoca un menor AIC es Height,
# por lo que se agrega y se repite el proceso
modelo <- update(modelo, .~. + Height)

print(summary(modelo))

aic_1 <- AIC(modelo)
bic_1 <- BIC(modelo)
print(aic_1)
print(bic_1)

modelotest <- add1(modelo, scope = .~. + Biiliac.diameter + Calf.Maximum.Girth +
                     Bicep.Girth + Bitrochanteric.diameter + Ankles.diameter)
print(modelotest)

# Se encuentra que la variable que provoca un menor AIC es Calf.Maximum.Girth,
# por lo que se agrega y se repite el proceso
modelo <- update(modelo, .~. + Calf.Maximum.Girth)

print(summary(modelo))

aic_1 <- AIC(modelo)
bic_1 <- BIC(modelo)
print(aic_1)
print(bic_1)

modelotest <- add1(modelo, scope = .~. + Biiliac.diameter + Bicep.Girth + 
                     Bitrochanteric.diameter + Ankles.diameter)
print(modelotest)

# Se encuentra que no hay más variables que reduzcan el AIC, por lo cual se
# termina el proceso y se tiene el modelo final.

# Modelo Final:
print(summary(modelo))

# Verificación de Condiciones:
# 1. Las variables predictoras todas miden una distancia
# por lo que son numéricas.

# 2. La variable de respuesta es peso, por lo que es cuantitativa
# y continua sin restricciones para su variabilidad.

# 3. Ningún predictor es constante ya que las mediciones
# cambian de mujer a mujer.

vif(modelo)
1/vif(modelo)
# 4. Vemos que el VIF de ninguna variable predictora es mayor a 10
# por lo que no existe multicolinealidad. Aunque, se destaca que
# el VIF obtenido para la variable "Knee.Girth" es mayor a 5 y que
# la tolerancia de esta misma variable es menor a 0.2, por lo
# que la variable podría ser problemática al tener un grado de
# multicolinealidad. Con este riesgo en mente se verifican
# el resto de condiciones.

ncvTest(modelo)
# 5. Con un p-value > 0.05 se tiene que los residuos son
# homocedásticos.

shapiro.test(modelo$residuals)
# 6. Con un p-value > 0.05 se tiene que los residuos siguen
# una distribución cercana a la normal.

# 7. Los valores de la variable de respuesta son independientes
# entre sí ya que el peso de una mujer no afecta el peso de otra.

# 8. Cada variable predictora tiene un p-value < 0.5 (o muy cercano
# a este) obtenido por la prueba t, por lo que se relacionan linealmente
# con la variable de respuesta.

durbinWatsonTest(modelo)
# 9. Con un p-value > 0.05, se tiene que los residuos son 
# independientes entre sí.


# Con las condiciones verificadas, se procede a la evaluación del modelo.

# Se realiza una prueba del modelo con los datos sobre mujeres no
# escogidos para la realización del modelo
mse_entrenamiento <- mean(modelo$residuals**2) 
predicciones <- predict(modelo, prueba)
error <-prueba[["Weight"]] - predicciones
mse_prueba <- mean(error**2)

# Se puede observar que se obtuvo un mse de entrenamiento del modelo
# de 7.42 y un ms de prueba del modelo de 7.98, como este valor varía
# en menos de una unidad, podemos concluir que el modelo "Aprende"
# y no "Memoriza" por lo que puede ser generalizado.



# Podemos concluir finalmente que el modelo RLM obtenido de la expansión de un
# modelo RLS generado al inicio del código, demostró ser generalizable y 
# logró una correcta predicción del peso de 210 mujeres en base a las 
# variables predictoras asociadas a este. La expansión mencionada fue lograda
# a través de una progresiva agregación de variables predictoras, en la cual
# se escogió en cada paso la variable que generaba un AIC menor, logrando así
# una reducción del AIC de 330.5 a 256.1.
