library("tidyverse")
library("ggpubr")
library("leaps")
library("caret")
library("car")
library ("pROC")

set.seed(51665)

datos <- read.csv2("EP09 Datos.csv", sep= ";")
index <- 1:nrow(datos)
#Se crean las columnas imc y EN
datos <- datos %>% mutate(index, imc = as.numeric(datos$Weight) / (as.numeric(datos$Height)/100)**2)
datos <- datos %>% mutate(index, EN = case_when( datos$imc  >= 25 ~ as.numeric(1), datos$imc < 25 ~ as.numeric(0)))
#Se preparan los datos para extraer 50 y 50 de cada grupo.
sobrepeso <- datos %>% filter(EN == 1)
no_sobrepeso <- datos %>% filter(EN == 0)
index_sample_sobrepeso <- sample.int(n=nrow(sobrepeso) , size=50, replace=FALSE)
index_sample_no_sobrepeso <- sample.int(n=nrow(no_sobrepeso) , size=50, replace=FALSE)
sample_sobrepeso <- sobrepeso[index_sample_sobrepeso, ]
sample_no_sobrepeso <- no_sobrepeso[index_sample_no_sobrepeso, ]
#Se prepara el dataframe y se desordena.
muestra <- rbind(sample_sobrepeso, sample_no_sobrepeso)
muestra <- sample_n(muestra, 100, replace = FALSE)
#Se obtiene un conjunto de prueba
sample_prueba_s <- sobrepeso[-index_sample_sobrepeso, ]
sample_prueba_ns <- no_sobrepeso[-index_sample_no_sobrepeso, ]
conjunto_de_prueba <- rbind(sample_prueba_s, sample_prueba_ns)
conjunto_de_prueba <- sample_n(conjunto_de_prueba, 407, replace = FALSE)
#Preparación 
predictores <- subset(muestra, select = -c(EN, imc, Weight, index, Height))
pesos <- muestra %>% pull(Weight)


# A través del paquete "leaps", se crea un modelo RLM.
formula <- as.formula(paste("Weight ~", paste(names(predictores), collapse = "+")))
modelo <- regsubsets(formula, data = muestra, nbest = 1, nvmax = 8)
plot(modelo)

# El modelo anterior se evalúa con bootstrapping de 100 repeticiones
control <- trainControl(method = "boot", number = 100)  # Bootstrapping con 100 repeticiones
modelLeaps <- train(Weight ~ Biacromial.diameter + Chest.depth + Knees.diameter 
               + Chest.Girth + Hip.Girth + Calf.Maximum.Girth + Age
               , data = muestra, method = "lm", trControl = control)

print(summary(modelLeaps))

# Verificación de Condiciones:
# 1. Las variables predictoras que miden una distancia
# son numéricas y la variable "Gender" es dicótomica.

# 2. La variable de respuesta es peso, por lo que es cuantitativa
# y continua sin restricciones para su variabilidad.

# 3. Ningún predictor es constante ya que las mediciones
# cambian de persona a persona.

vif(modelLeaps$finalModel)
1/vif(modelLeaps$finalModel)
# 4. Vemos que el VIF de ninguna variable predictora es mayor a 10
# por lo que no existe multicolinealidad. Además, ninguna tolerancia
# baja de 0.5, por lo que no existe un sesgo significativo en el modelo.

ncvTest(modelLeaps$finalModel)
# 5. Con un p-value < 0.05 se tiene que los residuos son
# heterocedásticos. A pesar de que la presencia
# de diferentes varianzas entre los residuos puede entorpecer
# la interpretación y confianza del modelo, se continuará
# de todos modos con el riesgo en mente.

shapiro.test(modelLeaps$finalModel$residuals)
# 6. Con un p-value > 0.05 se tiene que los residuos siguen
# una distribución cercana a la normal.

# 7. Los valores de la variable de respuesta son independientes
# entre sí ya que el peso de una persona no afecta el peso de otra.

# 8. Cada variable predictora tiene un p-value < 0.5
# obtenido por la prueba t, por lo que se relacionan linealmente
# con la variable de respuesta.

durbinWatsonTest(modelLeaps$finalModel)
# 9. Con un p-value > 0.05, se tiene que los residuos son 
# independientes entre sí.


# Con las condiciones verificadas, se procede a la evaluación del modelo.

# Se realiza una prueba del modelo con los datos sobre personas no
# escogidos para la realización del modelo
mse_entrenamiento <- mean(modelLeaps$finalModel$residuals**2) 
predicciones <- predict(modelLeaps, conjunto_de_prueba)
error <-conjunto_de_prueba[["Weight"]] - predicciones
mse_prueba <- mean(error**2)
mse_entrenamiento
mse_prueba

# Se puede observar que se obtuvo un mse de entrenamiento del modelo
# de 7.67 y un ms de prueba del modelo de 12.38, vemos que el MSE no
# varía una gran cantidad del original, por lo que se considerara
# aceptable y se concluye que el modelo es suficientemente generalizable.

# Se procede a obtener el modelo RLM a través de rfe.
controlRfe <- rfeControl(functions = lmFuncs,
                         method = "repeatedcv",
                         repeats = 5,
                         number = 5,
                         verbose = FALSE,
                         saveDetails = TRUE)
controlRfe$functions$selectSize <- pickSizeTolerance

lmRfe <- rfe(x = predictores, 
             y = pesos, 
             metric = "Rsquared",
             sizes = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
             rfeControl = controlRfe)
predictors(lmRfe)

# A través del procedimiento realizado por rfe, se obtuvieron los siguientes
# predictores: 
# Gender + Wrists.diameter + Knees.diameter + 
# Wrist.Minimum.Girth + Biacromial.diameter + Elbows.diameter + 
#  Chest.depth + Ankles.diameter + Chest.Girth + Forearm.Girth + Thigh.Girth

modelo_lmRfe <- lm(Weight ~ Gender + Wrists.diameter + Knees.diameter + 
                   Wrist.Minimum.Girth + Biacromial.diameter + Elbows.diameter + 
                   Chest.depth + Ankles.diameter + Chest.Girth + 
                   Forearm.Girth + Thigh.Girth, muestra)
print(summary(modelo_lmRfe))

# Verificación de Condiciones:
# 1. Las variables predictoras que miden una distancia
# son numéricas y la variable "Gender" es dicotómica.

# 2. La variable de respuesta es peso, por lo que es cuantitativa
# y continua sin restricciones para su variabilidad.

# 3. Ningún predictor es constante ya que las mediciones
# cambian de persona a persona.

vif(modelo_lmRfe)
1/vif(modelo_lmRfe)
# 4. Vemos que el VIF de la variable "Forearm.Girth" supera 10
# y las tolerancias de la mitad de los predictores es menor
# a 0.2, por lo que existe una colinealidad entre predictores
# significativamente problemática. Teniendo en mente que
# los valores de vif y tolerancia obtenidos pueden provocar
# estimaciones poco confiables de los parámetros correlacionados
# se continuará con la verificación de condiciones.

ncvTest(modelo_lmRfe)
# 5. Con un p-value < 0.05 se tiene que los residuos son
# heterocedásticos. A pesar de que la presencia
# de diferentes varianzas entre los residuos puede entorpecer
# la interpretación y confianza del modelo, se continuará
# de todos modos con el riesgo en mente.

shapiro.test(modelo_lmRfe$residuals)
# 6. Con un p-value > 0.05 se tiene que los residuos siguen
# una distribución cercana a la normal.

# 7. Los valores de la variable de respuesta son independientes
# entre sí ya que el peso de una persona no afecta el peso de otra.

# 8. Con p-values > 0.05 se tiene que existen variables predictoras
# que no presentan una relación enteramente lineal con la variable 
# de respuesta.

durbinWatsonTest(modelo_lmRfe)
# 9. Con un p-value > 0.05, se tiene que los residuos son 
# independientes entre sí.


# Con las condiciones verificadas, se procede a la evaluación del modelo.

# Se realiza una prueba del modelo con los datos sobre personas no
# escogidos para la realización del modelo
mse_entrenamiento <- mean(modelo_lmRfe$residuals**2) 
predicciones <- predict(modelo_lmRfe, conjunto_de_prueba)
error <-conjunto_de_prueba[["Weight"]] - predicciones
mse_prueba <- mean(error**2)
mse_entrenamiento
mse_prueba

# Se puede observar que se obtuvo un mse de entrenamiento del modelo
# de 8.59 y un ms de prueba del modelo de 15.89, vemos que el MSE
# varía moderadamente del original, por lo que se considerara
# no aceptable y se considera el modelo como no generalizable.

# Se procede finalmente, a obtener el modelo de Regresión Logística con rfe.
muestra[["EN"]] <- as.factor(muestra[["EN"]])
EN <- muestra[["EN"]]
muestra_final <- muestra %>% select(-c(Weight, Height, imc, index, EN))

lrFuncs$summary <- twoClassSummary

myControl <- trainControl(
  method = "LOOCV",
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)
myControlRfe <- rfeControl(functions = lrFuncs, method = "LOOCV")

glmRfe <- rfe(muestra_final, EN, metric = "ROC", rfeControl = myControlRfe,
             trControl = myControl, sizes = 2:6)

predictors(glmRfe)

# A través del procedimiento realizado por rfe, se obtuvieron los siguientes
# predictores: 
# "Chest.diameter", "Waist.Girth", "Forearm.Girth", "Gender" y "Hip.Girth". 

modeloGlm <- glm(EN ~ Chest.diameter + Waist.Girth + 
                   Forearm.Girth + Gender + Hip.Girth,
                 family = "binomial"(link = "logit"),
                 data = muestra)
print(summary(modeloGlm))

# Verificación de Condiciones:
# 1. Las variables predictoras que miden una distancia
# son numéricas y la variable "Gender" es dicotómica.

# 2. La variable de respuesta es peso, por lo que es cuantitativa
# y continua sin restricciones para su variabilidad.

# 3. Ningún predictor es constante ya que las mediciones
# cambian de persona a persona.

vif(modeloGlm)
1/vif(modeloGlm)
# 4. Vemos que el VIF de ninguna variable predictora es mayor a 10
# por lo que no existe multicolinealidad. Aunque se destaca que
# las variables predictoras "Gender" y "Forearm.Girth" presentan
# una tolerancia menor a 0.2, por lo existen riesgos de presencia
# de sesgo en el modelo. Se procede con este riesgo en mente.

# 5. Los valores de la variable de respuesta son independientes
# entre sí ya que el estado nutricional
# de una persona no afecta el estado nutricional de otra.

# 6. Con p-values > 0.05 se tiene que existen variables predictoras
# que no presentan una relación enteramente lineal con la variable 
# de respuesta.

durbinWatsonTest(modeloGlm)
# 7. Con un p-value > 0.05, se tiene que los residuos son 
# independientes entre sí.


# Con las condiciones verificadas, se procede a la evaluación del modelo.

# Se evalúa el modelo con el conjunto de prueba
probs_e <- predict(modeloGlm, conjunto_de_prueba, type = "response")
preds_e <- sapply(probs_e , function(p)ifelse (p >= 0.5, "1","0"))
ROC_e <- roc(conjunto_de_prueba[["EN"]] , probs_e)
plot(ROC_e)

# Ya que la curva ROC se aleja bastante de una recta se concluye que
# el modelo realiza su función como clasificador y es generalizable.


# Podemos concluir, que en base a la verificación de condiciones
# de los 3 modelos y sus respectivas evaluaciones que el primer
# modelo realizado con leaps y el último modelo de regresión
# logística con rfe son los únicos modelos generalizables, mientras
# que el segundo modelo de regresión lineal múltiple con rfe
# por su gran diferencia de MSE presente en la evaluación de este
# y por la falta de cumplimiento de una variedad de condiciones
# no puede ser generalizable.
