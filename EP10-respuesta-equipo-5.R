library("tidyverse")
library("ggpubr")
library("car")
library ("pROC")
datos <- read.csv2("EP09 Datos.csv", sep= ";")
set.seed(1665)

#Se preparan los datos
datos <- datos %>% filter(Gender == 1)
index <- 1:nrow(datos)
datos <- datos %>% mutate(index, imc = as.numeric(datos$Weight) / (as.numeric(datos$Height)/100)**2)
datos <- datos %>% mutate(index, EN = case_when( datos$imc  >= 25 ~ as.numeric(1), datos$imc < 25 ~ as.numeric(0)))

#Se separan los grupos por sobrepeso y no sobrepeso
sobrepeso <- datos %>% filter(EN == 1)
no_sobrepeso <- datos %>% filter(EN == 0)
index_sample_sobrepeso <- sample.int(n=nrow(sobrepeso) , size=45, replace=FALSE)
index_sample_no_sobrepeso <- sample.int(n=nrow(no_sobrepeso) , size=45, replace=FALSE)
sample_sobrepeso <- sobrepeso[index_sample_sobrepeso, ]
sample_no_sobrepeso <- no_sobrepeso[index_sample_no_sobrepeso, ]
index_mitad_sobrepeso <- sample.int(n=nrow(sample_sobrepeso) , size=30, replace=FALSE)
index_mitad_no_sobrepeso <- sample.int(n=nrow(sample_no_sobrepeso) , size=30, replace=FALSE)

# Se obtiene la muestra de entrenamiento para la generación del modelo
entrenamiento <- sample_sobrepeso[index_mitad_sobrepeso, ]
entrenamiento <- rbind(entrenamiento, sample_no_sobrepeso[index_mitad_no_sobrepeso, ])
entrenamiento <- sample_n(entrenamiento, 60, replace = FALSE)

# Se obtiene la muestra de prueba para evaluar la generabilidad del modelo.
prueba <- sample_sobrepeso[-index_mitad_sobrepeso, ]
prueba <- rbind(prueba, sample_no_sobrepeso[-index_mitad_no_sobrepeso, ])
prueba <- sample_n(prueba, 30, replace = FALSE)

#Se recuerdan del ejercicio anterior las variables:
#Chest.Girth + Biiliac.diameter + Ankles.diameter + Knee.Girth
#Calf.Maximum.Girth + Bicep.Girth + Bitrochanteric.diameter + Height
#Dado que aleatoriamente en el ejercicio anterior salió Height, este se reemplazará por otra aleatoria
#Sorteado: Age.


# Se construye el modelo inicial con la variable "Navel.Girth", el cual se refiere
# al grosor a la altura del ombligo.
# Se piensa que esta variable es útil para la predicción de la variable "Weight"
# porque generalmente una acumulación de tejido adiposo al nivel del ombligo
# puede significar que la persona presenta sobrepeso.

modelo <- glm(EN ~ Navel.Girth, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo))


# Se Prueba agregar una de las 8 variables escogidas previamente
modelotest <- add1(modelo, scope = .~. + Age + Chest.Girth + Biiliac.diameter +
                     Calf.Maximum.Girth + Bicep.Girth + Bitrochanteric.diameter +
                     Ankles.diameter + Knee.Girth)
print(modelotest)

modelo <- update(modelo, .~. + Calf.Maximum.Girth)

print(summary(modelo))

# Se encuentra que la variable que provoca un menor AIC es Knee.Girth, 
# por lo que se agrega y se repite el proceso

modelotest <- add1(modelo, scope = .~. + Age + Chest.Girth + Biiliac.diameter + Bicep.Girth + Bitrochanteric.diameter +
                     Ankles.diameter + Knee.Girth)
print(modelotest)

modelo <- update(modelo, .~. + Chest.Girth)

print(summary(modelo))

# Se encuentra que la variable que provoca un menor AIC es Chest.Girth, 
# por lo que se agrega y se repite el proceso

modelotest <- add1(modelo, scope = .~. + Age + Biiliac.diameter + Bicep.Girth + Bitrochanteric.diameter +
                     Ankles.diameter + Knee.Girth)
print(modelotest)

modelo <- update(modelo, .~. + Age)

print(summary(modelo))

# Se encuentra que la variable que provoca un menor AIC es Age, 
# por lo que se agrega y se repite el proceso


modelotest <- add1(modelo, scope = .~. + Biiliac.diameter + Bicep.Girth + Bitrochanteric.diameter +
                     Ankles.diameter + Knee.Girth)
print(modelotest)

# Se encuentra que no hay más variables que reduzcan el AIC, por lo cual se
# termina el proceso y se tiene el modelo final.


# Verificación de Condiciones:
#1. Se verifica la colinealidad y tolerancia del modelo.
VIF <- vif(modelo)
print(VIF)
print(1/VIF)
# Se puede observar que ningún vif supera a 10 y ninguna tolerancia es menor a 0.2

#2. Se verifica 
durbinWatsonTest(modelo , max.lag = 5)
# Con un p-value > 0.05, se tiene que los residuos son 
# independientes entre sí.

#3. Se verifica que las variables predictoras tengan una relación lineal}
# con la variable de respuesta.
print(summary(modelo))
# Se puede observar que en su mayoría las variables predictoras se relacionan
# linealmente con la variable de respuesta, pero se destaca que la variable
# "Navel.Girth" presenta un p-value mayor a 0.1, por lo que esta no presenta
# una relación enteramente lineal. Teniendo en cuenta los riesgos asociados
# a esto, se continua con la evaluación del modelo.


# Se evalúa el modelo con el conjunto de prueba
probs_e <- predict(modelo, prueba, type = "response")
preds_e <- sapply(probs_e , function(p)ifelse (p >= 0.5, "1","0"))
ROC_e <- roc(prueba[["EN"]] , probs_e)
plot(ROC_e)

# Ya que la curva ROC se aleja bastante de una recta se concluye que
# el modelo realiza su función como clasificador y es generalizable.
