
#---MODELOS NO LINEALES---
#Este script sireva para realizar dos tipos de modelos no lineales:
# Regresión polinomial: extensión del modelo lineal mediante la incorporación de 
# predictores extra, obtenidos elevando a distintas potencias los predictores originales
#Permite ajustar una curva extremadamente no lineal

#Splines de regresión: Consiste en dividir el rango de X en K intervalos 
#distintos, y en cada intervalo se ajusta una función polinomial, restringidas para que estén
#unidas por los límites de cada intervalo, produce un ajuste muy flexible.

#Posteriormente se compararn estos dos modelos y se determinará cual es
#el que mejor se ajusta a los datos de ER de los genes ARF8 y TIR1 durante 
#la TFF de Vanilla planifolia

#librerias:

#library(ggplot2) Permite realizar gráficos complejos y de alta calidad empleando códigos sencillos
# library(tidyr) Permite transformar un data frame a la estructura que deseemos.
#library(dplyr) proporciona una "gramática"para la manipulación y operaciones con data frames
#library(magrittr) Ayuda a hacer más fluido el proceso de análisis de datos.
# library(boot) Ofrece facilidades para métodos de remuestreo
#library(splines) Regresión por Splines 


#---Para ARF8---

#Abrir el archivo
M_no_lineal_ARF8<- read.table(file= "ARF8.csv", header = TRUE, sep = ",")
M_no_lineal_ARF8

#Se graficaran los datos en ggplot2 para corroborar que a relación entre 
#la fase del desarrollo y la expresión realtiva no es lineal

library(ggplot2)

ggplot(M_no_lineal_ARF8,
       aes(x=Fase_desarrollo,
           y=ER))+
  geom_point() +
  theme_classic()
        

#Se dividirá el set de datos en un conjunto de entrenamiento (80%) y test (20%)

set.seed(1)
trainARF8 <- sample(nrow(M_no_lineal_ARF8),
                    0.8*nrow(M_no_lineal_ARF8),
                    replace = FALSE
)


#Como fase del desarrollo no es una variable numerica se ocupan los dias para agrupar las clases
# para tomar los numeros de las clases y no el valor de clase

library(tidyr)
library(dplyr)
library(magrittr)

datosA.train_ARF8 <- datosA.train_ARF8 %>%
  separate(Fase_desarrollo, #<- Extraer variable numerica de fase del desarrollo
           into=c("Fase_desarrollo"),
           sep=" ") %>%
  mutate(Fase_desarrollo=as.numeric(Fase_desarrollo)#<- convertir a numeros
)

#---Ajuste del modelo---

#Selección del mejor grado de polinomio mediante validación cruzada
#Se recurre a la a la validación cruzada (10-fold cross validation) 
#para escoger el valor óptimo en función del error de validación

library(boot)

# Se realiza un Vector para almacenar el error de validación de cada polinomio
cv.error <- rep(NA, 10)

# Vector para almacenar el RSS (suma de residuos al cuadrado) de cada polinomio
rss <- rep (NA, 10)
for (i in 1:7){
  modelo.poli_ARF8 <- glm(ER ~ poly(Fase_desarrollo, i), data = datosA.train_ARF8)
  set.seed(2)
  cv.error[i] <- cv.glm(datosA.train_ARF8,modelo.poli_ARF8, K = 10)$delta[1]
  rss[i] <- sum(modelo.poli_ARF8$residuals^2)
}

ggplot(data = data.frame(polinomio = 1:10, cv.error = cv.error),
       aes(x = polinomio, y = cv.error)) +
  geom_point(color = "orangered2") +
  geom_path() +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "cv.MSE  ~ Grado de polinomio") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = data.frame(polinomio = 1:10, RSS = rss),
       aes(x = polinomio, y = RSS)) +
  geom_point(color = "orangered2") +
  geom_path() +
  scale_x_continuous(breaks=0:10) +
  labs(title = "RSS  ~ Grado de polinomio") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#El RSS disminuye conforme aumenta el grado del polinomio,
#El modelo se ajusta cada vez más a los datos.

#---Polinomio con menor error de validación---

#Se ajustó el modelo con el valor de polinomio 2 para evitar sobreajustar el modelo a los datos

modelo.poli_ARF8 <- lm(ER ~ poly(Fase_desarrollo, which.min(cv.error)),
                       data = datosA.train_ARF8)

summary(modelo.poli_ARF8)

#Todos los grados de polinomio resultan ser significativos en el ajuste del modelo

#---Grafica del modelo---
ggplot(data = datosA.train_ARF8, aes(x = Fase_desarrollo, y = ER)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = TRUE,
              level = 0.95) +
  labs(title = "Polinomio grado 2:ER ~ Fase del desarrollo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)
)

#----Selección del mejor grado de polinomio mediante ANOVA---

#Para seleccionar el mejor modelo, de un conjunto con distinto grado de polinomio,
#Se contrasta la hipótesis ANOVA (test F).
#Se evalúa la hipótesis nula de que el modelo M1 es suficiente para explicar los datos
#frente a la hipótesis alternativa de que un modelo M2 es mejor.
#los modelos se anidan

modelo1 <- lm(ER ~ Fase_desarrollo, data = datosA.train_ARF8)

modelo2 <- lm(ER ~ poly(Fase_desarrollo, 2),
              data = datosA.train_ARF8)

modelo3 <- lm(ER ~ poly(Fase_desarrollo, 3),
              data = datosA.train_ARF8)

anova(modelo1, modelo2, modelo3)

#---Evaluación del modelo---
pred.modelo.poli_ARF8 <-  predict(modelo2)
test.error.poli_ARF8 <- mean((pred.modelo.poli_ARF8 - datosA.test_ARF8$ER)^2)
test.error.poli_ARF8

# El contraste de ANOVA muestra que el modelo polinómico de grado 2 es superior al modelo lineal.


#---Spline de regresión---

library(splines)

#---Ajuste del modelo---

#----Selección de los grados de libertad mediante validación cruzada---
#Se empleara el método de 10-fold cross validation para encontrar el valor 
#óptimo de grados de libertad, probando valores de entre 3 y 16. 

# Vector donde se almacenarán los errores de validaci?n
cv.error <- rep(NA, 16)

# K-fold cross validation para cada valor de df
for(i in 2:16){
  modelo.spline_ARF8 <- glm(ER ~ bs(Fase_desarrollo, degree = 2, df = i),
                            data = datosA.train_ARF8)
  set.seed(3)
  cv.error[i] = cv.glm(datosA.train_ARF8,
                       modelo.spline_ARF8, K = 10)$delta[1]
}

# El primer valor del vector cv.error son NA ya que no se ha evaluado
head(cv.error)

ggplot(data = data.frame(grados_libertad = 2:16, cv.error = cv.error[-1]),
       aes(x = grados_libertad, y = cv.error)) +
  geom_point(color = "orangered2") +
  geom_path() +
  scale_x_continuous(breaks=3:16) +
  labs(title = "cv.MSE  ~ Grados de libertad") +
  theme_bw() +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)
)

which.min(cv.error)

modelo.spline_ARF8 <- lm(ER ~ bs(Fase_desarrollo, degree = 2, df = 6),
                         data = datosA.train_ARF8)

summary(modelo.spline_ARF8)

# SPLINE
ggplot(data = datosA.train_ARF8, aes(x = Fase_desarrollo, y = ER)) +
  geom_point(col = "darkgrey") +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 2, df = 6),
              color = "red", se = TRUE, level = 0.95) +
  labs(title = "Spline con df = 6: ER ~ Fase_desarrollo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)
)

# SPLINE NATURAL (por defecto, cúbico)
ggplot(data = datosA.train_ARF8, aes(x = Fase_desarrollo, y = ER)) +
  geom_point(col = "darkgrey") +
  geom_smooth(method = "lm", formula = y ~ ns(x, df = 6), color = "black",
              se = TRUE, level = 0.95) +
  labs(title = "Natural spline con df = 6: ER ~ Fase_desarrollo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)
)

# SPLINE DE SUAVIZADO
modelo.spline.s_ARF8 <- smooth.spline(x = datosA.train_ARF8$Fase_desarrollo,
                                      y = datosA.train_ARF8$ER, cv = TRUE)

# Grados de libertad y lambda correspondientes al modelo ajustado
modelo.spline.s_ARF8$df

modelo.spline.s_ARF8$lambda

plot(ER ~ Fase_desarrollo, data = datosA.train_ARF8,
     col = "darkgrey") 
title("Smoothing spline (df = 4,65)")
lines(modelo.spline.s_ARF8, col = "darkgreen", lwd = 2)

#----Evaluación del modelo---
pred.modelo.spline_ARF8 <-  predict(modelo.spline.s_ARF8)
test.error.spline_ARF8 <- mean((pred.modelo.spline_ARF8$y - datosA.test_ARF8$ER)^2)
test.error.spline_ARF8

#Comparación de modelos

modelo <- c("Polinómico (d = 2)", "B-Spline")
test.MSE <- c(test.error.poli_ARF8, test.error.spline_ARF8)
comparacion <- data.frame(modelo = modelo, test.MSE = test.MSE)

ggplot(data = comparacion, aes(x = reorder(x = modelo, X = test.MSE), 
                               y = test.MSE, color = modelo, 
                               label = round(test.MSE,2))) + 
  geom_point(size = 8) + 
  geom_text(color = "white", size = 2) + 
  labs(x = "Modelo regresión", y = "Test error(MSE)") + theme_bw() + 
  coord_flip() +
  theme(legend.position = "none")

#EL MODELO POLINOMICO ES EL QUE MEJOR SE AJUSTA A LOS DATOS


#---Para TIR1---

#Abrir el archivo
M_no_lineal_TIR1<- read.table(file = choose.files(), header = TRUE, sep = ",")
M_no_lineal_TIR1


ggplot(M_no_lineal_TIR1, aes(x=Fase_desarrollo, y=ER))+
  geom_point() +
  theme_bw() +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)
)

#Se dividirá el set de datos en un conjunto de entrenamiento (80%) y test (20%)

set.seed(1)

trainATIR1 <- sample(nrow(M_no_lineal_TIR1),
                     0.8*nrow(M_no_lineal_TIR1), replace = FALSE)

datosA.train_TIR1 <- M_no_lineal_TIR1[trainATIR1, ]
datosA.test_TIR1 <- M_no_lineal_TIR1[-trainATIR1, ]

nrow(datosA.train_TIR1) + nrow(datosA.test_TIR1)

#Como fase del desarrollo no es una variable numerica se van a ocupar los dias para agrupar las clases
# para tomar los numeros de las clases y no el valor de clase

datosA.train_TIR1 <- datosA.train_TIR1 %>%
  separate(Fase_desarrollo, into=c("Fase_desarrollo"),
           sep=" ") %>% #<- Extraer variable numerica de fase del desarrollo
  mutate(Fase_desarrollo=as.numeric(Fase_desarrollo) #<- convertir a numeros
)

#---Ajuste del modelo---

#Selección del mejor grado de polinomio mediante validación cruzada
#Se recurre a la a la validación cruzada (10-fold cross validation) 
#para escoger el valor óptimo en función del error de validación

# Se realiza un Vector para almacenar el error de validación de cada polinomio
cv.error <- rep(NA, 10)

# Vector para almacenar el RSS de cada polinomio
rss <- rep (NA, 10)
for (i in 1:7){
  modelo.poli_TIR1 <- glm(ER ~ poly(Fase_desarrollo, i),data = datosA.train_TIR1)
  set.seed(2)
  cv.error[i] <- cv.glm(datosA.train_TIR1, modelo.poli_TIR1, K = 10)$delta[1]
  rss[i] <- sum(modelo.poli_TIR1$residuals^2)
}

ggplot(data = data.frame(polinomio = 1:10, cv.error = cv.error),
       aes(x = polinomio, y = cv.error)) +
  geom_point(color = "orangered2") +
  geom_path() +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "cv.MSE  ~ Grado de polinomio") +
  theme_bw() +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)
)

ggplot(data = data.frame(polinomio = 1:10, RSS = rss), aes(x = polinomio,
                                                           y = RSS)) +
  geom_point(color = "orangered2") +
  geom_path() +
  scale_x_continuous(breaks=0:10) +
  labs(title = "RSS  ~ Grado de polinomio") +
  theme_bw() +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)
)

#El RSS disminuye conforme aumenta el grado del polinomio,
#El modelo se ajusta cada vez más a los datos.

# Polinomio con menor error de validación
which.min(cv.error)


#Se ajustó el modelo con el valor de polinomio 2 para evitar sobreajustar el modelo a los datos

modelo.poli_TIR1 <- lm(ER ~ poly(Fase_desarrollo,
                                 which.min(cv.error)),data = datosA.train_TIR1)
summary(modelo.poli_TIR1)

#Todos los grados de polinomio resultan ser significativos en el ajuste del modelo

#---Grafica del modelo---
ggplot(data = datosA.train_TIR1, aes(x = Fase_desarrollo, y = ER)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = TRUE,
              level = 0.95) +
  labs(title = "Polinomio grado 2:ER ~ Fase del desarrollo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)
)

#----Selección del mejor grado de polinomio mediante ANOVA---

#Para seleccionar el mejor modelo, de un conjunto con distinto grado de polinomio,
#Se contrasta la hipótesis ANOVA (test F).
#Se evalúa la hipótesis nula de que el modelo M1 es suficiente para explicar los datos
#frente a la hipótesis alternativa de que un modelo M2 es mejor.
#los modelos se anidan

modelo1 <- lm(ER ~ Fase_desarrollo, data = datosA.train_TIR1)

modelo2 <- lm(ER ~ poly(Fase_desarrollo, 2), data = datosA.train_TIR1)

modelo3 <- lm(ER ~ poly(Fase_desarrollo, 3), data = datosA.train_TIR1)

anova(modelo1, modelo2, modelo3)

#Ningun modelo fue significativo

#---Evaluación del modelo---
pred.modelo.poli_TIR1 <-  predict(modelo2)
test.error.poli_TIR1 <- mean((pred.modelo.poli_TIR1 - datosA.test_TIR1$ER)^2)
test.error.poli_TIR1

#---Spline de regresión---

#---Ajuste del modelo---

#----Selección de los grados de libertad mediante validación cruzada---
#Se empleara el método de 10-fold cross validation para encontrar el valor 
#óptimo de grados de libertad, probando valores de entre 3 y 16. 

# Vector donde se almacenarán los errores de validación
cv.error <- rep(NA, 16)

# K-fold cross validation para cada valor de df
for(i in 2:16){
  modelo.spline_TIR1 <- glm(ER ~ bs(Fase_desarrollo, degree = 2, df = i),
                            data = datosA.train_TIR1)
  set.seed(3)
  cv.error[i] = cv.glm(datosA.train_TIR1, modelo.spline_TIR1, K = 10)$delta[1]
}

# El primer valor del vector cv.error son NA ya que no se ha evaluado
head(cv.error)

ggplot(data = data.frame(grados_libertad = 2:16, cv.error = cv.error[-1]),
       aes(x = grados_libertad, y = cv.error)) +
  geom_point(color = "orangered2") +
  geom_path() +
  scale_x_continuous(breaks=3:16) +
  labs(title = "cv.MSE  ~ Grados de libertad") +
  theme_bw() +
  theme(panel.background = element_blank(), panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)
)

# Número y posición de los knots en función de los df

attr(x = bs(datosA.train_ARF8$Fase_desarrollo, degree = 2, df = 6),
     which = "knots")

# Modelo spline

modelo.spline_ARF8 <- lm(ER ~ bs(Fase_desarrollo,
                                 degree = 2, df = 6),
                         data = datosA.train_ARF8)

# Número y posici?n de los knots en función de los df

attr(x = bs(datosA.train_TIR1$Fase_desarrollo, degree = 2, df = 6),
     which = "knots")

# Modelo spline

modelo.spline_ARF8 <- lm(ER ~ bs(Fase_desarrollo, degree = 2, df = 6),
                         data = datosA.train_TIR1)
summary(modelo.spline_TIR1)

# SPLINE
ggplot(data = datosA.train_TIR1, aes(x = Fase_desarrollo, y = ER)) +
  geom_point(col = "darkgrey") +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 2, df = 6),
              color = "red", se = TRUE, level = 0.95) +
  labs(title = "Spline con df = 6: ER ~ Fase_desarrollo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)
  )

# SPLINE NATURAL (por defecto, cúbico)
ggplot(data = datosA.train_TIR1, aes(x = Fase_desarrollo, y = ER)) +
  geom_point(col = "darkgrey") +
  geom_smooth(method = "lm", formula = y ~ ns(x, df = 6), color = "black",
              se = TRUE, level = 0.95) +
  labs(title = "Natural spline con df = 6: ER ~ Fase_desarrollo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)
  )

# SPLINE DE SUAVIZADO
modelo.spline.s_TIR1 <- smooth.spline(x = datosA.train_TIR1$Fase_desarrollo,
                                      y = datosA.train_TIR1$ER, cv = TRUE)

# Grados de libertad y lambda correspondientes al modelo ajustado
modelo.spline.s_TIR1$df

modelo.spline.s_TIR1$lambda

plot(ER ~ Fase_desarrollo, data = datosA.train_TIR1, col = "darkgrey") 
title("Smoothing spline (df = 4,65)")
lines(modelo.spline.s_TIR1, col = "darkgreen",
      lwd = 2)

#----Evaluación del modelo---

pred.modelo.spline_TIR1 <-  predict(modelo.spline.s_TIR1)
test.error.spline_TIR1 <- mean((pred.modelo.spline_TIR1$y - datosA.test_ARF8$ER)^2)
test.error.spline_TIR1


#Comparación de modelos

modelo <- c("Polinómico (d = 2)", "B-Spline")
test.MSE <- c(test.error.poli_TIR1, test.error.spline_TIR1)
comparacion <- data.frame(modelo = modelo, test.MSE = test.MSE)

ggplot(data = comparacion, aes(x = reorder(x = modelo, X = test.MSE), 
                               y = test.MSE, color = modelo, 
                               label = round(test.MSE,2))) + 
  geom_point(size = 8) + 
  geom_text(color = "white", size = 2) + 
  labs(x = "Modelo regresión", y = "Test error(MSE)") + theme_bw() + 
  coord_flip() + theme(legend.position = "none")

