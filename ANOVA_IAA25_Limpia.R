#----DESCRIPCIÓN---
#Este script sirve para realizar una ANOVA de dos factores al alzar balanceada,
#prueba de Tukey, distintas pruebas para comprobar los supuestos de normalidad y homogeneidad de los datos
#Factores:
#Genotipo y fase del desarrollo
#Niveles de factor:
#Genotipo: CH-I y CH-VI
#Fase del desarrollo: 1dap, As/p, Ap, 1ddp, 10ddp, 25ddp y 45ddp
#Celda: R1, R2 y R3 (balanceado)
#Variable de respuesta: Expresión relativa (ER)
#Queremos saber si la ER depende del genotipo o de la Fase del desarrollo

#---LIBRERIAS---

#library(dplyr) proporciona una "gramática"para la manipulación y operaciones con data frames
#library(ggpubr) facilita la creación de gráficos basados en ggplot2 para investigadores
# library(multcomp) sirve para generar pruebas simultaneas e intervalos de confianza para
#hipótesis lineales en modelos paramétricos, lineales, lineales generalizados etc.
#library(car) Calcular tablas de análisis de varianza



#Leer la tabla en R desde un archivo CSV

ANOVA_IAA25_1 <-read.table(file= "IAA25.csv", header = TRUE, sep = ",")

head(ANOVA_IAA25_1)

#Para checar la estructura de los datos

library(dplyr)
str(ANOVA_IAA25_1)

#Como R considera a Fase_desarrollo como una variable numérica. Lo convertiremos
#a una variable de factor (es decir, variable de agrupación) 
#Convertir fase del desarrollo a factor y recordar los niveles 1dap, As/p...

ANOVA_IAA25_1$Fase_desarrollo <- factor(ANOVA_IAA25_1$Fase_desarrollo)
levels = c("1 (1dap)", "2 (A s/p)", "3 (Ap)", "4 (1ddp)", "5 (10ddp)", "6 (25ddp)", "7 (45ddp)")
labels = c("F1 (1dap)", "F2 (A s/p", "F3 (Ap)", "F4 (1ddp)", "F5 (10ddp)", "F6 (25ddp)", "F7 (45ddp)")


#Generar tabla de frecuencia para saber si los datos están balanceados

table(ANOVA_IAA25_1$Genotipo,
      ANOVA_IAA25_1$Fase_desarrollo)



#---Distribución de los datos---
#Se realizaron trazados de linea con multiples grupos, cada grupo fue una fase del desarrollo,
#se agruparon por genotipo y se agragaron barras de error

library(ggpubr)
ggline(ANOVA_IAA25_1, x = "Fase_desarrollo", y = "ER", color = "Genotipo", #<- Agrupar por genotipo
       add = c("mean_se","dotplot"), #<-barras de error y graficas de punto (de cada repilica)
       palette = c("#00AFBB", "#FC4E07") #<colores
)

#ANOVA Modelo aditivo, supone que las dos variables de factores son independientes.
#¿La expresión relativa del gen IAA25 depende de la fase del desarrollo o del genotipo?
#La función R aov () se puede usar para responder esta pregunta.
#La función summary.aov () se utiliza para resumir el ANOVA

res.aovIAA25 <- aov(ER ~ Genotipo + Fase_desarrollo,
                    data =
                      ANOVA_IAA25_1
)

summary(res.aovIAA25)


#---Interpretación---
#Hay diferencias significativas entre las medias del factor genotipo con una significacia de 0.05; por lo tanto,
#la expresióon relativa del gen IAA25 depende del genotipo


#---ANOVA bidireccional con efecto de interacción---
#Se realiza cuando se cree que las dos variables podrían interactuar para crear un efecto sinérgico

res.aovIAA25 <- aov(ER ~ Genotipo * Fase_desarrollo,
                    data =
                      ANOVA_IAA25_1
)
summary(res.aovIAA25)

res.aovIAA25 <- aov(ER ~ Genotipo + Fase_desarrollo + Genotipo:Fase_desarrollo,
                    data = 
                      ANOVA_IAA25_1
)

summary(res.aovIAA25)

#---Interpretación---
#Los niveles de ER del gen IAA25 están relacionados con la fase del desarrollo
#Los valores de ER del gen IAA25 están relacionados con el genotipo 
#Los niveles de ER de IAA25 están relacionados con la interacción entre el genotipo por fase del desarrollo 

#---Comparación múltiple por pares entre las medias de los grupos---
#Algunas de las medias del grupo son diferentes, pero no sabemos qué pares de grupos son diferentes.
#Se realizará la prueba deTukey HSD para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Sólo se realizará en el factor fase del desarrollo porque
#el factor genotipo solo tiene dos niveles y ya sabemos que son diferentes.

library(multcomp)
TukeyHSD(res.aovIAA25,
         which = 
           "Fase_desarrollo")

#diff: diferencia entre medias de los dos grupos
#lwr, upr: el punto final inferior y superior del intervalo de confianza al 95% (predeterminado)
#p adj: valor p después del ajuste para las comparaciones múltiples.
#las comparaciones por pares son significativas si tienen un valor p ajustado <0.05.

#---Múltiples comparaciones usando el paquete multcomp---
#Para múltiples procedimientos de comparación para un ANOVA
#glht (pruebas de hipótesis lineales generales)

summary(glht(res.aovIAA25,
             linfct = 
               mcp(Fase_desarrollo = "Tukey")
))


#---Verificación de los supuestos del ANOVA---
#ANOVA supone que los datos se distribuyen normalmente y la varianza entre los grupos es homogénea

#--Homogenéidad de varianzas--
#La gráfica de residuos versus ajustes se utiliza para verificar la homogeneidad de las varianzas

plot(res.aovIAA25, 1)

#no hay relaciones evidentes entre los residuos y los valores ajustados (la media de cada grupo)
#Se asume homogeneidad de las varianzas
#Los puntos 24 y 20 son valores atípicos que puede afectar la normalidad y la homogeneidad 

#---prueba de Levene para verificar la homogeneidad de las varianzas---
#Con la libreria car

library(car)
leveneTest(ER~ Genotipo*Fase_desarrollo,
           data =
             ANOVA_IAA25_1)

#El valor p es mayor que el nivel de significancia de 0.05
#Por lo tanto, podemos asumir la homogeneidad de las varianzas en los diferentes grupos de tratamiento

#---Normailida---
#los cuantiles de los residuos se grafican contra los cuantiles de la distribución normal.
#También se traza una línea de referencia de 45 grados.
#La gráfica de probabilidad normal de los residuos se utiliza para verificar la suposición de que los residuos se distribuyen normalmente.

plot(res.aovIAA25, 2)

#La gráfica sigue una linea recta, existe normalidad

#---prueba de Shapiro-Wilk de los residuos de ANOVA---

#Extraer residuos
aov_residuals <- residuals(object =
                             res.aovIAA25)

#test de Shapiro-Wilk
shapiro.test(x = 
               aov_residuals )

#---Interpretación---
#La hipótesis nula se rechazará si W es demasiado pequeño
#El valor de W puede oscilar entre 0 y 1.
#Siendo la hipótesis nula que la población está distribuida normalmente
#LOS DATOS TIENEN DISTRIBUCIÓN NORMAL, ANOVA BALANCEADA ADECUADA

