#http://rstudio-pubs-static.s3.amazonaws.com/366647_cd7fd22905e34974856eb908d846d66f.html#introducci%C3%B3n
#http://halweb.uc3m.es/esp/Personal/personas/durban/esp/web/GLM/curso_GLM.pdf
#https://tsmatz.wordpress.com/2017/08/30/glm-regression-logistic-poisson-gaussian-gamma-tutorial-with-r/
#https://www.uv.es/lejarza/eaa/teoria/EAA7%20glm.pdf
#http://people.tamu.edu/~alawing/materials/ESSM689/GLMtutorial.pdf
#https://rpubs.com/JessicaP/459130


#Este script sirve para hacer un modelo lineal generalizado con distribución gaussiana link identidad
#Ya que se realizaron las mismas prueba de normalidad y de homogeneidad de varianza y al no cumplir con los supuestos del ANOVA
#Se sealizó un GLM.

#El modelo que mejor se ajusto a la variablde de resupesta fue con la distibución gaussiana link identidad
# Ya que se probaron varios modelos y con este la desviansa disminuyó notablemente.

#---PARA TIR1---

#Para ingresar los datos en formato CSV
DATOS_TIR1_1 <- read.table(file= "TIR1.csv", header = TRUE, sep = ",")
head(DATOS_TIR1_1)

#---Función liga---
#tenemos un modelo lineal donde modelizamos la media de la respuesta sino una función de dicha media

inf.TIR1 <- glm(ER ~ Fase_desarrollo + Genotipo, family = gaussian(link = identity), 
                data = DATOS_TIR1_1)

summary(inf.TIR1)




#---Interpretación---
#Existen cambios significativos de la ER del gen TIR1 entre fases del desarrollo
#Existe un cambio significativo  en la ER de TIR1 entre la fase 1 y la fase 2 (A/sp) con una significancia del 0.05
#Existe un cambio significativo entre la fase 1 y la fase 3 (Ap) con una significancia del 0.01
#Existe un cambio significativo entre la fase 1 y la fase 4 (1ddp) con una significancia del 0.01
#Existe un cambio significativo entre la fase 1 y la fase 5 (10ddp) con una significancia del 0.01
#Existe un cambio significativo entre la fase 1 y la fase 7 (45dp) con una significancia del 0.01
#No existen cambios significativos de ER entre el genotipo CH-I y CH-VI

#La relación entre la fase del desarrollo y la expresión realtiva no es lineal
#Se probaron varios tipos de distribución y la que mejor se ajusto a la variable de respuesta fue
#gaussina link identidad, ya que se redujo notablemente la devianza.

#---Para ARF8---


DATOS_ARF8_1 <- read.table(file= "ARF8.csv", header = TRUE, sep = ",")

head(DATOS_ARF8_1)

#---Función liga---
#tenemos un modelo lineal donde modelizamos la media de la respuesta sino una función de dicha media

inf.ARF8 <- glm(ER ~ Fase_desarrollo + Genotipo, family = gaussian (link = identity), 
                data = DATOS_ARF8_1)

summary(inf.ARF8)

#---Interpretación---
#Existen cambios significativos de ER del gen ARF8 entre fases del desarrollo
#Existe cambios significativos de ER entre la fase 1 y todas las fases del desarrollo
#No existen cambios significativos de ER entre el genotipo CH-I y CH-VI

#La relación entre la fase del desarrollo y la expresión realtiva no es lineal
#Por esta razón se realizó un modelo no lineal para ambos genes 


