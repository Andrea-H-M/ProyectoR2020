
#Este script permite realizar graficas de lineas con los mismos rangos en el eje y, con rangos de punto
#en lugar de barras de error para hacer m�s estetico el gr�fico

# Librerias:
library(ggplot2) #Permite realizar gr�ficos complejos y de alta calidad empleando c�digos sencillos


#Para CH-I
CH_ICOLOR <- read.table(file= "CH-I.csv", header = TRUE, sep = ",")

ggplot(data = CH_ICOLOR, aes(x = Tiempo, y = ER, group = Gen, color = Gen)) + #<- Agrupar por genotipos
  geom_line(size = .25) + #<- Agregar lineas
  geom_point() + #<- Graficar los valores con puntos
  geom_pointrange(aes(ymin = ER+DE, ymax =ER-DE), width = 0.05) + #<- Graficar la desvisi�n estandar en 
  theme_classic() +                                              #rangos de punto
  xlab ("Fase del desarrollo") + #<-titulo de x
  ylab ("Expresi�n relativa (ER)") + #<-titulo de y
  ylim (0,7) #<-Graficar hasta el valor 7 de y (de esta manera pude tener las 2 graficas en los mismos rangos)


#Para CH-VI

CH_VICOLOR <- read.table(file= "CH-VI.csv", header = TRUE, sep = ",")

ggplot(data = CH_VICOLOR, aes(x = Tiempo, y = ER, group = Gen, color = Gen)) +
  geom_line(size = .25) +
  geom_point() +
  geom_pointrange(aes(ymin = ER+DE, ymax =ER-DE), width = 0.05) +
  theme_classic() +
  xlab ("Fase del desarrollo") +
  ylab ("Expresi�n relativa (ER)") + 
  ylim (0,7)

