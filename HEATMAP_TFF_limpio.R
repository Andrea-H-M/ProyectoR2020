
#Este script permite realizar un Heatmap, en el cual sea posible modificar los rangos del color key
#y agregar una paleta de colores propia

#Librerias
#library(RColorBrewer) Sirve para crear paletas de colores
#library(gplots) realizar graficos, utiliceé la función heatmap.2



#Los datos se van a abrir desde Import Datatest, después se selecciona From test (base)
#se selecciona el arcivo y se le pone Row names, use first column

#Transformar a matriz
Matriz_TFF<-data.matrix(Heatmap_datos1)
Matriz_TFF

#Definí una paleta de colores llamada my_palette2

library(RColorBrewer)
my_palette2<- colorRampPalette(c("blue", "white", "red"))(n = 299)

#Defini los intervalos de expresión que queria resaltar, como los datos más bajos
#tendian a 0 estan en azul y los más altos llegaban hasta 6 por eso están en rojo
mycol_breaks2 = c(seq(-1,0.2,length=100),  #azul
                  seq(0.21,0.96,length=100), #blanco   
                  seq(0.97,6,length=100))   #rojo

#Este es el códifo final en el que logre cambiar el color, agregar el color key y definir
#los saltos de color que queria resaltar
library(gplots)
heatmap.2(Matriz_TFF,main = "Transición de flor a fruto",margins = c(12,9), col=my_palette2,#Titulo, paleta
          breaks=mycol_breaks2,#<-Saltos de color
          key.title=NA, #<- No agregar titulo a Color key
          key.xlab=NA, #<- No establecer etiqueta a x
          key.par=list(mgp=c(1.5, 0.5, 0), 
                       mar=c(1, 2.5, 1, 0)), #<- Parámetros gráficos para la clave de color
          key.xtickfun=function() { #<- Cálculo de la ubicación de marca y etiquetas para el eje x del color key 
            cex <- par("cex")*par("cex.axis") 
            side <- 1
            line <- 0
            col <- par("col.axis")
            font <- par("font.axis")
            mtext("low", side=side, at=0, adj=0,
                  line=line, cex=cex, col=col, font=font)
            mtext("high", side=side, at=1, adj=1,
                  line=line, cex=cex, col=col, font=font)
            return(list(labels=FALSE, tick=FALSE))
          })
