#Graficos para estadisticas del laboratorio de bioinformación

#Paqueterias utilizadas
library(RColorBrewer)
library(dplyr)
library(gganimate)
library(ggplot2)

#Instalar paquetería para realizar conexión con airtable
install.packages("devtools")

devtools::install_github("bergant/airtabler")

#Clave API general
Sys.setenv(AIRTABLE_API_KEY='key71qEJAZj3dRU7p')

#Importamos la base "equipo bioinformación" con su respectivo API e indicamos que tablas necesitamos
Laboratorio <- airtable(base = "app8a51li6LWHdlyq", tables = c("horas","equipo"))  

#GRAFICA 1 -  HORAS TRABAJADAS DURANTE LA SEMANA POR INTEGRANTE
#Seleccionamos la tabla "horas"
Tabla_horas <- Laboratorio$horas$select() 
#Creamos 2 vectores que incluyen los nombres de los integrantes y las horas realizadas durante la semana correspondiente
nombre <- c(Tabla_horas$`equipo nombre`)
horas <- c(Tabla_horas$`39-22`)

#Creamos un dataframe
df2 = data.frame(nombre,horas)
#Eliminamos datos nulos
datos <- na.omit(df2)
#Ordenamos por número de horas
datosorden <- datos[order(datos$horas, decreasing = TRUE),]
nombre <- c(datosorden$nombre)
horas <- c(datosorden$horas)

#Paleta de colores
colourCount = length(unique(nombre))
getPalette = colorRampPalette(brewer.pal(3, "Pastel2"))

#Grafico de barras
animate <- ggplot() + geom_bar(data = datosorden, aes(x= reorder(nombre,horas), y= horas), stat = 'identity', fill=getPalette(colourCount)) + 
           coord_flip() +
           labs(x = "", y = "Horas",title ="HORAS DE TRABAJO", subtitle = "SEMANA 39", caption = "Laboratorio de Bioinformación") + 
           theme(axis.text=element_text(colour="#F8F8FF"), #color del texto general
           axis.title= element_text(color = "white", size =15),
           plot.title = element_text(color = "white", size = 25, #color y tamaño título
                                    hjust = .5, face = "bold"), #título centrado negrita
           plot.subtitle = element_text(color = "#FFF5EE", hjust = .5,face = "italic", size = 15), #subtítulo cursiva
           plot.caption = element_text(color = "white", face = "italic", size = 10),
           panel.background = element_rect(fill = '#141414'), #modificar color panel fondo
           plot.background=element_rect(fill = "#141414"), #modificar fondo
           panel.border = element_blank(), #eliminar bordes
           panel.grid.major = element_blank(), #eliminar grid
           panel.grid.minor = element_blank())+ #eliminar grid
           geom_text(aes(y=horas, x=nombre, label= horas, vjust= 0.1 , hjust= -0.20), color = "white") + 
           transition_states(horas, wrap = FALSE) +   shadow_mark()  
  
#Animación del grafico
animate(animate, width = 700, height = 500, fps = 25, duration = 9, rewind = TRUE)
  
#Guardamos el gráfico en archivo .gif
anim_save("Horasporsemana.gif")
  
#Version pdf
ggplot() + geom_bar(data = datosorden, aes(x= reorder(nombre,horas), y= horas), stat = 'identity', fill=getPalette(colourCount)) + 
           coord_flip() +
           labs(x = "Nombres", y = "Horas",title ="HORAS DE TRABAJO", subtitle = "SEMANA 39", caption = "Laboratorio de Bioinformación") + 
           theme(axis.text=element_text(colour="black"), #color del texto general
           axis.title= element_text(color = "black", size =15),
           plot.title = element_text(color = "black", size = 25, #color y tamaño título
                                    hjust = .5, face = "bold"), #título centrado negrita
           plot.subtitle = element_text(color = "black", hjust = .5,face = "italic", size = 15), #subtítulo cursiva
           plot.caption = element_text(color = "black", face = "italic", size = 10),
           panel.background = element_rect(fill = 'white'), #modificar color panel fondo
           plot.background=element_rect(fill = "white"), #modificar fondo
           panel.border = element_blank(), #eliminar bordes
           panel.grid.major = element_blank(), #eliminar grid
           panel.grid.minor = element_blank())+ #eliminar grid
           geom_text(aes(y=horas, x=nombre, label= horas, vjust= 0.1 , hjust= -0.20), color = "black")
    

#GRAFICA 2 -  HORAS TRABAJADAS POR Linea
#Seleccionamos la tabla "horas"
Tabla_horas <- Laboratorio$horas$select() 
#Creamos 2 vectores que incluyen la linea y las horas realizadas durante la semana correspondiente
linea <- c(Tabla_horas$`equipo linea`)
horas <- c(Tabla_horas$`36-22`)
  
#Creamos un dataframe
df2 = data.frame(linea,horas)
#Eliminamos datos nulos
datos <- na.omit(df2)
grupos <- group_by(datos, linea)
suma <- summarise(grupos ,sum = sum(horas))
#Ordenamos por número de horas
datosorden <- suma[order(suma$sum, decreasing = TRUE),]
linea <- c(datosorden$linea)
horas <- c(datosorden$sum)

#Paleta de colores
colourCount = length(unique(linea))
getPalette = colorRampPalette(brewer.pal(6,"PuRd"))

#Grafico de barras  
animate <- ggplot() + geom_bar(data = datosorden, aes(x= reorder(linea,horas), y= horas), stat = 'identity', fill=getPalette(colourCount)) + 
           coord_flip() +
           labs(x = "Linea", y = "Horas",title ="HORAS DE TRABAJO", subtitle = "SEMANA 36", caption = "Laboratorio de Bioinformación") + 
           theme(axis.text=element_text(colour="#F8F8FF"), #color del texto general
           axis.title= element_text(color = "white", size =15),
           plot.title = element_text(color = "#FFB5C5", size = 25, #color y tamaño título
                                    hjust = .5, face = "bold"), #título centrado negrita
           plot.subtitle = element_text(color = "#FFF5EE", hjust = .5,face = "italic", size = 15), #subtítulo cursiva
           plot.caption = element_text(color = "#F08080", face = "italic", size = 10),
           panel.background = element_rect(fill = '#141414'), #modificar color panel fondo
           plot.background=element_rect(fill = "#141414"), #modificar fondo
           panel.border = element_blank(), #eliminar bordes
           panel.grid.major = element_blank(), #eliminar grid
           panel.grid.minor = element_blank())+ #eliminar grid
           geom_text(aes(y=horas, x=linea, label= horas, vjust= 0.1 , hjust= -0.20), color = "white") + 
           transition_states(horas, wrap = FALSE)  +   shadow_mark()  
          
#Animación del grafico
animate(animate, width = 700, height = 500, fps = 25, duration = 10, rewind = TRUE)
      
#Versión pdf
ggplot() + geom_bar(data = datosorden, aes(x= reorder(linea,horas), y= horas), stat = 'identity', fill=getPalette(colourCount)) + 
           coord_flip() +
           labs(x = "Linea", y = "Horas",title ="HORAS DE TRABAJO", subtitle = "SEMANA 36", caption = "Laboratorio de Bioinformación") + 
           theme(axis.text=element_text(colour="black"), #color del texto general
           axis.title= element_text(color = "black", size =15),
           plot.title = element_text(color = "pink", size = 25, #color y tamaño título
                                            hjust = .5, face = "bold"), #título centrado negrita
           plot.subtitle = element_text(color = "black", hjust = .5,face = "italic", size = 15), #subtítulo cursiva
           plot.caption = element_text(color = "black", face = "italic", size = 10),
           panel.background = element_rect(fill = 'white'), #modificar color panel fondo
           plot.background=element_rect(fill = "white"), #modificar fondo
           panel.border = element_blank(), #eliminar bordes
           panel.grid.major = element_blank(), #eliminar grid
           panel.grid.minor = element_blank())+ #eliminar grid
           geom_text(aes(y=horas, x=linea, label= horas, vjust= 0.1 , hjust= -0.20), color = "black")


#GRAFICA 3 -  HORAS TRABAJADAS POR PROYECTO
#Seleccionamos la tabla "horas"
Tabla_horas <- Laboratorio$horas$select() 
#Creamos 2 vectores que incluyen el proyecto y las horas realizadas durante la semana correspondiente
proyecto <- c(Tabla_horas$`equipo proyecto`)
horas <- c(Tabla_horas$`36-22`)

#Creamos un dataframe
df2 = data.frame(proyecto,horas)
datos <- na.omit(df2)
grupos <- group_by(datos, proyecto)
suma <- summarise(grupos ,sum = sum(horas))
#Ordenamos por número de horas
datosorden <- suma[order(suma$sum, decreasing = TRUE),]
proyecto <- c(datosorden$proyecto)
horas <- c(datosorden$sum)

#Paleta de color
colourCount = length(unique(proyecto))
getPalette = colorRampPalette(brewer.pal(6,"GnBu"))

#Grafico de barras
animate <- ggplot() + geom_bar(data = datosorden, aes(x= reorder(proyecto,horas), y= horas), stat = 'identity', fill=getPalette(colourCount)) + 
           coord_flip() +
           labs(x = "Proyecto", y = "Horas",title ="HORAS DE TRABAJO", subtitle = "SEMANA 36", caption = "Laboratorio de Bioinformación") + 
           theme(axis.text=element_text(colour="#F8F8FF"), #color del texto general
           axis.title= element_text(color = "white", size =15),
           plot.title = element_text(color = "#BBFFFF", size = 25, #color y tamaño título
                                  hjust = .5, face = "bold"), #título centrado negrita
           plot.subtitle = element_text(color = "#F0FFF0", hjust = .5, face = "italic"), #subtítulo cursiva
           plot.caption = element_text(color = "#F0FFF0", face = "italic", size = 10),
           panel.background = element_rect(fill = '#141414'), #modificar color panel fondo
           plot.background=element_rect(fill = "#141414"), #modificar fondo
           panel.border = element_blank(), #eliminar bordes
           panel.grid.major = element_blank(), #eliminar grid
           panel.grid.minor = element_blank())+ #eliminar grid
           geom_text(aes(y=horas, x=proyecto, label= horas, vjust= 0.1 , hjust= -0.20), color = "white") + 
           transition_states(horas, wrap = FALSE)  +   shadow_mark()  

#Animación del grafico
animate(animate, width = 700, height = 500, fps = 25, duration = 10, rewind = TRUE)


#GRAFICA 4 -  Hombres vs Mujeres
Tabla_genero <- Laboratorio$equipo$select()
genero <- c(Tabla_genero$genero)

#Creamos un dataframe
df2 = data.frame(genero)

datos <- na.omit(df2)
cantidadM <- sum(datos == "Mujer")
cantidadH <- sum(datos == "Hombre")

df <- data.frame(value = c(cantidadH, cantidadM), group = c("Hombre", "Mujer"))

ggplot(df, aes(x = "", y = value, fill = group)) + geom_col(color = "black") + geom_text(aes(label = value),
      position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") + scale_fill_brewer() + theme_void()
  
  
  
  
  
  
  


