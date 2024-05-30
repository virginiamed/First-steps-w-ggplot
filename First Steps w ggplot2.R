##Primeros pasos usando la paqueteria GGPLOT2##
##First Steps using GGPLOT2 package##

#Estructura general del GGPLOT2 / General Structure of GGPLOT2
#ggplot(data = DATOS) + GEOM_FUNCIÓN( mapping = aes(MAPEOS X y Y), 
#stat = ESTADÍSTICAS, position = POSICIÓN ) + 
#FUNCIÓN_COORDENADAS + FUNCIÓN_FACETAS

install.packages("tidyverse")
library(tidyverse)
library(readxl)

#Este comando sirve para determinar el directorio de trabajo,
#del cual se extraeran los archivos y bds
#Use this function to determine the work directory from which DBs and files 
#will be extracted
setwd("C:/Users/virgi/OneDrive/Escritorio/R/CursoA2")
enoe <- read_xlsx("mu_enoe.xlsx")
View(enoe)

#Determina el tipo de objeto (numeric, string, json, etc)
#use this function to determine the object class
class(enoe)

#Nos dice el tama;o de nuestra base de datos
#Use this function to determine the size of the database
dim(enoe)

#Nos dice el nombre de las columnas, en este caso variables
#Use this to determine the column names of the database, in this case variables
colnames(enoe)

#Generar grafica de dispersion con ggplot2
#This function generates graphs through ggplot package
ggplot(data = enoe) + 
  geom_point(mapping = aes (x = anios_esc, y = ingreso_mensual, 
                            color = tipo_empleo, 
                            shape = tipo_empleo))
ggplot(data = enoe) +
  geom_jitter(mapping = aes(x = anios_esc, y = ingreso_mensual,
                            color = tipo_empleo,
                            shape = tipo_empleo))

#Generar facetas en graficos, separar variables categoricas
#Information can be segmented
ggplot(data = enoe) +
  geom_point(mapping = aes(x = anios_esc, y = ingreso_mensual,
                           color = tipo_empleo))+
  facet_wrap(~sex, nrow = 1, ncol = 2)

ggplot(data = enoe) +
  geom_col(mapping = aes(x = num_trabajos, y = ingreso_mensual, fill = sex)) +
  facet_grid(sex~tipo_empleo)

ggplot(data = enoe) +
  geom_col(mapping = aes(x = num_trabajos, y = anios_esc, fill = sex)) +
  facet_grid(tipo_empleo~sex)

#Segregacion de informacion
#A specific segment of information can be displayed
ggplot(data = enoe) +
  geom_point(mapping = aes(x = anios_esc, y = ingreso_mensual, 
                           color = sex))+
  facet_grid(tipo_empleo~sex)

#Otros tipos de graficas - smooth o linea suavizada
#Other graphs - smooth line
ggplot(data = enoe) +
  geom_smooth(mapping = aes(x = anios_esc, y = ingreso_mensual),
              color = "gray")

#smooth o linea suavizada con segmentacion por sexo
#Smooth line graph with sex segmentation
ggplot(data = enoe) +
  geom_smooth(mapping = aes(x = anios_esc, y = ingreso_mensual,
                            linetype = tipo_empleo), color = "darkgreen")

#Graficando en varias capas
#several layers can compose a graph
ggplot(data = enoe) +
  geom_point(mapping = aes(x = anios_esc, y = ingreso_mensual,
                           color = tipo_empleo)) +
  geom_smooth(mapping = aes(x = anios_esc, y = ingreso_mensual, 
                            linetype = tipo_empleo), color = "orange") +
  facet_grid(sex~num_trabajos)

#Graficando en varias capas, simplificado
#Graphing on multiple layers, simplified
ggplot(data = enoe, mapping = aes(x = anios_esc, y = ingreso_mensual)) +
  geom_smooth(mapping = aes(linetype = tipo_empleo), color = "orange")+
  geom_point(mapping = aes(color = tipo_empleo))

#Graficando en capas y agregando filtros
#Graphing in multiple layers and adding filters
#filter(base_datos, filtro_deseado) / filter(DB, chosen filter)
#show.legend: se utiliza para ocultar o mostrar la leyenda
ggplot(data = enoe, mapping = aes(x = anios_esc, y = ingreso_mensual)) +
  geom_point(mapping = aes(color = niv_edu), show.legend = TRUE) +
  geom_smooth(data = filter(enoe, estado == "Durango"), se = FALSE,
              color = "red")

#Grafica de barras para diferenciar sexo
#bars graph to distinguish sex

ggplot(data = enoe) +
  geom_bar(mapping = aes(x = sex, fill = sex))

#Grafica de barras con diferente sintaxis para diferenciar sexo
#bars graph with another sintax
ggplot(data = enoe) +
  stat_count(mapping = aes(x = sex, fill = sex))

#El simbolo "?" permite conocer la documentacion de comandos
# ? we could use this symbol if we want to know how to use a function
?geom_bar

#Conocer la proporcion de observaciones en relacion al sexo
#Even we could know the proportion of the dataset related to sex
ggplot(data = enoe) +
  geom_bar(mapping = aes(x = sex, y = ..prop.., group = 1))

#Cada geometria tiene una funcion estadistica asociada
#Each geom has associated an statistical function

#Podemos llenar las barras con informacion referente a otro campo 
#We can fill the bars with some other information, like education level
#Stack position
ggplot(data = enoe) +
  geom_bar(mapping = aes(x = sex, fill = niv_edu))

#Existen tres posiciones en las que una grafica puede presentarse
#fill, identity o dodge.
#There are three positions in which the graph could be presented
#identity, fill or dodge
ggplot(data = enoe, mapping = aes(x = sex, fill = num_trabajos)) +
  geom_bar(alpha = .75, position = "identity")

ggplot(data = enoe) +
  geom_bar(mapping = aes(x = sex, fill = tipo_empleo))

ggplot(data = enoe, mapping = aes(x = sex, fill = tipo_empleo)) +
  geom_bar(alpha = .80, position = "identity")

ggplot(data = enoe, mapping = aes(x = sex, color = tipo_empleo)) +
  geom_bar(fill = NA, position = "identity")

ggplot(data = enoe, mapping = aes(x = sex, fill = tipo_empleo)) +
  geom_bar(position = "fill")

ggplot(data = enoe, mapping = aes(x = sex, fill = pos_ocu)) +
  geom_bar(position = "fill")

ggplot(data = enoe, mapping = aes(x = sex, fill = pos_ocu)) +
  geom_bar()

ggplot(data = enoe, mapping = aes(x= factor(1), fill = niv_edu)) +
  geom_bar(position = "fill")

ggplot(data = enoe, mapping = aes(x = factor(1), fill = pos_ocu)) +
  geom_bar(position = "fill")

ggplot(data = enoe, mapping = aes(x = sex, fill= niv_edu)) +
  geom_bar(position = "dodge") +
  labs(title = "Nivel Educativo", x = "Sexo", y = "Numero de Observaciones")

#Para hacer un cambio en las coordenadas coord_flip()
#We can use coord_flip() to switch between the coords position
ggplot(data = enoe, mapping = aes(x = niv_edu, y = ingreso_mensual)) +
  geom_boxplot()

ggplot(data = enoe, mapping = aes(x = niv_edu, y = ingreso_mensual)) +
  geom_boxplot() +
  coord_flip()

ggplot(data = enoe, mapping = aes(x = niv_edu, y = anios_esc)) +
  geom_col()

ggplot(data = enoe, mapping = aes(x = niv_edu, y = hrsocup, color = niv_edu)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Horas de ocupacion por nivel educativo", 
         x = "Nivel Educativo", y = "A;os de Escuela")

#Coord Polar
ggplot(data = enoe, mapping = aes(x = factor(1), fill = pos_ocu)) +
  geom_bar(position = "fill") +
  coord_polar(theta = "y") +
  labs(title = "% Posicion ocupada por trabajadores activos",
       x = "", y = "")

#Otras graficas
#Some other graphs
ggplot(data = enoe, mapping = aes(x = edad, y = ingreso_mensual,
                                  color = niv_edu)) +
  geom_point() +
  labs(title = "Ingresos Mensuales en Relacion a la Edad", x = "Edad",
       y = "Ingreso Mensual")

ggplot(data = enoe, mapping = aes(x = edad, y = ingreso_mensual,
                                  color = niv_edu)) +
  geom_point() +
  labs(title = "Ingresos Mensuales en Relacion a la Edad", x = "Edad",
       y = "Ingreso Mensual", color = "Nivel Educativo")

ggplot(data = enoe, mapping = aes(x = anios_esc, y = ingreso_mensual,
                                  alpha = niv_edu)) +
  geom_point()

ggplot(data = enoe, mapping = aes(x = niv_edu, y = ingreso_mensual,
                                  color = sex, shape = sex)) +
  geom_point()

ggplot(data = enoe) + 
  geom_point(mapping = aes(x = anios_esc, y = ingreso_mensual), 
                                 shape=3, size=3, color = "red")

ggplot(data = enoe) + 
  geom_point(mapping = aes(x =anios_esc, y =ingreso_mensual), 
             shape=5, size=1, color= "blue")
