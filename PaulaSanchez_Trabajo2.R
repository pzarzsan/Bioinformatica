#TRABAJO 2 R
#CARGA LOS DATOS Y EXAMÍNALOS EN R
getwd() #para verificar la ubicación actual, para saber donde estoy
setwd() #para cambiar al directorio donde están mis datos
data<- read.table(file="datos-trabajoR.txt", header = TRUE) #para cargar y leer los datos del archivo en R
head(data) #para ver las primeras filas de los datos
summary(data) #para que me haga un resumen estadístico de mis datos
dim(data) #para ver las dimensiones de los datos
str(data) #para saber la estructura compacta de los datos

#¿CUÁNTAS VARIABLES HAY?
#hay 2 variables 
#¿CUÁNTOS TRATAMIENTOS HAY?
#hay 5 tratamientos para las 2 variables

#REALIZA UN BOXPLOT PARA NUESTROS DATOS
boxplot(data) #para crear un boxplot de las variables de los datos

boxplot(Tratamiento, col = "red") #para crear un boxplot del Tratamiento de color rojo
boxplot(Variable1, col = "orange") #para crear un boxplot de la Variable1 de color naranja
boxplot(Variable2, col = ("yellow") #para crear un boxplot de la Variable2 de color amarillo
 
#CREA UN GRÁFICO DE DISPERSIÓN CON LAS DOS VARIABLES
#primero hay que definir las variables por separado
Tratamiento <- data[,1] #para crear una nueva variable que contiene los datos de la primera columna de los datos
Variable1 <- data[,2] #para crear una nueva variable que contiene los datos de la segunda columna de los datos
Variable2 <- data[,3] #para creat una nueva variable que contiene los datos de la tercera columna de los datos

class(Tratamiento) #para ver el tipo de datos que tengo en el Tratamiento
class(Variable1) #para ver el tipo de datos que tengo en el Variable1
class(Variable2) #para ver el tipo de datos que tengo en el Variable2

#HAZ UN GRÁFICO DE DISPERSIÓN CON LAS DOS VARIABLES
plot(Variable2~Variable1) #para crear un grafico de que representa la relación entre "Variable2" en función de "Variable1", donde Variable1 estará en el eje X y Variable2 en el Y 
plot(x = Variable1, y = Variable2, col= data$Tratamiento) #para establecer puntos coloreados según los valores en la columna "Tratamiento"

#PONLE LA LEYENDA AL GRÁFICO DEL APARTADO ANTERIOR
legend(x = "bottomright", legend = c("Tratamiento1", "Tratamiento2", "Tratamiento3", "Tratamiento4", "Tratamiento5"), fill = c("black", "red", "green", "light blue", "dark blue"), title = "Tratamientos") #para añadir una leyenda en el plot en la esquina inferior derecha

#HAZ UN HISTOGRAMA PARA CADA VARIABLE
hist(Variable1, c="orange") #para hacer un histograma con la Variable1 de color naranja
hist(Variable2, c="yellow") #para hacer un historgrama con la Variable2 de color amarillo
hist(Tratamiento, c="red") #para hacer un histograma con el Tratamiento de color rojo

#HAZ UN FACTOR EN LA COLUMNA TRATAMIENTO Y GUÁRDALO EN UNA VARIABLE
Tratamiento <- factor(Tratamiento) #para convertir una variable en un factor

#CALCULA LA MEDIA Y LA DESVIACIÓN ESTÁNDAR PARA CADA TRATAMIENTO
aggregate(Variable1~Tratamiento, FUN = function(x) c(Media=mean(x), SD=sd(x))) #para calcular la media y la desviación estándar de Variable1 agrupada por la variable categórica Tratamiento
aggregate(Variable2~Tratamiento, FUN = function(x) c(Media=mean(x), SD=sd(x))) #para calcular la media y la desviación estándar de Variable2 agrupada por la variable categórica Tratamiento
	
#AVERIGUA CUÁNTOS ELEMENTOS TIENE CADA TRATAMIENTO
table(Tratamiento) #para crear una tabla de frecuencias que muestra cuántas veces aparece cada tratamiento
#así, obtenemos que cada tratamiento tiene 10 elementos (50 en total)

#EXTRAER LOS DATOS PARA EL TRATAMIENTO 1 Y EL TRATAMIENTO 4, GUÁRDALOS EN UNA VARIABLE DIFERENTE
Tratamiento1[1:10] #esto equivale a los valores del Tratamiento1
Tratamiento4[31:40] #esto equivale a los valores del Tratamiento4

#ahora los aislamos en variables independientes para poder trabajar sobre ellas
Tratamiento1<-data[1:10,1:3] #para crear una nueva variable independiente "Tratamiento1", que contiene los primeros 10 elementos de la variable "Tratamiento"
Tratamiento4<-data[31:40,1:3] #para crear una nueva variable independiente "Tratamiento4", que contiene las observaciones que van desde la posición 31 hasta la 40 de la variable "Tratamiento"
#si usamos la función table, podemos comprobar que solo hay elementos para el tratamiento correspondiente
table(Tratamiento1)
table(Tratamiento4)

#COMPROBAR LA HIPÓTESIS
#nuestra hipótesis nula es que las medias de Tratamiento1 y Tratamiento4 para la Variable1 son iguales

#primero aislamos los datos
Tratamiento1V1<-data[1:10,2] #así conseguimos los datos del Tratamiento 1 para la variable 1
Tratamiento4V1<-data[31:40,2] #así conseguimos los datos del Tratamiento 4 para la variable 1

#antes de comprobar la hipótesis, debemos ver si nuestros datos siguen una distribución normal
#para ello, utilizamos el shapiro.test, de esta forma, si el resultado del p-value >0.05, nuestros datos seguirían una distribución normal
shapiro.test(Tratamiento1V1) #p-value=0.06434
shapiro.test(Tratamiento4V1) #p-value=0.1564
#como p-value>0.05, nuestros datos siguen una distribucón normal

#ahora, para saber si nuestra hipótesis se cumple, habría que comparar el valor de sus medias mediante t.test
#primero, se vuelve a hallar las medias, concretamente del tratamiento1 y 4 de la variable 1
mTratamiento1V1<-mean(Tratamiento1V1)
mTratamiento4V1<-mean(Tratamiento4V1)
#para comprobar si las medias son las mismas, usamos el t.test
#en este caso, si nuestro p-value<0.05, indicará que las medias son diferentes
t.test(Tratamiento1V1, Tratamiento4V1)
#dado que el valor de p-value es <0.05, los resultados indican que hay una diferencia significativa entre las medias de las dos muestras

#para saber si sus varianzas son iguales, usamos el F-test
var.test(Tratamiento1V1, Tratamiento4V1) 
#en este caso, el p-value<0.05, se rechaza la hipótesis nula, por lo que las varianzas son diferentes



