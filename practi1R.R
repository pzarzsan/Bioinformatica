#############################################################################
#
# PRACTICA 1
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
## ENTREGA EL 01 OCTUBRE 23:59
## Se requiere la entrega de este script completado con los códigos más las imágenes y las respuestas a las preguntas
## Adjuntar en la entrega el PDF final y el archivo con los genes
#
##############################################################################

# Instalar RCurl

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")

# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas
dim(data) #esto mide las dimensiones
head(data)#para mirar las primeras filas
tail(data) #para mirar las últimas filas

# Hacemos un primer histograma para explorar los datos
hist(data) #para crear un histograma

# Transformamos los datos con un logaritmo 
data_log=log2(data) #para guardar la transformacion logarítmica en una variable aparte
hist(data_log)#para hacer un nuevo histograma de los datos logarítmicos

# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?
	#cambio la distribuciónde la gráfica a una más normal
	#puede servir para normalizar la distribución de los datos y reducir la asimetría

# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot?
boxplot(data_log) #para generar un boxplot de los datos transformados
boxplot(data_log,col=c("blue","blue","blue","orange","orange","orange"),main="GSE5583 - boxplots",las=2) #para cambiar el color del boxplot y el título del gráfico
	#hemos puesto las 2 para poner los nombres en vertical
	#un boxplot es un método estandar que se utiliza para representar gráficamente una serie de datos numéricos a través de sus cuartiles. De tal forma que se puede ver a simple vista la mediana y los cuartiles de los datos, además de sus valores atípicos
 
#Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación
# de los valores de expresión. ¿Es correcta la separación?
hc = hclust(as.dist(1-cor(data_log))) #para agrupar los datos en un cluster
plot(hc, main="clustering") #para cambiar el nombre del gráfico
	#sí, nos ha hecho una buena clasificación, claramente hay 3 knockout y 3 wildtype
#######################################
# Análisis de Expresión Diferencial 
#######################################

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
wt <- data[,1:3] 
ko <- data[,4:6]
	#para separar los datos de wildtype y knockout, así cogemos una columna correspondiente a cada dato
class(wt)#nos dice el tipo de datos que tenemos, como una matiz
head(wt) #para leer las primeras líneas
	#el tipo de datos que tenemos es una matriz 

# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean = apply(wt, 1, mean) #para calcular la media sobre el gen wt, generando una variable nueva
head(wt.mean) #para ver las priemras líneas
ko.mean = apply(ko, 1, mean) #para calcular la media sobre el gen ko, generando una variable nueva
head(ko.mean) #para ver las primeras líneas

# ¿Cuál es la media más alta?
max(wt.mean)
max(ko.mean)
	#la media más alta la tiene ko (37460.5)

# Ahora hacemos un scatter plot (gráfico de dispersión)
plot(ko.mean ~ wt.mean) #compara unas medias en el eje x(wt) con el eje y(ko)
plot(ko.mean ~ wt.mean, xlab = "WT", ylab = "KO", main = "GSE5583 - Scatter")

# Añadir una línea diagonal con abline
abline(0, 1, col = "red")#para añadir una linea diagonal de color rojo
abline(h=2, col="blue")#para poner una linea horizontal en color azul
abline(v=5, col="green")#para poner una línea vertical en color verde

# ¿Eres capaz de añadirle un grid?
grid()#para añadir una cuadrícula

# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean #para calcular las diferencias entre las medias de las condiciones entre wt y ko

# Hacemos un histograma de las diferencias de medias
hist(diff.mean,col="pink") #para hacer un histograma de color rosa de las diferencias de las medias entre wt y ko

# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué?
# ¿Cuántas valores tiene cada condición?

pvalue = NULL #para crear una lista vacía p-value
tstat = NULL #para crear una lista vacía de las estadísticas del test
for(i in 1 : nrow(data)) {#Para cada gen
	x = wt[i,] # gene wt número i
	y = ko[i,] # gene ko número i

	# Hacemos el test
	t = t.test(x, y)

	# Añadimos el p-value a la lista
	pvalue[i] = t$p.value
	# Añadimos las estadísticas a la lista
	tstat[i] = t$statistic
}

head(pvalue)#para mirar las primeras filas
length (pvalue)#para ver la longitud
	#Usamos los datos sin transformar porque son más fiables
	#Hay 2 condiciones, y cada una tiene 3, por lo que hay 6 en total

# Ahora comprobamos que hemos hecho TODOS los cálculos


# Hacemos un histograma de los p-values.
# ¿Qué pasa si le ponemos con una transformación de -log10?
hist(pvalue) #para hacer un histograma de p-values
hist(-log10(pvalue), col = "gray")#para hacer un histograma de color gris, transformando los pvalues en -log10
	#reducimos el gráfico a los valores que nos interesan y lo cambiamos de color

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano")#para hacer un volcano plot

# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?
diff.mean_cutoff = 2 #para establecer una diferencia significativa de 2
pvalue_cutoff = 0.01 #para establecer un p-value de 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)#para agregar una línea vertical de color azul, que tendrá un ancho de 3 unidades
#abline(v = -diff.mean_cutoff, col = "red", lwd = 3) 
abline(h = -log10(pvalue_cutoff), col = "green", lwd = 3)#para gregar una linea horizontal de color verde

# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff #para hacer el filtro para la diferencia de medias
dim(data[filter_by_diff.mean, ]) #para buscar los genes con la diferencia significativa de 2

# Ahora el filtro de p-value
filter_by_pvalue = pvalue <=pvalue_cutoff #para filtrar datos y crear un nuevo conjunto de datos que contiene aquellos en los que el valor de pvalue es menor o igual que el valor de pvalue_cutoff
dim(data[filter_by_pvalue, ]) #da las dimensiones del subconjunto de los datos

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios?
filter_combined = filter_by_diff.mean & filter_by_pvalue #para hacer el filtro combinado
filtered = data[filter_combined,] #contiene solo las filas del conjunto de datos original que cumplen con el filtro combinado
dim(filtered)#para obtener las dimensiones 
head(filtered)#para ver la primeras lineas
	# los combinamos para que tengan ambos valores
	# hay 426 genes que cumplen los dos criterios

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #2) #para crear otro volcano plot
points (diff.mean[filter_combined], -log10(pvalue[filter_combined]),col = "red") #para agregar puntos rojos

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #3") #para crear otro volcano plot
points (diff.mean[filter_combined & diff.mean < 0], 
	-log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points(diff.mean[filter_combined & diff.mean > 0],
	-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue") #para superponer puntos rojos y azules con el objetivo de resaltar registros específicos
#diff.mean = wt.mean - ko.mean	
	#parece que están al revés proque es negativo

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
heatmap(filtered)

rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Row=row, Colv=colv, cexCol=0.7,labRow=FALSE)
#cexCol es el tamaño de la letra del eje x
#Colv y Row son los dendograms
#heatmap(filtered, Row=row, Colv=colv, cexCol=0.7, col=hcl.colors(50))


# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)
library(RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row"

# Lo guardamos en un archivo PDF
pdf("GSE5583_DE_Heatmap.pdf")
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row", labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
+ col = redgreen(75), scale = "row",labRow=FALSE)

# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table (filtered, "GSE5583_DE.txt", sep = "\t",
	quote = FALSE)
