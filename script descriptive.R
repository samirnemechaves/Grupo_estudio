# create the data

k.stores <- 20    # 20 stores
k.weeks <- 104    # 2 years of data each

# create a data frame of initially missing values to hold the data
store.df <- data.frame(matrix(NA, ncol=10, nrow=k.stores*k.weeks))
names(store.df) <- c("storeNum", "Year", "Week", "p1sales", "p2sales", 
                     "p1price", "p2price", "p1prom", "p2prom", "country")

dim(store.df)

store.num <- 101:(100+k.stores)
(store.cty <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2),
                rep("JP", 4), rep("AU", 1), rep("CN", 2)))
length(store.cty)    # make sure the country list is the right length


store.df$storeNum <- rep(store.num, each=k.weeks)
store.df$country  <- rep(store.cty, each=k.weeks)
rm(store.num, store.cty)    # clean up

(store.df$Week <- rep(1:52, times=k.stores*2))

# try the inner parts of the next line to figure out how we use rep()
(store.df$Year  <- rep(rep(1:2, each=k.weeks/2), times=k.stores))
str(store.df)

store.df$storeNum <- factor(store.df$storeNum)
store.df$country  <- factor(store.df$country)
str(store.df)

head(store.df)   # defaults to 6 rows
head(store.df, 120)  # 120 rows is enough to check 2 stores
tail(store.df, 120)  # make sure end looks OK too


# set random seed to make the random sequences replicable
set.seed(98250)  # a favorite US postal code

# promotion status, using binomial distribution, rbinom()
store.df$p1prom <- rbinom(n=nrow(store.df), size=1, p=0.1)  # 10% promoted
store.df$p2prom <- rbinom(n=nrow(store.df), size=1, p=0.15) # 15%
head(store.df)

# prices
store.df$p1price <- sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99), 
                           size=nrow(store.df), replace=TRUE)
store.df$p2price <- sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19), 
                           size=nrow(store.df), replace=TRUE)
head(store.df)

# sales data, using poisson (counts) distribution, rpois()
# first, the default sales in the absence of promotion
tmp.sales1 <- rpois(nrow(store.df), lambda=120)  # lambda = mean sales per week
tmp.sales2 <- rpois(nrow(store.df), lambda=100)  # lambda = mean sales per week

# scale sales according to the ratio of log(price)
tmp.sales1 <- tmp.sales1 * log(store.df$p2price) / log(store.df$p1price)
tmp.sales2 <- tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)

# final sales get a 30% or 40% lift when promoted
store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom*0.3))
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom*0.4))
head(store.df)

#Un comando final es útil para inspeccionar los datos porque selecciona 
#filas al azar y por lo tanto puede encontrar problemas enterrados dentro de un marco de datos lejos de la cabeza o la cola:

install.packages("car") # if needed
library(car)
some(store.df , 10)

#Luego de crear datos ahora a analizarlos

#Functions to Summarize a Variable: Discrete Variables
table(store.df$p1price)
p1.table <- table( store.df$p1price)
p1.table
#Graficar
plot(p1.table)
#tabla cruzada frecuencia se promocionó cada producto según cada precio
table(store.df$p1price , store.df$p1prom)
table(store.df$p2price , store.df$p2prom)
#De hecho, podemos calcular la fracción exacta de veces que el producto 1 está en promoción en cada punto de precio, 
#si asignamos la tabla a una variable y luego dividimos la segunda columna de la tabla por la suma de la primera y segunda columnas:
p1.table2 <- table(store.df$p1price , store.df$p1prom)
p1.table2[, 2] / (p1.table2[, 1] + p1.table2[, 2])

#Continuous Variables
min(store.df$p1sales) #numero minimo
max(store.df$p2sales) #numero maximo
mean(store.df$p1prom) #promedio
median(store.df$p2sales) #mediana (punto exacto donde todos los datos se dividen en la mitad)
var(store.df$p1sales) #variabilidad respecto a la media
sd(store.df$p1sales) #desviación estandar
IQR(store.df$p1sales) #Rango intercuartil, si dividieramos la muestra en 4 partes (25, 50, 75, 100%) y luego vemos la diferencia entre el tercer y el primer cuartil de una distribución
quantile( store.df$ p1sales , probs=c (0.25, 0.5, 0.75))#percentiles
mad(store.df$p1sales) #Desviación media absoluta (Estimador robusto de la varianza) # https://clck.ru/QzsXM
#Buscar otros cuartiles 
quantile( store.df$ p1sales , probs=c (0.05, 0.95)) # central 90% of data
quantile( store.df$ p1sales , probs =0:10/10)
#Resumen de las ventas del producto 1 y del producto 2 basado en su rango medio e intercuartil.
mysummary.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df) <- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df
#Summarizing Data Frames: Summary and Describe
#Summary
summary(store.df)
summary(store.df$Week)
summary(store.df , digits =2)#The digits= argument is helpful if you wish to change the precision of the display
#describe
install.packages ("psych")
library(psych)
#incluyendo n, el recuento de observaciones; media recortada, 
#la media después de caer una pequeña proporción de valores extremos; y estadísticas como el sesgo 
#y la curtosis que son útiles al interpretar datos con respecto a distribuciones normales.
describe(store.df) 
#describe() es especialmente recomendado para resumir los datos de la encuesta con valores discretos como 1–7 likert
describe( store.df[ , c(2, 4:9)])#seleccionar solo las variables numericas

#Apply(x=DATA, MARGIN=MARGIN, FUN=FUNCTION) ejecuta cualquier función que especifique en cada una de las filas y/o columnas de un objeto
#El término margen es una metáfora bidimensional que denota qué "dirección" desea hacer algo: 
#ya sea a lo largo de las filas (MARGIN-1) o columnas (MARGIN-2), o ambas simultáneamente (MARGIN-c(1, 2)).

#supongamos que queremos encontrar la media de cada columna de store.df, excepto store.df$Store, 
#que no es un número y por lo tanto no tiene una media
apply(store.df[ ,2:9], MARGIN=2, FUN=mean)

#Así las cosas, colMeans() hace lo mismo que el comando anterior, pero apply te da la flexibilidad de aplicar cualquier función que te guste. 
#Si queremos que la fila medias en su lugar, simplemente cambiamos el margen a 1:
apply(store.df[ ,2:9], MARGIN=1, FUN=mean)
apply(store.df[ ,2:9], MARGIN=2, FUN=sum)#Aoliquemosle suma
apply(store.df[ ,2:9], 2, sd) #Apliquemosle desviación estandar (sd)

#Imagine que estamos comprobando los datos y deseamos conocer la diferencia entre la media y la mediana de cada variable, tal vez para marcar el sesgo en los datos
apply(store.df[ ,2:9], 2, function(x) { mean(x) - median(x) } )
## creating a summary data frame using apply()
mysummary2.df <- data.frame(matrix(NA, nrow=2, ncol=2)) #Si hubiera muchos productos en lugar de solo dos cmabiariamos nrow
names(mysummary2.df) <- c("Median Sales", "IQR")
rownames(mysummary2.df) <- names(store.df)[4:5] # names from the data frame
mysummary2.df[, "Median Sales"] <- apply(store.df[, 4:5], 2, median)
mysummary2.df[, "IQR"]          <- apply(store.df[, 4:5], 2, IQR)
mysummary2.df
#Single Variable Visualization
#Histograms
hist(store.df$p1sales)
#We add the title and axis labels to our plot command:
hist(store.df$p1sales, 
main=" Product 1 Weekly Sales Frequencies, All Stores",
xlab=" Product 1 Sales ( Units)",
ylab="Count",
breaks=30, # more columns
col="lightblue") # color the bars
#mejoremos la grafica
hist(store.df$p1sales, 
main="Product 1 Weekly Sales Frequencies, All Stores",
xlab="Product 1 Sales (Units)",
ylab="Relative frequency",             # it's no londer the count!
breaks=30,                            #more columns    
col="lightblue", 
freq=FALSE,                            # freq=FALSE significa no contar para trazar la densidad
xaxt="n")                              # xaxt="n" no marcas de graduación del eje x
axis(side=1, at=seq(60, 300, by=20))        # añadir las marcas de graduación del eje x (lado 1) que queremos
lines(density(store.df$p1sales, bw=10),    # "bw= ..." ajusta el suavizado
type="l", col="darkred", lwd=2)      # lwd = anchura de la línea

#producto 2
#We add the title and axis labels to our plot command:
hist(store.df$p2sales, 
     main=" Product 2 Weekly Sales Frequencies, All Stores",
     xlab=" Product 2 Sales ( Units)",
     ylab="Count",
     breaks=30, # more columns
     col="lightblue") # color the bars
#mejoremos la grafica
hist(store.df$p2sales, 
     main="Product 2 Weekly Sales Frequencies, All Stores",
     xlab="Product 2 Sales (Units)",
     ylab="Relative frequency",             # it's no londer the count!
     breaks=30,                             # more columns
     col="lightblue",                       #color de llenado
     freq=FALSE,                            # freq=FALSE significa no contar para trazar la densidad
     xaxt="n")                              # xaxt="n" no marcas de graduación del eje x
axis(side=1, at=seq(60, 300, by=20))        # añadir las marcas de graduación del eje x (lado 1) que queremos
lines(density(store.df$p2sales, bw=10),    # "bw= ..." ajusta el suavizado
      type="l", col="darkred", lwd=2)      # lwd = anchura de la línea

#Box plot: La mediana es la línea central, mientras que la 25 y 75 los percentiles definen la caja. 
#Las líneas exteriores son bigotes en los puntos de los valores más extremos
#Los puntos más allá de los bigotes son valores atípicos dibujados como círculos individuales.
boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)

#comparar dos distribuciones
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
        ylab="Store", xlab="Weekly unit sales", las=1, #las 1 obliga a la Eso obliga a los ejes a tener texto en dirección horizontal
        main="Weekly Sales of P2 by Store")
#Existen diferencias en las promiciones en las tiendas
boxplot(store.df$p2sales ~ store.df$p2prom, horizontal=TRUE,
        ylab="Store", xlab="Weekly unit sales", las=1, #las 1 obliga a la Eso obliga a los ejes a tener texto en dirección horizontal
        main="Weekly Sales of P2 by Store")

# using data and with()
boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n", 
        ylab="P2 promoted in store?", xlab="Weekly sales",
        main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2), labels=c("No", "Yes"))
#usar beanplots para usar otra forma para ver la distribución

install.packages("beanplot") # install "beanplot" package as needed
library(beanplot)
beanplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n", 
         what=c(0,1,1,0), log="", side="second",
         ylab="P2 promoted in store?", xlab="Weekly sales",
         main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2), labels=c("No", "Yes"))

#verificar la distribución normal de una muestra (graficamente)
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

qqnorm(log(store.df$p1sales))#hacemos una conversión logaritmica que normaliza nos datos a una distribución normal
qqline(log(store.df$p1sales))#incluyamosle una linea

#generar tablas cruzadas data=DATA, INDICES=INDICES, FUN=FUNCTION). by()
#Supongamos que deseamos encontrar las ventas promedio de P1 por tienda
by(store.df$p1sales, store.df$storeNum, mean)

#Para agruparlo por más de un factor, Por ejemplo, podemos obtener la media de ventas de  p1 ventas por tienda y por año:
by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

#aggregate() que opera casi de manera idéntica a by() pero devuelve
#un marco de datos con un formato agradable. lo que nos permite graficarlo posteriormente
#aggregate(x=DATA, by=BY, FUN= FUNCTION)

aggregate(store.df$p1sales, by=list(country=store.df$country), sum)
#guardar el cuadro de datos
p1sales.sum <- aggregate(store.df$ p1sales ,
                         by=list( country=store.df$ country), sum)
