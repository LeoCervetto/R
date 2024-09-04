# Script Básico de R para Operaciones y Visualización
# Autor: Lcervetto

# Descripción: Este script cubre operaciones básicas, manejo de variables, tipos de datos, 
#              estadística descriptiva y visualización en R.


# Uso como calculadora --------------------------------------------------------

# Suma
4 + 8

# Resta
7 - 3

# Multiplicación
6 * 7

# División
20 / 5

# Exponencial
3 ** 3

# Operación compleja
(3 + 9) / (2 * 2)


# Asignar Variables -----------------------------------------------------------

mi_variable <- 50

# Impresión explícita
print(mi_variable)

# Impresión implícita
mi_variable

# Operación entre variables 
x <-  40
y <- 10

x + y

# Ver y eliminar variables
ls()
rm(mi_variable)


# Estructuras para almacenar datos --------------------------------------------------------------

# Conjunto unidimensional 
conjunto_1 <- c(20, 26, 32, 40, 50)
conjunto_2 <- seq(2, 200, 2)
conjunto_3 <- array(1:5, 10)

# Conjunto multidimensional
conjunto_4 <- matrix(1:12, nrow = 4, ncol = 3)
conjunto_5 <- data.frame(col1 = 1:12, col2 = 5 * 2, col3 = 3)

# Listas
lista_1 <- list(conjunto_1, conjunto_4, conjunto_3)


# Estadística descriptiva -----------------------------------------------------

# Ejemplo
ventas <- c(100, 95, 103, 105, 112)
costos <- c(90, 80, 97, 93, 100)

Control_diario <- data.frame(ventas, costos)

# Añadir columna de utilidad
Control_diario["utilidad"] <- Control_diario$ventas - Control_diario$costos

# Cálculos básicos
attach(Control_diario)
sum(ventas)
mean(ventas)
max(ventas)
min(ventas)
length(ventas)
sd(ventas)
var(ventas)
quantile(ventas, 0.75)
summary(ventas)

# Seleccionar 3er cuartil
summary(ventas)[5]

# Desvincular el dataframe
detach(Control_diario)


# Gráficos --------------------------------------------------------------------

# Barplot
barplot(ventas, main = "Ventas Diarias", xlab = "Día", ylab = "Ventas")

# Boxplot
boxplot(ventas, main = "Distribución de Ventas")

# Histograma
hist(ventas, main = "Histograma de Ventas", xlab = "Ventas", ylab = "Frecuencia", col = "lightblue")

# Gráfico de Puntos
plot(ventas, main = "Ventas Diarias", xlab = "Día", ylab = "Ventas", pch = 19)

# Línea
plot(ventas, type = "l", main = "Ventas en el Tiempo", xlab = "Día", ylab = "Ventas", lty = 2)

# Más parámetros
plot(ventas, 
     xlab = "Día", 
     ylab = "Ventas", 
     pch = 22, 
     col = "red", 
     lwd = 3,
     bg = "blue")

?plot
