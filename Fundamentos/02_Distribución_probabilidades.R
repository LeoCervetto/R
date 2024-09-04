# Script para Distribuciones de Probabilidad en R 
# Autor: Lcervett0

# Descripción: Este script cubre cómo calcular probabilidades, generar números aleatorios,
#              y visualizar distribuciones de probabilidad en R, con ejemplos aplicados.


# Distribución Normal --------------------------------------------------------


# Calcular la probabilidad de  un valor en una distribución normal estándar
prob_normal <- pnorm(1.6)
prob_normal

# Calcular el percentil 75 con media 100 y desviación estándar 15
percentil_normal <- qnorm(0.75, mean = 100, sd = 15)
percentil_normal  

# Números aleatorios
random_normal <- rnorm(1000, mean = 100, sd = 15)
random_normal

# Gráfico
hist(random_normal, 
     breaks = 30, 
     col = "blue", 
     main = "Distribución Normal",
     xlab = "Valor", 
     ylab = "Frecuencia")



# Distribución de Poisson -----------------------------------------------------

# Calcular la probabilidad de 3 eventos cuando la media (lambda) es 2
prob_poisson <- dpois(3, lambda = 2)
prob_poisson  

# Números aleatorios con lambda = 2
random_poisson <- rpois(100, lambda = 2)

# Gráfico 
hist(random_poisson, 
     breaks = 9, 
     col = "blue", 
     main = "Distribución de Poisson", 
     xlab = "Número de eventos", 
     ylab = "Frecuencia")



# Distribución Exponencial ----------------------------------------------------

# Calcular la probabilidad acumulada de un valor < 2 eventos con lambda = 1 evento prom por minutos/hora
prob_exponencial <- pexp(2, rate = 1)
prob_exponencial  

# Números aleatorios 
random_exponencial <- rexp(100, rate = 1)

# Gráfico 
hist(random_exponencial, breaks = 30,
     col = "blue", 
     main = "Distribución Exponencial",
     xlab = "Valor", 
     ylab = "Frecuencia")



# Distribución Hipergeométrica ------------------------------------------------

# Calcular la probabilidad de 7 éxitos en una muestra de tamaño 12
# con una población de 200 (50 éxitos y 150 fracasos)
prob_hipergeometrica <- dhyper(7, m = 50, n = 150, k = 12)
prob_hipergeometrica 

# Números aleatorios
random_hipergeometrica <- rhyper(500, m = 50, n = 150, k = 12)

# Gráfico 
hist(random_hipergeometrica,
     breaks = 8, 
     col = "blue", 
     main = "Distribución Hipergeométrica", 
     xlab = "Número de éxitos",
     ylab = "Frecuencia")



# Distribución Uniforme -------------------------------------------------------

# Calcular la probabilidad que un poceso dure menos de 15 min cuando normalmente
# tarda entre 10 min y 20 min  
prob_uniforme <- punif(15, min = 10, max = 20)
prob_uniforme

# Números aleatorios
random_uniforme <- runif(1000, min = 5, max = 10)

# Gráfico 
hist(random_uniforme, 
     breaks = 30, 
     col = "blue", 
     main = "Distribución Uniforme",
     xlab = "Valor",
     ylab = "Frecuencia")


