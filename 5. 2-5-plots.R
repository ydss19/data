# Cargar el conjunto de datos mtcars
data(mtcars)

###########################################################################
# 1. Diagrama de Barras de Frecuencia Absoluta de Cilindros (cyl)
barplot(table(mtcars$cyl), main = "Diagrama de Barras de Cilindros", xlab = "Cilindros", ylab = "Frecuencia")


###########################################################################
# 2. Histograma de Millas por Galón (mpg)
# Vamos a mostrar un histograma básico para las millas por galón (mpg)
# Este histograma se generará con un número predeterminado de bins que es calculado automáticamente, de una forma similar al método de Sturges.
hist(mtcars$mpg, main = "Histograma Básico de Millas por Galón", 
     xlab = "Millas por Galón", ylab = "Frecuencia")

# #### HISTOGRAMA CON EL MÉTODO DE STURGES ####
# Método de Sturges:
# El número de bins se calcula con la fórmula:
#   k = ⌈ log2(n) + 1 ⌉
# Este método asume que los datos siguen una distribución normal y utiliza solo el tamaño de la muestra n.
# Es adecuado para conjuntos de datos pequeños a medianos.

num_bins_sturges <- ceiling(log2(num_observaciones) + 1)

# Crear el histograma con el número de bins calculado por Sturges
hist(mtcars$mpg, breaks = num_bins_sturges, 
     main = "Histograma con Método de Sturges", 
     xlab = "Millas por Galón", ylab = "Frecuencia")

# Explicación del método de Sturges:
# Este método se basa solo en el tamaño de la muestra. A medida que el tamaño de la muestra aumenta, también lo hace el número de bins.
# Es adecuado para distribuciones normales, pero puede no ser eficaz con grandes volúmenes de datos o distribuciones no normales.


# #### HISTOGRAMA CON EL MÉTODO DE SCOTT ####
# Método de Scott:
# El ancho de los bins se calcula con la fórmula:
#   h = (3.5 * desviación estándar) / (n^(1/3))
# Esto minimiza el sesgo al ajustarse a la dispersión de los datos.
# Este método es útil cuando se supone que los datos siguen una distribución normal.
rango_datos <- diff(range(mtcars$mpg))
desviacion_estandar <- sd(mtcars$mpg)
num_observaciones <- length(mtcars$mpg)
ancho_bins_scott <- (3.5 * desviacion_estandar) / (num_observaciones^(1/3))
num_bins_scott <- round(rango_datos / ancho_bins_scott)

# Crear el histograma con el número de bins calculado por Scott
hist(mtcars$mpg, breaks = num_bins_scott, 
     main = "Histograma con Método de Scott", 
     xlab = "Millas por Galón", ylab = "Frecuencia")

# Explicación del método de Scott:
# Este método utiliza la desviación estándar de los datos para calcular el ancho de los bins.
# A medida que la desviación estándar aumenta, el ancho de los bins también aumentará, lo que llevará a menos bins.
# Esto es útil cuando los datos son bastante dispersos.


# #### HISTOGRAMA CON EL MÉTODO DE FREEDMAN-DIACONIS ####
# Método de Freedman-Diaconis:
# El ancho de los bins se calcula con la fórmula:
#   h = (2 * IQR) / (n^(1/3))
# donde IQR es el rango intercuartil (la diferencia entre el cuartil 75 y el cuartil 25).
# Este método es más robusto frente a valores atípicos y distribuciones sesgadas.

iqr <- IQR(mtcars$mpg)
ancho_bins_fd <- 2 * iqr / (num_observaciones^(1/3))
num_bins_fd <- round(rango_datos / ancho_bins_fd)

# Crear el histograma con el número de bins calculado por Freedman-Diaconis
hist(mtcars$mpg, breaks = num_bins_fd, 
     main = "Histograma con Método de Freedman-Diaconis", 
     xlab = "Millas por Galón", ylab = "Frecuencia")

# Explicación del método de Freedman-Diaconis:
# Este método utiliza el rango intercuartil (IQR), que es más robusto frente a los valores atípicos y distribuciones sesgadas.
# En lugar de la desviación estándar, como en el método de Scott, usa el IQR para calcular el ancho de los bins, lo que lo hace más adecuado para datos con distribuciones no normales o con valores atípicos.

###########################################################################
# 3. Diagrama de Dispersión entre Millas por Galón (mpg) y Caballos de Fuerza (hp)
plot(mtcars$hp, mtcars$mpg, main = "Diagrama de Dispersión\nentre Caballos de Fuerza y Millas por Galón", xlab = "Caballos de Fuerza", ylab = "Millas por galón")


###########################################################################
# 4. Gráfico de Pastel de Tipo de Transmisión (am)
pie(table(mtcars$am), main = "Gráfico de Pastel de Tipo de Transmisión", labels = c("Automática", "Manual"))

###########################################################################
# 5. Diagrama de Caja de Peso (wt) por Tipo de Transmisión (am)
boxplot(wt ~ am, data = mtcars, main = "Diagrama de Caja de Peso por Tipo de Transmisión", xlab = "Tipo de Transmisión", ylab = "Peso (miles de libras)")

# Vamos a alegrar la tarea al analista
boxplot(wt ~ am, data = mtcars, 
        main = "Diagrama de Caja de Peso por Tipo de Transmisión", 
        xlab = "Tipo de Transmisión", 
        ylab = "Peso (miles de libras)", 
        col = c("skyblue", "salmon"))  # Colores para cada tipo de transmisión

# Agregar la leyenda
legend("topright",                            # Posición de la leyenda (topright)
       legend = c("Automática", "Manual"),    # Las etiquetas
       fill = c("skyblue", "salmon"),         # Los colores correspondientes
       title = "Tipo de Transmisión")         # Título de la leyenda

###########################################################################
# 6. Crear una tabla de contingencia de las variables "am" y "cyl"
am_factor <- factor(mtcars$am, labels = c("Automática", "Manual"))
tabla_contingencia <- table(am_factor, mtcars$cyl)
print(tabla_contingencia)

# Crear un gráfico de barras para mostrar las dos variables categóricas
barplot(tabla_contingencia, beside = TRUE,
        main = "Cantidad de Automóviles por Tipo de Transmisión y Número de Cilindros",
        xlab = "Número de Cilindros", ylab = "Cantidad de Automóviles",
        col = c("skyblue", "salmon"), ylim = c(0, max(tabla_contingencia) + 2))


# Definir las etiquetas de la leyenda
etiquetas_leyenda <- rownames(tabla_contingencia)

# Agregar la leyenda centrada manualmente
legend("top", legend = etiquetas_leyenda, fill = c("skyblue", "salmon"), title = "Tipo de Transmisión", horiz = TRUE, cex = 0.7)