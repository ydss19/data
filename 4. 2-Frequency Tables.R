# Cargar el conjunto de datos mtcars
data(mtcars)

head(mtcars)
tail(mtcars)
dim(mtcars)
summary(mtcars)

# Obtener los valores únicos de la variable "cyl" en el conjunto de datos mtcars
valores_cilindros <- unique(mtcars$cyl)
print(valores_cilindros)
# Generar automáticamente los nombres para los niveles de cilindros
nombres_cilindros <- paste(valores_cilindros, "cilindros")
print(nombres_cilindros)

# Un factor es un tipo de dato en R que representa una variable categórica o 
# de factor. Lo que nos permite tratar los datos como niveles categóricos 
# en lugar de valores numéricos.
cilindros_factor = factor(mtcars$cyl, levels = valores_cilindros, labels = nombres_cilindros)
# Crear una tabla de frecuencias con nombres descriptivos para la variable "cyl"
tabla_cilindros <- table(cilindros_factor)
# Mostrar tabla de frecuencias para "cyl"
print("Tabla de frecuencias para la variable 'cyl' (Número de cilindros):")
print(tabla_cilindros)

# Calcular las frecuencias relativas
frecuencia_relativa <- prop.table(tabla_cilindros)
print("Frecuencia relativa para la variable 'cyl':")
print(frecuencia_relativa)

# Calcular las frecuencias acumuladas
frecuencia_acumulada <- cumsum(tabla_cilindros)
# Calcular las frecuencias relativas acumuladas
frecuencia_relativa_acumulada <- cumsum(frecuencia_relativa)

# Crear una tabla combinada con frecuencia absoluta y relativa
tabla_combinada <- cbind(FrecuenciaAbsoluta = tabla_cilindros, 
                         FrecuenciaRelativa = frecuencia_relativa,
                         FrecuenciaAcumulada = frecuencia_acumulada,
                         FrecuenciaRelativaAcumulada = frecuencia_relativa_acumulada)

# Mostrar la tabla combinada
print("Tabla de frecuencias para la variable 'cyl' (Número de cilindros):")
print(tabla_combinada)

###############################################################################
###############################################################################
###############################################################################

# Definir el número de intervalos (bins) para discretizar los datos
num_bins <- 5 

# Crear intervalos de ancho igual para discretizar la variable "mpg"
intervalos <- cut(mtcars$mpg, breaks = num_bins, include.lowest = TRUE)

# Crear tabla de frecuencias para la variable "mpg"
tabla_mpg <- table(intervalos)

# Calcular las frecuencias relativas para la variable "mpg"
frecuencia_relativa <- prop.table(tabla_mpg)

# Calcular las frecuencias acumuladas para la variable "mpg"
frecuencia_acumulada <- cumsum(tabla_mpg)

# Calcular las frecuencias relativas acumuladas para la variable "mpg"
frecuencia_relativa_acumulada <- cumsum(frecuencia_relativa)

# Crear una tabla combinada con frecuencia absoluta, relativa, acumulada y relativa acumulada
tabla_combinada <- cbind(FrecuenciaAbsoluta = tabla_mpg,
                         FrecuenciaRelativa = frecuencia_relativa,
                         FrecuenciaAcumulada = frecuencia_acumulada,
                         FrecuenciaRelativaAcumulada = frecuencia_relativa_acumulada)

# Mostrar la tabla combinada
print("Tabla de frecuencias para la variable 'mpg' (Millas por galón):")
print(tabla_combinada)


