library(readxl)

##### Datos #####
datos <- read_excel("C:/Users/USUARIO/Desktop/Articulo/datos.xlsx")

# Renombramos para evitar problemas con caracteres especiales:
colnames(datos) <- c("Grupo", "Masa", "Diametro", "EM_FM")




##### Media, Moda y promedio ####

datos$EM_FM <- as.numeric(datos$EM_FM)

# Media (Promedio)
media_masa <- mean(datos$Masa)
media_diametro <- mean(datos$Diametro)
media_emfm <- mean(datos$EM_FM)

# Mediana (el valor central)
mediana_masa <- median(datos$Masa)
mediana_diametro <- median(datos$Diametro)
mediana_emfm <- median(datos$EM_FM)

# Moda
calcular_moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

moda_masa <- calcular_moda(datos$Masa)
moda_diametro <- calcular_moda(datos$Diametro)
moda_emfm <- calcular_moda(datos$EM_FM)

# Resultados:
cat("Resultados para la Fuerza Máxima (EM-FM):\n",
    "Media:", media_emfm, "\n",
    "Mediana:", mediana_emfm, "\n",
    "Moda:", moda_emfm, "\n")

"Podemos notar que la media y la mediana son muy cercanos esto es un 
buen indicador de que los datos de la fuerza máxima tienden a ser simétricos."

##### Medidas de variabilidad #####

# Medidas básicas con EM_FM
rango_emfm <- max(datos$EM_FM) - min(datos$EM_FM)
varianza_emfm <- var(datos$EM_FM)
desviacion_emfm <- sd(datos$EM_FM)

# Cuartiles, Deciles y Percentiles
cuartiles <- quantile(datos$EM_FM, probs = c(0.25, 0.50, 0.75))
deciles <- quantile(datos$EM_FM, probs = seq(0.1, 0.9, by = 0.1))
percentiles <- quantile(datos$EM_FM, probs = c(0.01, 0.05, 0.95, 0.99))

# Datos atípicos mediante boxplot
outliers_emfm <- boxplot.stats(datos$EM_FM)$out


"Tenemos una variable categórica (Grupo) y 3 variables continuas 
(Masa, Diámetro, EM-FM)."



#### Distribución de frecuencias para la variable categórica (Grupo) ####

# Calculamos la frecuencia absoluta:
frec_absoluta_grupo <- table(datos$Grupo)

# Calculamos la frecuencia relativa:
frec_relativa_grupo <- prop.table(frec_absoluta_grupo)

# Unimos ambas en una tabla ordenada
tabla_grupo <- data.frame(
  Frecuencia_Absoluta = as.numeric(frec_absoluta_grupo),
  Frecuencia_Relativa = as.numeric(frec_relativa_grupo),
  Porcentaje = as.numeric(frec_relativa_grupo) * 100
)
rownames(tabla_grupo) <- names(frec_absoluta_grupo)

print(tabla_grupo)




#### Distribución de frecuencias para variables continuas: #####

install.packages("fdth")
library(fdth)

# Generamos la tabla de distribución de frecuencias completa:
tabla_frec_continua <- fdt(datos$EM_FM)
print(tabla_frec_continua)

plot(tabla_frec_continua, type="fh")  # Histograma de frecuencias




##### Boxplot #####

# Para la Fuerza Máxima
boxplot(datos$EM_FM, 
        main = "Diagrama de Caja: Fuerza Máxima", 
        ylab = "EM-FM (N)", 
        col = "skyblue")

# Para la Masa
boxplot(datos$Masa, 
        main = "Diagrama de Caja: Masa", 
        ylab = "Masa (g)", 
        col = "skyblue")

# Para el diametro
boxplot(datos$Masa, 
        main = "Diagrama de Caja: Diametro", 
        ylab = "Diametro (cm)", 
        col = "skyblue")


# Boxplot comparativo EM_FM por Grupo 
boxplot(EM_FM ~ Grupo, data = datos, 
        main = "Fuerza Máxima por Grupo Experimental", 
        xlab = "Grupo", 
        ylab = "EM-FM (N)", 
        col = "lightgreen")

# Boxplot comparativo diametro por Grupo 
boxplot(Diametro ~ Grupo, data = datos, 
        main = "Diametro por Grupo Experimental", 
        xlab = "Grupo", 
        ylab = "Diametro (cm)", 
        col = "lightgreen")

# Boxplot comparativo Masa por Grupo 
boxplot(Masa ~ Grupo, data = datos, 
        main = "Masa por Grupo Experimental", 
        xlab = "Grupo", 
        ylab = "Masa (g)", 
        col = "lightgreen")




##### Histogramas #####

hist(datos$EM_FM, 
     main = "Distribución de Fuerza Máxima (EM-FM)", 
     xlab = "EM-FM (N)", 
     ylab = "Frecuencia",
     col = "lightblue", 
     border = "black")

hist(datos$Diametro, 
     main = "Distribución del diametro", 
     xlab = "Diametro (cm)", 
     ylab = "Frecuencia",
     col = "lightblue", 
     border = "black")

hist(datos$Masa, 
     main = "Distribución de la masa", 
     xlab = "masa (g)", 
     ylab = "Frecuencia",
     col = "lightblue", 
     border = "black")





###### Grafico de barras #####

frecuencias_grupo <- table(datos$Grupo)

# Grafico de frecuencia:
barplot(frecuencias_grupo, 
        main = "Distribución por Grupo Experimental", 
        xlab = "Grupo", 
        ylab = "Cantidad de Observaciones", 
        col = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"))

"Podemos observar que el grupo A y D tienen la mayor cantidad
de observaciones"