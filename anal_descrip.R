##############################################################
#  ANÁLISIS DESCRIPTIVO - datos.xlsx
#  121 niños (5 - 9 años): antropometría, MABC, cuestionario
#  TDC, Foot Posture Index (FPI) y composición corporal (DXA)
# Trastorno del Desarrollo de la Coordinación (TDC/DCD)
##############################################################

##### Datos #####
library(readxl)

url <- "https://raw.githubusercontent.com/Paulinagv1802/Articulo_regresion/main/datos.xlsx"

archivo <- tempfile(fileext = ".xlsx")
download.file(url = url, destfile = archivo, mode = "wb", quiet = TRUE)
datos <- read_excel(archivo)
unlink(archivo)
head(datos)
dim(datos)

# Renombramos para evitar problemas con caracteres especiales:
# Renombramos (35 columnas, en el mismo orden del Excel)
colnames(datos) <- c(
  "Sujeto", "Edad", "Genero", "Altura", "Peso", "Long_pierna",
  "MABC", "TDC",
  "FPI_L_RF_1", "FPI_L_RF_2", "FPI_L_RF_3",
  "FPI_L_FF_1", "FPI_L_FF_2", "FPI_L_FF_3", "FPI_L_total",
  "FPI_R_RF_1", "FPI_R_RF_2", "FPI_R_RF_3",
  "FPI_R_FF_1", "FPI_R_FF_2", "FPI_R_FF_3", "FPI_R_total",
  "Pi_grasa_g",      # 1a columna DXA (asumo pierna izquierda, masa grasa)
  "Pi_magra_g", "Pi_total_g", "Pi_grasa_pct",
  "Pd_grasa_g", "Pd_magra_g", "Pd_total_g", "Pd_grasa_pct",
  "Tot_grasa_g", "Tot_magra_g", "Tot_total_g", "Tot_grasa_pct",
  "Grasa_estatura"   # masa grasa DXA / estatura^2 (kg/m2)
)

str(datos)
datos$Genero <- factor(datos$Genero, levels = c("Female", "Male"),
                       labels = c("Mujer", "Hombre"))

# Edad en años cumplidos (sin decimales): 5.54 -> 5, 9.96 -> 9
datos$Edad <- floor(datos$Edad)


##### 2. Calidad de datos #####

# Valores faltantes por variable
colSums(is.na(datos))

# IDs duplicados
anyDuplicated(datos$Sujeto)


# El total de masa (g) debe ser ~ grasa + magra
# + contenido mineral óseo, ~600-1200 g:

dif_dxa <- datos$Tot_total_g - (datos$Tot_grasa_g + datos$Tot_magra_g)
summary(dif_dxa)
datos[which(dif_dxa > 3000), c("Sujeto", "Tot_grasa_g", "Tot_magra_g", "Tot_total_g")]





##### 3. Variables derivadas #####


# IMC (altura en metros)
datos$IMC <- datos$Peso / datos$Altura^2

# Categoría MABC (percentil): <= 5 TDC probable, 6-15 en riesgo, > 15 típico (Obtenido del articulo)
datos$MABC_cat <- cut(datos$MABC, breaks = c(-Inf, 5, 15, Inf),
                      labels = c("TDC probable (<=5)", "En riesgo (6-15)", "Típico (>15)"))


# Categoría FPI (suma de 6 criterios): < 0 supinado, 0-5 normal, 6-9 pronado, >= 10 muy pronado
cat_fpi <- function(x) cut(x, breaks = c(-Inf, -1, 5, 9, Inf),
                           labels = c("Supinado", "Normal", "Pronado", "Muy pronado"))

datos$FPI_L_cat <- cat_fpi(datos$FPI_L_total)
datos$FPI_R_cat <- cat_fpi(datos$FPI_R_total)


# Variables numéricas principales
vars_num <- c("Edad", "Altura", "Peso", "IMC", "Long_pierna", "MABC", "TDC",
              "FPI_L_total", "FPI_R_total",
              "Pi_grasa_pct", "Pd_grasa_pct", "Tot_grasa_pct",
              "Tot_grasa_g", "Tot_magra_g", "Tot_total_g", "Grasa_estatura")






##### 4. Medidas de tendencia central y variabilidad #####

# Utilizamos funciones para no tener que aplicar lo mismo
# a cada una de las variables
calcular_moda <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


asimetria <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }
curtosis  <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^4) / sd(x)^4 - 3 }  # exceso


resumen_num <- function(x) {
  x <- x[!is.na(x)]
  q <- quantile(x, c(0.25, 0.75))
  c(n = length(x),
    Media = mean(x), Mediana = median(x), Moda = calcular_moda(x),
    DE = sd(x), Varianza = var(x), CV_pct = sd(x) / mean(x) * 100,
    Min = min(x), Max = max(x), Rango = max(x) - min(x),
    Q1 = q[[1]], Q3 = q[[2]], RIC = q[[2]] - q[[1]],
    Asimetria = asimetria(x), Curtosis = curtosis(x),
    Outliers = length(boxplot.stats(x)$out),
    Shapiro_p = shapiro.test(x)$p.value)
}

tabla_resumen <- round(t(sapply(datos[vars_num], resumen_num)), 3)
print(tabla_resumen)


# Analisis con la grasa corporal total:
cat("Resultados para el % de grasa corporal total:\n",
    "Media:", mean(datos$Tot_grasa_pct), "\n",
    "Mediana:", median(datos$Tot_grasa_pct), "\n",
    "Moda:", calcular_moda(datos$Tot_grasa_pct), "\n")


# Cuartiles, deciles y percentiles
quantile(datos$Tot_grasa_pct, probs = c(0.25, 0.50, 0.75))
quantile(datos$Tot_grasa_pct, probs = seq(0.1, 0.9, by = 0.1))
quantile(datos$Tot_grasa_pct, probs = c(0.01, 0.05, 0.95, 0.99))


# Sabemos que: Asimetría cercana a 0 -> simétrica; > 0 cola a la derecha.
# Shapiro_p > 0.05 -> no se rechaza normalidad.





##### 5. Datos atípicos #####


for (v in vars_num) {
  out <- boxplot.stats(datos[[v]])$out
  if (length(out) > 0) {
    ids <- datos$Sujeto[datos[[v]] %in% out & !is.na(datos[[v]])]
    cat(v, "-> atípicos:", paste(round(out, 2), collapse = ", "),
        "| Sujetos:", paste(ids, collapse = ", "), "\n")
  }
}





##### 6. Distribución de frecuencias: variables categóricas #####

tabla_frec <- function(f) {
  fa <- table(f)
  data.frame(Categoria = names(fa),
             Frec_Absoluta = as.numeric(fa),
             Frec_Relativa = round(as.numeric(prop.table(fa)), 3),
             Porcentaje = round(as.numeric(prop.table(fa)) * 100, 1))
}

tabla_frec(datos$Genero)
tabla_frec(datos$MABC_cat)
tabla_frec(datos$FPI_L_cat)
tabla_frec(datos$FPI_R_cat)
tabla_frec(datos$Edad_anios)


# Tablas cruzadas
addmargins(table(datos$Genero, datos$MABC_cat))
round(prop.table(table(datos$Genero, datos$MABC_cat), margin = 1) * 100, 1)  # % por género






##### 7. Distribución de frecuencias: variables continuas #####

if (!requireNamespace("fdth", quietly = TRUE)) 
install.packages("fdth")
library(fdth)

tabla_frec_continua <- fdt(datos$Tot_grasa_pct)
print(tabla_frec_continua)
plot(tabla_frec_continua, type = "fh")





##### 8. Histogramas (con curva de densidad) #####

# Etiquetas con unidades:
etiquetas <- c(Edad = "Edad (años)", Altura = "Altura (m)", Peso = "Peso (kg)",
               IMC = "IMC (kg/m²)", Long_pierna = "Longitud de pierna (cm)",
               MABC = "MABC (percentil)", TDC = "Cuestionario TDC (puntaje)",
               FPI_L_total = "FPI izquierdo (total)", FPI_R_total = "FPI derecho (total)",
               Pi_grasa_pct = "Grasa pierna izq. (%)", Pd_grasa_pct = "Grasa pierna der. (%)",
               Tot_grasa_pct = "Grasa corporal total (%)",
               Tot_grasa_g = "Masa grasa total (g)", Tot_magra_g = "Masa magra total (g)",
               Tot_total_g = "Masa total (g)", Grasa_estatura = "Masa grasa / estatura² (kg/m²)")


par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
for (v in c("Edad", "IMC", "Tot_grasa_pct", "MABC")) {
  hist(datos[[v]], freq = FALSE, main = paste("Distribución de", etiquetas[v]),
       xlab = etiquetas[v], ylab = "Densidad", col = "lightblue", border = "black")
  lines(density(datos[[v]], na.rm = TRUE), col = "red", lwd = 2)
}
par(mfrow = c(1, 1))






##### 9. Boxplots #####

# Individuales
par(mfrow = c(2, 2))
for (v in c("Peso", "IMC", "Tot_grasa_pct", "TDC")) {
  boxplot(datos[[v]], main = paste("Diagrama de caja:", etiquetas[v]),
          ylab = etiquetas[v], col = "skyblue")
}
par(mfrow = c(1, 1))

# Comparativos por género
par(mfrow = c(2, 3))
for (v in c("Edad", "Peso", "IMC", "Tot_grasa_pct", "MABC", "FPI_R_total")) {
  boxplot(datos[[v]] ~ datos$Genero, main = paste(etiquetas[v], "por género"),
          xlab = "Género", ylab = etiquetas[v], col = c("pink", "lightblue"))
}






##### 10. Gráficos de barras #####

par(mfrow = c(1, 3))
barplot(table(datos$Genero), main = "Distribución por género",
        xlab = "Género", ylab = "Cantidad", col = c("#E7298A", "#1B9E77"))

barplot(table(datos$MABC_cat), main = "Categoría MABC",
        xlab = "", ylab = "Cantidad", col = c("#440154", "#2A788E", "#7AD151"), las = 2)

barplot(table(datos$FPI_R_cat), main = "Postura del pie (FPI derecho)",
        xlab = "", ylab = "Cantidad", col = c("#2A788E", "#22A884", "#FDE725"), las = 2)
par(mfrow = c(1, 1))







##### 11. Normalidad (QQ-plots) #####

par(mfrow = c(2, 2))
for (v in c("Edad", "IMC", "Tot_grasa_pct", "TDC")) {
  qqnorm(datos[[v]], main = paste("QQ-plot:", etiquetas[v]), pch = 19, col = "steelblue")
  qqline(datos[[v]], col = "red", lwd = 2)
}
par(mfrow = c(1, 1))







##### 12. Comparación por género #####

res_genero <- t(sapply(vars_num, function(v) {
  x <- datos[[v]]
  g <- datos$Genero
  c(Media_Mujer  = mean(x[g == "Mujer"],  na.rm = TRUE),
    Media_Hombre = mean(x[g == "Hombre"], na.rm = TRUE),
    DE_Mujer     = sd(x[g == "Mujer"],  na.rm = TRUE),
    DE_Hombre    = sd(x[g == "Hombre"], na.rm = TRUE),
    p_Wilcoxon   = wilcox.test(x ~ g, exact = FALSE)$p.value)
}))
print(round(res_genero, 3))
# p < 0.05 -> diferencia significativa entre mujeres y hombres





##### 13. Correlaciones #####

# Diagramas de dispersión relevantes
par(mfrow = c(1, 3))
plot(datos$Peso, datos$Tot_total_g / 1000, pch = 19, col = "steelblue",
     xlab = "Peso (kg)", ylab = "Masa total DXA (kg)", main = "Peso vs. masa DXA")
abline(lm(I(Tot_total_g / 1000) ~ Peso, data = datos), col = "red", lwd = 2)


plot(datos$IMC, datos$Tot_grasa_pct, pch = 19, col = "steelblue",
     xlab = "IMC (kg/m²)", ylab = "Grasa corporal (%)", main = "IMC vs. % grasa")
abline(lm(Tot_grasa_pct ~ IMC, data = datos), col = "red", lwd = 2)


plot(datos$TDC, datos$MABC, pch = 19, col = "steelblue",
     xlab = "Cuestionario TDC", ylab = "MABC (percentil)", main = "TDC vs. MABC")
abline(lm(MABC ~ TDC, data = datos), col = "red", lwd = 2)
par(mfrow = c(1, 1))

