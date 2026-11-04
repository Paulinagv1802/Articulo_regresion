##############################################################
#  mod_lin.R
#  Replicación del modelo de regresión lineal del artículo:
#  Yam TTT, Fong SSM, Tsang WWN (2022). Foot posture index and body
#  composition measures in children with and without developmental
#  coordination disorder. PLoS ONE 17(3): e0265280.
#
#  Modelo (Tabla 3 del artículo), SOLO en niños con TDC:
#     FPI-6 derecho (puntaje total) ~ % de grasa corporal total
#  Resultado reportado: r = -0.281, t = -2.213, p = 0.031,
#  R2 = 7.9% (R2 ajustado = 6.3%)
#
# Trastorno del Desarrollo de la Coordinación (TDC/DCD)
##############################################################


##### 1. Datos #####
library(readxl)

url <- "https://raw.githubusercontent.com/Paulinagv1802/Articulo_regresion/main/datos.xlsx"

archivo <- tempfile(fileext = ".xlsx")
download.file(url = url, destfile = archivo, mode = "wb", quiet = TRUE)
datos <- read_excel(archivo)
unlink(archivo)
head(datos)
dim(datos) 

str(datos)
datos$Genero <- factor(datos$Genero, levels = c("Female", "Male"),
                       labels = c("Mujer", "Hombre"))

# Edad en años cumplidos (sin decimales): 5.54 -> 5, 9.96 -> 9
datos$Edad <- floor(datos$Edad)


# Renombramos las columnas (mismo orden del Excel)
colnames(datos) <- c(
  "Sujeto", "Edad", "Genero", "Altura", "Peso", "Long_pierna",
  "MABC", "TDC",
  "FPI_L_RF_1", "FPI_L_RF_2", "FPI_L_RF_3",
  "FPI_L_FF_1", "FPI_L_FF_2", "FPI_L_FF_3", "FPI_L_total",
  "FPI_R_RF_1", "FPI_R_RF_2", "FPI_R_RF_3",
  "FPI_R_FF_1", "FPI_R_FF_2", "FPI_R_FF_3", "FPI_R_total",
  "Pi_grasa_g", "Pi_magra_g", "Pi_total_g", "Pi_grasa_pct",
  "Pd_grasa_g", "Pd_magra_g", "Pd_total_g", "Pd_grasa_pct",
  "Tot_grasa_g", "Tot_magra_g", "Tot_total_g", "Tot_grasa_pct",
  "Grasa_estatura"
)




##### 2. Grupos: TDC vs. control #####

# El artículo clasifica como TDC a los niños con MABC-2 en el percentil <= 15
# (59 DCD y 62 controles). En esta base los percentiles toman valores
# 0, 1, 2, 5, 9, 16, 25, ... y el valor 16 corresponde al corte del
# percentil 15, por lo que el grupo DCD es MABC <= 16.

datos$Grupo <- ifelse(datos$MABC <= 16, "TDC", "Control")
datos$Grupo <- factor(datos$Grupo, levels = c("TDC", "Control"))


# Subconjunto con los niños con TDC
dcd <- subset(datos, Grupo == "TDC")
nrow(dcd)   




##### 3. Exploración previa del modelo #####

# Variables del modelo: Y = FPI_R_total, X = Tot_grasa_pct
summary(dcd$FPI_R_total)
summary(dcd$Tot_grasa_pct)

# Normalidad de las variables
shapiro.test(dcd$FPI_R_total)
shapiro.test(dcd$Tot_grasa_pct)

# Correlación de Pearson (el artículo se reporta r = -0.281, p = 0.031)
cor(dcd$Tot_grasa_pct, dcd$FPI_R_total)
cor.test(dcd$Tot_grasa_pct, dcd$FPI_R_total, method = "pearson")





##### 4. Modelo de regresión lineal simple #####

# FPI-6 derecho (total) explicado por el % de grasa corporal total
mod_lin <- lm(FPI_R_total ~ Tot_grasa_pct, data = dcd)

summary(mod_lin)

# Intervalos de confianza al 95 % de los coeficientes
confint(mod_lin)

# Tabla ANOVA 
anova(mod_lin)

# Coeficiente estandarizado (beta)
beta_est <- coef(mod_lin)[2] * sd(dcd$Tot_grasa_pct) / sd(dcd$FPI_R_total)
beta_est


# R2 y R2 ajustado
summary(mod_lin)$r.squared
summary(mod_lin)$adj.r.squared

###### Ecuación del modelo: #####
# FPI_R_total = 10.97 - 0.141 * Tot_grasa_pct



##### 5. Figuras #####

# Figura 1. Diagrama de dispersión con la recta de regresión
plot(dcd$Tot_grasa_pct, dcd$FPI_R_total,
     main = "FPI-6 derecho vs. grasa corporal total (niños con DCD)",
     xlab = "Grasa corporal total (%)",
     ylab = "FPI-6 derecho (puntaje total)",
     pch = 19, col = "steelblue")
abline(mod_lin, col = "red", lwd = 2)
legend("topright", legend = c("Datos", "Recta de regresión"),
       pch = c(19, NA), lty = c(NA, 1), col = c("steelblue", "red"), bty = "n")
text(x = max(dcd$Tot_grasa_pct) - 2, y = max(dcd$FPI_R_total) - 3,
     labels = paste0("r = ", round(cor(dcd$Tot_grasa_pct, dcd$FPI_R_total), 3),
                     "\nR² = ", round(summary(mod_lin)$r.squared, 3)))




# Figura 2. Recta de regresión con banda de confianza al 95 %
x_nuevo <- data.frame(Tot_grasa_pct = seq(min(dcd$Tot_grasa_pct),
                                          max(dcd$Tot_grasa_pct), length.out = 100))
pred <- predict(mod_lin, newdata = x_nuevo, interval = "confidence", level = 0.95)

plot(dcd$Tot_grasa_pct, dcd$FPI_R_total,
     main = "Recta de regresión con IC 95 % (niños con DCD)",
     xlab = "Grasa corporal total (%)",
     ylab = "FPI-6 derecho (puntaje total)",
     pch = 19, col = "steelblue")
lines(x_nuevo$Tot_grasa_pct, pred[, "fit"], col = "red", lwd = 2)
lines(x_nuevo$Tot_grasa_pct, pred[, "lwr"], col = "red", lty = 2)
lines(x_nuevo$Tot_grasa_pct, pred[, "upr"], col = "red", lty = 2)



# Figura 3. Comparación del FPI-6 y del % de grasa entre grupos
par(mfrow = c(1, 3))
boxplot(FPI_L_total ~ Grupo, data = datos, main = "FPI-6 izquierdo",
        xlab = "Grupo", ylab = "Puntaje total", col = c("tomato", "lightblue"))
boxplot(FPI_R_total ~ Grupo, data = datos, main = "FPI-6 derecho",
        xlab = "Grupo", ylab = "Puntaje total", col = c("tomato", "lightblue"))
boxplot(Tot_grasa_pct ~ Grupo, data = datos, main = "Grasa corporal total",
        xlab = "Grupo", ylab = "Grasa (%)", col = c("tomato", "lightblue"))
par(mfrow = c(1, 1))



##### 6. Supuestos del modelo (diagnóstico de residuos) #####

residuos <- residuals(mod_lin)
ajustados <- fitted(mod_lin)

# Normalidad de los residuos
shapiro.test(residuos)



# Figura 4. Gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(ajustados, residuos, pch = 19, col = "steelblue",
     main = "Residuos vs. valores ajustados",
     xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, col = "red", lty = 2)


qqnorm(residuos, main = "QQ-plot de los residuos", pch = 19, col = "steelblue")
qqline(residuos, col = "red", lwd = 2)


hist(residuos, main = "Histograma de los residuos",
     xlab = "Residuos", ylab = "Frecuencia", col = "lightblue", border = "black")


plot(mod_lin, which = 4, main = "Distancia de Cook")
par(mfrow = c(1, 1))


# Homocedasticidad (prueba de Breusch-Pagan)
install.packages("lmtest")
library(lmtest)
bptest(mod_lin)