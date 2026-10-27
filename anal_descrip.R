library(readxl)

# Cargamos los datos:
datos <- read_excel("C:/Users/USUARIO/Desktop/Articulo/datos.xlsx")

# Renombramos para evitar problemas con caracteres especiales (como la tilde o el guion)
colnames(datos) <- c("Grupo", "Masa", "Diametro", "EM_FM")

datos$Grupo <- as.factor(datos$Grupo)

modelo_completo <- lm(EM_FM ~ Masa + Diametro + Grupo, data = datos)

#Vemos los estimadores (betas) y los valores-p
summary(modelo_completo)

# Generamos la tabla ANOVA
anova(modelo_completo)

# Miramos si nuestros datos tienden a una distribución normal:
# Visualizamos la distribución de la variable de Fuerza Máxima (EM-FM)

hist(datos$EM_FM, probability = TRUE, main = "Distribución de Fuerza Máxima", 
     xlab = "EM-FM (N)", col = "skyblue")
lines(density(datos$EM_FM), col = "red", lwd = 2)

"Podemos notar que existe una forma de campana de gauss, esto nos permite decir
que nuestros datos tienen una tendencia a distribución normal."


#Podemos verificar que los errores tiendan a una distribución normal, mediante un grafico
#Q-Q:

# Extraemos los residuales del modelo que creamos en el paso anterior:
residuales <- residuals(modelo_completo)

# Creamos el gráfico Q-Q para los residuales:
qqnorm(residuales, main = "Gráfico Q-Q de los Residuales")
qqline(residuales, col = "blue", lwd = 2)

"Podemos notar que en el extremo derecho tenemos un sesgo, ya que nuestros datos se dispersan
de manera significativa por ese extremo. Pero podemos notar que nuestros datos se alinean de manera
estrecha sobre la linea azul por una gran cantidad de parte, esto nos permite decir que 
nuestros datos se distribuyen normal."


#Confirmamos la normalidad de nuestros datos con la prueba de Shapiro-Wilk:

# Aplicamos la prueba estadística de normalidad a los residuales
shapiro.test(residuales)

"Como el p-valor de los residuales del modelo es mayor a 0.05, no rechazamos la
hipotesis nula, esto quiere decir que nuestros datos siguen una distribución normal"