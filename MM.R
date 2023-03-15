# Datos de ejemplo
datos <- data.frame(concentracion = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),
                    velocidad = c(0.01, 0.02, 0.05, 0.1, 0.15, 0.18, 0.19))

# Ajuste del modelo de regresión de Michaelis-Menten
modelo <- nls(velocidad ~ Vmax * concentracion / (Km + concentracion),
              data = datos,
              start = list(Vmax = 0.2, Km = 0.05))

# Resumen del modelo
summary(modelo)

# Gráfico de los datos y la curva ajustada
library(ggplot2)
ggplot(datos, aes(x = concentracion, y = velocidad)) +
  geom_point() +
  geom_line(aes(y = predict(modelo)))
