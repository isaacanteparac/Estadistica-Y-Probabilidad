# 1. Función para contar y listar números dentro de un rango
contar_en_rango <- function(datos, limite_inferior, limite_superior) {
  datos_limpios <- datos[!is.na(datos)]
  
  # Filtrar elementos en el rango inclusive [limite_inferior, limite_superior]
  elementos <- datos_limpios[datos_limpios >= limite_inferior & datos_limpios <= limite_superior]
  
  list(
    rango = c(limite_inferior = limite_inferior, limite_superior = limite_superior),
    cantidad = length(elementos),
    porcentaje = (length(elementos) / length(datos_limpios)) * 100,
    elementos = elementos
  )
}

# 2. Función para calcular los intervalos de la Regla Empírica
calcular_intervalos_empiricos <- function(media, desviacion) {
  list(
    `68% (1 SD)`   = c(limite_inferior = media - desviacion,     limite_superior = media + desviacion),
    `95% (2 SD)`   = c(limite_inferior = media - 2 * desviacion, limite_superior = media + 2 * desviacion),
    `99.7% (3 SD)` = c(limite_inferior = media - 3 * desviacion, limite_superior = media + 3 * desviacion)
  )
}

# 3. Función integradora para aplicar la regla empírica sobre un conjunto de datos
evaluar_regla_empirica <- function(datos, media, desviacion) {
  intervalos <- calcular_intervalos_empiricos(media, desviacion)
  
  # Uso de los nombres exactos de las llaves
  res_68 <- contar_en_rango(datos, intervalos$`68% (1 SD)`[1], intervalos$`68% (1 SD)`[2])
  res_95 <- contar_en_rango(datos, intervalos$`95% (2 SD)`[1], intervalos$`95% (2 SD)`[2])
  res_99 <- contar_en_rango(datos, intervalos$`99.7% (3 SD)`[1], intervalos$`99.7% (3 SD)`[2])
  
  list(
    Parametros = list(Media = media, Desviacion = desviacion),
    Intervalos = intervalos,
    Conteo = list(
      `68% (1 SD)`   = res_68,
      `95% (2 SD)`   = res_95,
      `99.7% (3 SD)` = res_99
    )
  )
}

# Datos de prueba
datos_ejemplo <- c(13.5, 9.5, 8.2, 6.5, 8.4, 8.1, 6.9, 7.5, 10.5, 13.5,
                   7.2, 7.1, 9.0, 9.9, 8.2, 13.2, 9.2, 6.9, 9.6, 7.7,
                   9.7, 7.5, 7.2, 5.9, 6.6, 11.1, 8.8, 5.2, 10.6, 8.2,
                   11.3, 5.6, 10.1, 8.0, 8.5, 11.7, 7.1, 7.7, 9.4, 6.0,
                   8.0, 7.4, 10.5, 7.8, 7.9, 6.5, 6.8, 9.5)

#resultados <- evaluar_regla_empirica(datos_ejemplo, 8.49, 1.98) 
#print(resultados)


# Función para graficar la curva de campana con los intervalos de la regla empírica
graficar_campana_empirica <- function(datos, media, desviacion) {
  # Crear secuencia de puntos para la curva normal
  x <- seq(media - 4 * desviacion, media + 4 * desviacion, length = 300)
  y <- dnorm(x, mean = media, sd = desviacion)
  
  # Dibujar la curva principal
  plot(x, y, type = "l", lwd = 2, col = "blue",
       main = "Regla Empírica - Curva de Campana",
       xlab = "Valores", ylab = "Densidad",
       xaxt = "n") # Ocultar eje X por defecto para personalizarlo
  
  # Sombrear la región del 68% (1 Desviación Estándar)
  x_68 <- seq(media - desviacion, media + desviacion, length = 100)
  y_68 <- dnorm(x_68, mean = media, sd = desviacion)
  polygon(c(media - desviacion, x_68, media + desviacion), 
          c(0, y_68, 0), col = rgb(0.2, 0.6, 1, 0.3), border = NA)
  
  # Histogramas de fondo (opcional para ver distribución real)
  hist(datos, prob = TRUE, add = TRUE, col = rgb(0.8, 0.8, 0.8, 0.3), border = "gray")
  lines(x, y, lwd = 2, col = "darkblue")
  
  # Marcadores de líneas verticales para Media y desviaciones
  abline(v = media, col = "red", lwd = 2, lty = 1)
  
  # 1 Desviación Estándar (68%)
  abline(v = c(media - desviacion, media + desviacion), col = "blue", lty = 2, lwd = 1.5)
  
  # 2 Desviaciones Estándar (95%)
  abline(v = c(media - 2 * desviacion, media + 2 * desviacion), col = "orange", lty = 3, lwd = 1.5)
  
  # 3 Desviaciones Estándar (99.7%)
  abline(v = c(media - 3 * desviacion, media + 3 * desviacion), col = "darkgreen", lty = 4, lwd = 1.5)
  
  # Personalizar etiquetas del eje X con los valores reales
  eje_x_valores <- c(media - 3*desviacion, media - 2*desviacion, media - desviacion, 
                    media, 
                    media + desviacion, media + 2*desviacion, media + 3*desviacion)
  
  axis(1, at = eje_x_valores, labels = round(eje_x_valores, 2), las = 2)
  
  # Leyenda explicativa
  legend("topright", 
         legend = c("Media (μ)", "1 SD (68%)", "2 SD (95%)", "3 SD (99.7%)"),
         col = c("red", "blue", "orange", "darkgreen"), 
         lty = c(1, 2, 3, 4), lwd = 2, cex = 0.8)
}

# --- EJEMPLO DE USO ---
datos_ejemplo <- c(13.5, 9.5, 8.2, 6.5, 8.4, 8.1, 6.9, 7.5, 10.5, 13.5,
                   7.2, 7.1, 9.0, 9.9, 8.2, 13.2, 9.2, 6.9, 9.6, 7.7,
                   9.7, 7.5, 7.2, 5.9, 6.6, 11.1, 8.8, 5.2, 10.6, 8.2,
                   11.3, 5.6, 10.1, 8.0, 8.5, 11.7, 7.1, 7.7, 9.4, 6.0,
                   8.0, 7.4, 10.5, 7.8, 7.9, 6.5, 6.8, 9.5)

# Generar gráfico pasándole los datos, la media (8.49) y desviación (1.98)
graficar_campana_empirica(datos_ejemplo, media = 8.49, desviacion = 1.98)

