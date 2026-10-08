# ==============================================================================
# Gráfico: Campana de Gauss (Regla Empírica)
# Archivo: src/graficos/campana_diagrama.R
# ==============================================================================

graficar_campana <- function(res, titulo_custom = "Campana de Gauss - Regla Empírica", guardar_png = TRUE) {
  if (missing(res) || is.null(res$ReglaEmpirica)) {
    stop("Debe proporcionar el diccionario de resultados retornado por la función resultado().")
  }
  
  datos      <- res$Datos
  media      <- res$ReglaEmpirica$Parametros$Media
  desviacion <- res$ReglaEmpirica$Parametros$Desviacion
  
  archivo_png <- "src/graficos/campana_diagrama.png"
  
  if (guardar_png) {
    png(archivo_png, width = 950, height = 650, res = 120)
  }
  
  par(mar = c(6, 5, 4, 2) + 0.1)
  
  x <- seq(media - 4 * desviacion, media + 4 * desviacion, length = 500)
  y <- dnorm(x, mean = media, sd = desviacion)
  
  plot(x, y, type = "n", 
       main = titulo_custom,
       xlab = "Valores de la Variable", 
       ylab = "Densidad de Probabilidad Teórica",
       xaxt = "n", las = 1, cex.main = 1.25, cex.lab = 1.1)
  
  grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
  
  # Sombra Área 3 SD (99.7%)
  x_99 <- seq(media - 3*desviacion, media + 3*desviacion, length = 200)
  polygon(c(media - 3*desviacion, x_99, media + 3*desviacion), c(0, dnorm(x_99, media, desviacion), 0), 
          col = rgb(0.85, 0.95, 0.85, 0.6), border = NA)
  
  # Sombra Área 2 SD (95%)
  x_95 <- seq(media - 2*desviacion, media + 2*desviacion, length = 200)
  polygon(c(media - 2*desviacion, x_95, media + 2*desviacion), c(0, dnorm(x_95, media, desviacion), 0), 
          col = rgb(1.0, 0.9, 0.7, 0.7), border = NA)
  
  # Sombra Área 1 SD (68%)
  x_68 <- seq(media - 1*desviacion, media + 1*desviacion, length = 200)
  polygon(c(media - 1*desviacion, x_68, media + 1*desviacion), c(0, dnorm(x_68, media, desviacion), 0), 
          col = rgb(0.7, 0.85, 1.0, 0.8), border = NA)
  
  hist(datos, prob = TRUE, add = TRUE, col = rgb(0.5, 0.5, 0.5, 0.2), border = "gray50")
  lines(x, y, col = "darkblue", lwd = 2.5)
  
  abline(v = media, col = "firebrick3", lwd = 3, lty = 1)
  abline(v = c(media - desviacion, media + desviacion), col = "royalblue3", lty = 2, lwd = 2)
  abline(v = c(media - 2*desviacion, media + 2*desviacion), col = "darkorange2", lty = 3, lwd = 2)
  abline(v = c(media - 3*desviacion, media + 3*desviacion), col = "forestgreen", lty = 4, lwd = 2)
  
  eje_x_at <- c(media - 3*desviacion, media - 2*desviacion, media - desviacion, 
                media, 
                media + desviacion, media + 2*desviacion, media + 3*desviacion)
  
  eje_x_labels <- c("-3σ", "-2σ", "-1σ", "μ (Media)", "+1σ", "+2σ", "+3σ")
  eje_x_sub <- paste0(eje_x_labels, "\n(", round(eje_x_at, 2), ")")
  
  axis(1, at = eje_x_at, labels = eje_x_sub, mgp = c(3, 2, 0), cex.axis = 0.85)
  
  legend("topright", 
         legend = c("Curva Teórica N(μ,σ)",
                    paste0("Media μ = ", round(media, 2)),
                    paste0("±1 SD (68%): [", round(media-desviacion,2), " a ", round(media+desviacion,2), "]"),
                    paste0("±2 SD (95%): [", round(media-2*desviacion,2), " a ", round(media+2*desviacion,2), "]"),
                    paste0("±3 SD (99.7%): [", round(media-3*desviacion,2), " a ", round(media+3*desviacion,2), "]")),
         fill = c(NA, NA, rgb(0.7, 0.85, 1.0, 0.8), rgb(1.0, 0.9, 0.7, 0.7), rgb(0.85, 0.95, 0.85, 0.6)),
         col = c("darkblue", "firebrick3", "royalblue3", "darkorange2", "forestgreen"),
         lty = c(1, 1, 2, 3, 4), lwd = 2, bg = "white", cex = 0.85, box.col = "gray70")
  
  if (guardar_png) {
    dev.off()
    cat("  Gráfico guardado en:", archivo_png, "\n")
  }
}