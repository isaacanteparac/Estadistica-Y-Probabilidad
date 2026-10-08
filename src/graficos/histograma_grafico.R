# ==============================================================================
# Gráfico: Histograma con Curva de Densidad y Líneas de Tendencia
# Archivo: src/graficos/histograma_grafico.R
# ==============================================================================

graficar_histograma <- function(res, titulo_custom = "Histograma de Frecuencias y Curva de Densidad", guardar_png = TRUE) {
  if (missing(res) || is.null(res$Tendencia)) {
    stop("Debe proporcionar el diccionario de resultados retornado por la función resultado().")
  }
  
  datos       <- res$Datos
  media_val   <- res$Tendencia$Media
  mediana_val <- res$Tendencia$Mediana
  modas       <- res$Tendencia$Moda
  
  archivo_png <- "src/graficos/histograma_grafico.png"
  
  if (guardar_png) {
    png(archivo_png, width = 900, height = 600, res = 120)
  }
  
  par(mar = c(5, 5, 4, 2) + 0.1)
  
  h <- hist(datos, prob = TRUE, 
            col = rgb(0.85, 0.92, 0.98, 0.8), 
            border = "steelblue",
            main = titulo_custom,
            xlab = "Valores Observados", 
            ylab = "Densidad de Frecuencia",
            cex.main = 1.3, cex.lab = 1.1, las = 1)
  
  dens <- density(datos)
  grid(nx = NULL, ny = NULL, col = "gray85", lty = "dotted")
  lines(dens, col = "darkblue", lwd = 2.5)
  
  abline(v = media_val, col = "firebrick3", lwd = 2.5, lty = 1)
  abline(v = mediana_val, col = "forestgreen", lwd = 2.5, lty = 2)
  
  if (is.numeric(modas)) {
    for (m in modas) {
      abline(v = m, col = "purple3", lwd = 1.8, lty = 4)
    }
  }
  
  legend("topright", 
         legend = c("Curva Densidad", 
                    paste0("Media (", round(media_val, 2), ")"), 
                    paste0("Mediana (", round(mediana_val, 2), ")"), 
                    paste0("Moda(s): ", paste(round(modas, 2), collapse = ","))),
         col = c("darkblue", "firebrick3", "forestgreen", "purple3"),
         lty = c(1, 1, 2, 4), lwd = c(2.5, 2.5, 2.5, 1.8),
         bg = "white", box.col = "gray70", cex = 0.9)
  
  if (guardar_png) {
    dev.off()
    cat("  Gráfico guardado en:", archivo_png, "\n")
  }
}
