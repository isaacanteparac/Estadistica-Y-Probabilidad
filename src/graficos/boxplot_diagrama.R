# ==============================================================================
# Gráfico: Diagrama de Caja y Bigotes (Boxplot)
# Archivo: src/graficos/boxplot_diagrama.R
# ==============================================================================

graficar_boxplot <- function(res, titulo_custom = "Diagrama de Caja y Bigotes (Boxplot - Outliers)", guardar_png = TRUE) {
  if (missing(res) || is.null(res$Atipicos)) {
    stop("Debe proporcionar el diccionario de resultados retornado por la función resultado().")
  }
  
  datos            <- res$Datos
  outliers         <- res$Atipicos$Outliers
  indices_outliers <- res$Atipicos$Indices_Outliers
  mediana          <- res$Tendencia$Mediana
  
  q1 <- quantile(datos, 0.25)
  q3 <- quantile(datos, 0.75)
  iqr_val <- q3 - q1
  
  archivo_png <- "src/graficos/boxplot_diagrama.png"
  
  if (guardar_png) {
    png(archivo_png, width = 900, height = 600, res = 120)
  }
  
  par(mar = c(5, 5, 4, 2) + 0.1)
  
  bp <- boxplot(datos, 
                main = titulo_custom,
                col = rgb(0.8, 0.9, 1, 0.6), 
                border = "darkblue",
                notch = FALSE,
                ylab = "Valores Observados",
                outline = FALSE,
                las = 1, cex.main = 1.25, cex.lab = 1.1)
  
  grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
  boxplot(datos, col = rgb(0.8, 0.9, 1, 0.6), border = "darkblue", add = TRUE, outline = FALSE, las = 1)
  
  stripchart(datos, method = "jitter", jitter = 0.15, vertical = TRUE, 
             add = TRUE, pch = 21, bg = rgb(0.2, 0.6, 0.9, 0.5), col = "navy", cex = 1.1)
  
  if (length(outliers) > 0) {
    points(rep(1, length(outliers)), outliers, pch = 19, col = "firebrick3", cex = 1.6)
    
    for (i in seq_along(outliers)) {
      val <- outliers[i]
      idx <- indices_outliers[i]
      text(1.08, val, labels = paste0("Val: ", val, " (Pos: #", idx, ")"), 
           col = "firebrick3", font = 2, pos = 4, cex = 0.9)
    }
  }
  
  abline(h = q1, col = "darkgreen", lty = 2, lwd = 1.5)
  abline(h = mediana, col = "firebrick", lty = 1, lwd = 2)
  abline(h = q3, col = "darkgreen", lty = 2, lwd = 1.5)
  
  legend("topleft", 
         legend = c(
           paste0("Mediana (Q2): ", round(mediana, 2)),
           paste0("Q1 (25%): ", round(q1, 2)),
           paste0("Q3 (75%): ", round(q3, 2)),
           paste0("IQR: ", round(iqr_val, 2)),
           paste0("Outliers: ", length(outliers), " dato(s)")
         ),
         fill = c(NA, NA, NA, NA, "firebrick3"),
         col = c("firebrick", "darkgreen", "darkgreen", NA, "firebrick3"),
         lty = c(1, 2, 2, NA, NA), lwd = c(2, 1.5, 1.5, NA, NA),
         bg = "white", box.col = "gray70", cex = 0.85)
  
  if (guardar_png) {
    dev.off()
    cat("  Gráfico guardado en:", archivo_png, "\n")
  }
}
