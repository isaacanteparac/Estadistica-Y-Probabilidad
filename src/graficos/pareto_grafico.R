# ==============================================================================
# Gráfico: Diagrama de Pareto (Frecuencia Acumulada 80/20)
# Archivo: src/graficos/pareto_grafico.R
# ==============================================================================

graficar_pareto <- function(res, titulo_custom = "Diagrama de Pareto (Principio 80/20)", guardar_png = TRUE) {
  if (missing(res) || is.null(res$TablaCruzada)) {
    stop("Debe proporcionar el diccionario de resultados retornado por la función resultado().")
  }
  
  frec_abs <- res$TablaCruzada$Frecuencia_Absoluta
  
  frec_ordenada <- sort(frec_abs, decreasing = TRUE)
  tot_obs       <- sum(frec_ordenada)
  pct_acumulado <- cumsum(frec_ordenada) / tot_obs * 100
  
  archivo_png <- "src/graficos/pareto_grafico.png"
  
  if (guardar_png) {
    png(archivo_png, width = 950, height = 650, res = 120)
  }
  
  par(mar = c(6, 5, 4, 5) + 0.1)
  
  bp <- barplot(frec_ordenada, 
                main = titulo_custom,
                xlab = "Categorías / Intervalos (Orden de Relevancia)", 
                ylab = "Frecuencia Absoluta",
                col = "steelblue3", border = "white",
                ylim = c(0, max(frec_ordenada) * 1.2),
                las = 2, cex.names = 0.85, cex.main = 1.25)
  
  grid(nx = NA, ny = NULL, col = "gray90", lty = "dotted")
  barplot(frec_ordenada, col = "steelblue3", border = "white", add = TRUE, las = 2, cex.names = 0.85)
  
  par(new = TRUE)
  plot(bp, pct_acumulado, type = "b", pch = 19, col = "firebrick3", lwd = 2,
       axes = FALSE, xlab = "", ylab = "", ylim = c(0, 105))
  
  axis(4, at = seq(0, 100, by = 20), labels = paste0(seq(0, 100, by = 20), "%"), las = 1, col.axis = "firebrick3")
  mtext("Porcentaje Acumulado (%)", side = 4, line = 3, col = "firebrick3", cex = 1.0)
  
  abline(h = 80, col = "darkorange2", lty = 2, lwd = 1.8)
  text(x = max(bp), y = 82, labels = "Límite 80%", col = "darkorange2", font = 2, pos = 2)
  
  legend("center", 
         legend = c("Frecuencia Absoluta", "Porcentaje Acumulado", "Límite 80% Pareto"),
         fill = c("steelblue3", NA, NA),
         col = c(NA, "firebrick3", "darkorange2"),
         lty = c(NA, 1, 2), pch = c(NA, 19, NA), lwd = c(NA, 2, 1.8),
         bg = "white", box.col = "gray70", cex = 0.85)
  
  if (guardar_png) {
    dev.off()
    cat("  Gráfico guardado en:", archivo_png, "\n")
  }
}
