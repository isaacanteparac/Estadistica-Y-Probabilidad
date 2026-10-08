# ==============================================================================
# Gráfico: Diagrama de Barras
# Archivo: src/graficos/barra_diagrama.R
# ==============================================================================

graficar_barra <- function(res, titulo_custom = "Diagrama de Barras - Frecuencias y Porcentajes", guardar_png = TRUE) {
  if (missing(res) || is.null(res$TablaCruzada)) {
    stop("Debe proporcionar el diccionario de resultados retornado por la función resultado().")
  }
  
  tabla_frec  <- res$TablaCruzada$Frecuencia_Absoluta
  porcentajes <- res$TablaCruzada$Frecuencia_Relativa * 100
  tot_obs     <- res$TablaCruzada$Total_Observaciones
  
  archivo_png <- "src/graficos/barra_diagrama.png"
  
  if (guardar_png) {
    png(archivo_png, width = 950, height = 650, res = 120)
  }
  
  par(mar = c(6, 5, 4, 2) + 0.1)
  
  colores <- colorRampPalette(c("skyblue2", "dodgerblue3", "darkblue"))(length(tabla_frec))
  max_y   <- max(tabla_frec) * 1.25
  
  bp <- barplot(tabla_frec, 
                main = titulo_custom,
                xlab = "Categorías / Intervalos", 
                ylab = "Frecuencia Absoluta",
                col = colores, 
                border = "white",
                ylim = c(0, max_y),
                las = 2, cex.names = 0.85, cex.main = 1.25, cex.lab = 1.1)
  
  grid(nx = NA, ny = NULL, col = "gray90", lty = "dotted")
  barplot(tabla_frec, col = colores, border = "white", add = TRUE, las = 2, cex.names = 0.85)
  
  for (i in seq_along(tabla_frec)) {
    cant <- tabla_frec[i]
    pct  <- round(porcentajes[i], 1)
    etiqueta <- paste0("n=", cant, "\n(", pct, "%)")
    text(x = bp[i], y = cant + (max_y * 0.03), labels = etiqueta, 
         col = "navyblue", font = 2, cex = 0.8, pos = 3)
  }
  
  legend("topright", 
         legend = c(
           paste0("Total Observaciones: N = ", tot_obs),
           paste0("Categorías: ", length(tabla_frec))
         ),
         fill = "dodgerblue3", bg = "white", box.col = "gray70", cex = 0.9)
  
  if (guardar_png) {
    dev.off()
    cat("  Gráfico guardado en:", archivo_png, "\n")
  }
}
