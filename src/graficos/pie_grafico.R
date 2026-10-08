# ==============================================================================
# Gráfico: Diagrama de Pastel / Pie Chart
# Archivo: src/graficos/pie_grafico.R
# ==============================================================================

graficar_pie <- function(res, titulo_custom = "Diagrama de Pastel (Distribución Porcentual)", guardar_png = TRUE) {
  if (missing(res) || is.null(res$TablaCruzada)) {
    stop("Debe proporcionar el diccionario de resultados retornado por la función resultado().")
  }
  
  frec_abs <- res$TablaCruzada$Frecuencia_Absoluta
  tot_obs  <- res$TablaCruzada$Total_Observaciones
  pcts     <- round((frec_abs / tot_obs) * 100, 1)
  
  etiquetas <- paste0(names(frec_abs), "\n", pcts, "% (n=", frec_abs, ")")
  colores   <- rainbow(length(frec_abs), s = 0.65, v = 0.9)
  
  archivo_png <- "src/graficos/pie_grafico.png"
  
  if (guardar_png) {
    png(archivo_png, width = 900, height = 650, res = 120)
  }
  
  par(mar = c(4, 4, 4, 4))
  
  pie(frec_abs, 
      labels = etiquetas, 
      col = colores, 
      main = titulo_custom, 
      cex = 0.85, 
      cex.main = 1.25)
  
  legend("topright", 
         legend = paste0(names(frec_abs), ": ", pcts, "%"),
         fill = colores, bg = "white", box.col = "gray70", cex = 0.8)
  
  if (guardar_png) {
    dev.off()
    cat("  Gráfico guardado en:", archivo_png, "\n")
  }
}
