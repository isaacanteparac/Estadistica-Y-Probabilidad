# ==============================================================================
# Gráfico: Diagrama de Tallos y Hojas (Stem-and-Leaf Plot)
# Consume el diccionario 'res' producido por la función resultado() en main.R
# Archivo: src/graficos/tallos_hojas_grafico.R
# ==============================================================================

graficar_tallos_hojas <- function(res, scale = 1) {
  if (missing(res) || is.null(res$Datos)) {
    stop("Debe proporcionar el diccionario de resultados retornado por la función resultado().")
  }
  
  datos <- res$Datos
  
  cat("\n┌────────────────────────────────────────────────────────────────────────┐\n")
  cat("│ DIAGRAMA DE TALLOS Y HOJAS (STEM-AND-LEAF PLOT)                        │\n")
  cat("└────────────────────────────────────────────────────────────────────────┘\n\n")
  
  # Imprimir el diagrama de tallos y hojas nativo de R
  stem(datos, scale = scale)
  cat("\n------------------------------------------------------------------------\n\n")
}
