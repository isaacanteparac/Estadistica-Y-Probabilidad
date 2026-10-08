# ==============================================================================
# Módulo: Corrección de Outliers por Promedio de Vecinos
# Fórmulas individuales por cada outlier: (datos[idx-1] + datos[idx+1]) / 2
# Archivo: src/correccion_outlier.R
# ==============================================================================

correccion_outlier <- function(datos) {
  datos_limpios <- datos[!is.na(datos)]
  n <- length(datos_limpios)
  
  if (n < 2) {
    return(list(
      Tiene_Outliers       = FALSE,
      Cantidad_Outliers    = 0,
      Outliers             = numeric(0),
      Indices_Outliers     = integer(0),
      Detalle_Correcciones = list(Mensaje = "No hay suficientes datos para corregir outliers."),
      Datos_Originales     = datos_limpios,
      Datos_Corregidos     = datos_limpios
    ))
  }
  
  # Identificar outliers usando el criterio de boxplot.stats
  outliers <- boxplot.stats(datos_limpios)$out
  indices_outliers <- which(datos_limpios %in% outliers)
  
  datos_corregidos <- datos_limpios
  detalle_correcciones <- list()
  
  if (length(indices_outliers) > 0) {
    for (k in seq_along(indices_outliers)) {
      idx <- indices_outliers[k]
      val_outlier <- datos_limpios[idx]
      
      if (idx == 1) {
        val_sig <- datos_limpios[idx + 1]
        promedio <- val_sig
        formula_str <- paste0("Corrección del outlier ", val_outlier, " (índice ", idx, "): datos[", idx+1, "] = ", val_sig)
      } else if (idx == n) {
        val_ant <- datos_limpios[idx - 1]
        promedio <- val_ant
        formula_str <- paste0("Corrección del outlier ", val_outlier, " (índice ", idx, "): datos[", idx-1, "] = ", val_ant)
      } else {
        val_ant <- datos_limpios[idx - 1]
        val_sig <- datos_limpios[idx + 1]
        promedio <- (val_ant + val_sig) / 2
        formula_str <- paste0("Corrección del outlier ", val_outlier, " (índice ", idx, "): (datos[", idx-1, "] + datos[", idx+1, "]) / 2 = (", val_ant, " + ", val_sig, ") / 2 = ", promedio)
      }
      
      datos_corregidos[idx] <- promedio
      clave_nom <- paste0("Outlier_", k, " (Valor ", val_outlier, ", Posición ", idx, ")")
      detalle_correcciones[[clave_nom]] <- formula_str
    }
  } else {
    detalle_correcciones[["Resultado"]] <- "No se detectaron outliers en el conjunto de datos."
  }
  
  list(
    Tiene_Outliers       = length(indices_outliers) > 0,
    Cantidad_Outliers    = length(indices_outliers),
    Outliers             = outliers,
    Indices_Outliers     = indices_outliers,
    Detalle_Correcciones = detalle_correcciones,
    Datos_Originales     = datos_limpios,
    Datos_Corregidos     = datos_corregidos
  )
}
