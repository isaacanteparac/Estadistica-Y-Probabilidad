# ==============================================================================
# Módulo: Valores Atípicos (Cálculos Estadísticos Puros - Sin Gráficos)
# ==============================================================================

analizar_valores_atipicos <- function(datos) {
  datos_limpios <- datos[!is.na(datos)]
  
  if (length(datos_limpios) == 0) {
    return(list(
      Outliers         = numeric(0),
      Indices_Outliers = integer(0),
      Media            = NA,
      Datos_Escalados  = numeric(0)
    ))
  }
  
  outliers <- boxplot.stats(datos_limpios)$out
  indices  <- which(datos_limpios %in% outliers)
  media    <- mean(datos_limpios)
  escalados<- as.vector(scale(datos_limpios))
  
  list(
    Outliers         = outliers,
    Indices_Outliers = indices,
    Media            = media,
    Datos_Escalados  = escalados
  )
}