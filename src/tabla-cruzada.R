# ==============================================================================
# Módulo: Tabla de Frecuencias Completa y Frecuencia Cruzada
# Cálculo de Intervalos: P = Xmax - Xmin | Amplitud = P / nclases
# Archivo: src/tabla-cruzada.R
# ==============================================================================

analizar_tabla_cruzada <- function(datos, nclases = 0, variable_secundaria = NULL) {
  datos_limpios <- datos[!is.na(datos)]
  
  if (is.null(variable_secundaria)) {
    if (!is.null(nclases) && is.numeric(nclases) && nclases > 0) {
      xmin <- min(datos_limpios)
      xmax <- max(datos_limpios)
      p    <- xmax - xmin  # Rango o Amplitud Total (P = xmax - xmin)
      
      ancho_intervalo <- p / nclases  # Amplitud de cada intervalo
      
      # Generar cortes de clase exactos
      cortes <- seq(from = xmin, to = xmax, length.out = nclases + 1)
      
      corte <- cut(datos_limpios, breaks = cortes, include.lowest = TRUE, right = FALSE)
      frec_abs <- table(corte)
      usar_intervalos <- TRUE
    } else {
      # Si nclases == 0, no se crean intervalos y se usan valores/categorías individuales
      frec_abs <- table(datos_limpios)
      usar_intervalos <- FALSE
      p <- NA
      ancho_intervalo <- NA
    }
    
    tot_obs     <- sum(frec_abs)
    frec_rel    <- frec_abs / tot_obs
    frec_pct    <- frec_rel * 100
    frec_acum   <- cumsum(frec_abs)
    frec_rel_ac <- cumsum(frec_rel) * 100
    
    col_nombre <- if (usar_intervalos) "Intervalo_Clase" else "Valor_Categoria"
    
    tabla_frecuencia_df <- data.frame(
      Categoria       = names(frec_abs),
      Frec_Absoluta   = as.vector(frec_abs),
      Frec_Relativa   = round(as.vector(frec_rel), 4),
      Porcentaje_Pct  = paste0(round(as.vector(frec_pct), 2), "%"),
      Frec_Acumulada  = as.vector(frec_acum),
      Pct_Acumulado   = paste0(round(as.vector(frec_rel_ac), 2), "%"),
      stringsAsFactors = FALSE
    )
    names(tabla_frecuencia_df)[1] <- col_nombre
    
    return(list(
      nclases              = nclases,
      Rango_P              = p,
      Ancho_Intervalo      = ancho_intervalo,
      Usar_Intervalos      = usar_intervalos,
      Cantidad_Clases      = length(frec_abs),
      Frecuencia_Absoluta  = frec_abs,
      Frecuencia_Relativa  = frec_rel,
      Total_Observaciones  = tot_obs,
      Tabla_Frecuencias_DF = tabla_frecuencia_df
    ))
  } else {
    tabla_cruzada <- table(datos_limpios, variable_secundaria)
    prop_filas     <- prop.table(tabla_cruzada, margin = 1)
    prop_columnas  <- prop.table(tabla_cruzada, margin = 2)
    
    return(list(
      Tabla_Cruzada        = tabla_cruzada,
      Proporcion_Filas     = prop_filas,
      Proporcion_Columnas  = prop_columnas,
      Total_Observaciones  = sum(tabla_cruzada)
    ))
  }
}
