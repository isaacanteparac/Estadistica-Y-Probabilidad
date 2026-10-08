# ==============================================================================
# Módulo UI - Visualización por Secciones Independientes y Tablas de Frecuencia
# Archivo: ui.R
# ==============================================================================

tipoContenido <- list(
  descripcion = "Descripción",
  resultado   = "Resultado"
)

# Banner principal
banner <- function(texto) {
  ancho <- 95
  linea <- paste(rep("═", ancho), collapse = "")
  texto_centrado <- sprintf("%*s", (ancho + nchar(texto)) %/% 2, toupper(texto))
  cat("\n", linea, "\n", texto_centrado, "\n", linea, "\n\n", sep = "")
}

# Título para cada sección independiente
title <- function(text) {
  cat("\n=========================================================================================\n")
  cat("  ► ", toupper(text), "\n")
  cat("=========================================================================================\n")
}

separador_seccion <- function() {
  cat("\n-----------------------------------------------------------------------------------------\n\n")
}

# Formatear valores individuales
formatear_valor <- function(val) {
  if (is.null(val)) return("NULL")
  if (length(val) == 0) return("(Vacío)")
  if (is.numeric(val)) {
    val_round <- round(val, 4)
    return(paste(val_round, collapse = ", "))
  }
  if (is.logical(val)) return(ifelse(val, "TRUE", "FALSE"))
  if (is.matrix(val) || is.table(val)) return("(Estructura de Datos/Tabla)")
  return(paste(val, collapse = ", "))
}

# Función para dividir texto largo en múltiples líneas
ajustar_texto_multilinea <- function(texto, ancho = 55) {
  if (is.null(texto) || nchar(texto) == 0) return("")
  if (nchar(texto) <= ancho) return(texto)
  
  tokens <- unlist(strsplit(texto, "(?<=\\s)|(?<=,)", perl = TRUE))
  lineas <- character()
  linea_actual <- ""
  
  for (t in tokens) {
    if (nchar(linea_actual) + nchar(t) <= ancho) {
      linea_actual <- paste0(linea_actual, t)
    } else {
      if (nchar(linea_actual) > 0) lineas <- c(lineas, trimws(linea_actual))
      linea_actual <- t
    }
  }
  if (nchar(linea_actual) > 0) lineas <- c(lineas, trimws(linea_actual))
  return(lineas)
}

# Imprimir un diccionario de sección individual de forma independiente
imprimir_seccion <- function(titulo_seccion, dict_seccion) {
  ancho_clave <- 30
  ancho_valor <- 55
  
  cat("\n┌─ [ ", titulo_seccion, " ] ", paste(rep("─", max(5, 85 - nchar(titulo_seccion))), collapse = ""), "\n", sep = "")
  cat(sprintf("│ %-*s │ %-*s │\n", ancho_clave, "MÉTRICA / PROPIEDAD", ancho_valor, "VALOR / RESULTADO"))
  cat("├", paste(rep("─", ancho_clave + 2), collapse = ""), "┼", paste(rep("─", ancho_valor + 2), collapse = ""), "┤\n", sep = "")
  
  nombres <- names(dict_seccion)
  if (is.null(nombres)) nombres <- paste0("[", seq_along(dict_seccion), "]")
  
  for (i in seq_along(dict_seccion)) {
    clave <- nombres[i]
    valor <- dict_seccion[[i]]
    
    if (is.data.frame(valor)) next
    
    if (is.list(valor)) {
      cat(sprintf("│ %-*s │ %-*s │\n", ancho_clave, paste0("► ", clave), ancho_valor, "[Sub-sección]"))
      imprimir_sub_seccion(valor, nivel = 1, ancho_clave = ancho_clave, ancho_valor = ancho_valor)
    } else if (is.table(valor) || is.matrix(valor)) {
      cat(sprintf("│ %-*s │ %-*s │\n", ancho_clave, clave, ancho_valor, "[Tabla Frecuencias]"))
    } else {
      val_str <- formatear_valor(valor)
      lineas_valor <- ajustar_texto_multilinea(val_str, ancho = ancho_valor)
      
      cat(sprintf("│ %-*s │ %-*s │\n", ancho_clave, clave, ancho_valor, lineas_valor[1]))
      if (length(lineas_valor) > 1) {
        for (j in 2:length(lineas_valor)) {
          cat(sprintf("│ %-*s │ %-*s │\n", ancho_clave, "", ancho_valor, lineas_valor[j]))
        }
      }
    }
  }
  
  cat("└", paste(rep("─", ancho_clave + 2), collapse = ""), "┴", paste(rep("─", ancho_valor + 2), collapse = ""), "┘\n\n", sep = "")
}

imprimir_sub_seccion <- function(dict, nivel = 1, ancho_clave = 30, ancho_valor = 55) {
  indent <- paste(rep("  ", nivel), collapse = "")
  nombres <- names(dict)
  
  for (i in seq_along(dict)) {
    clave <- nombres[i]
    valor <- dict[[i]]
    
    if (is.list(valor)) {
      cat(sprintf("│ %-*s │ %-*s │\n", ancho_clave, paste0(indent, "• ", clave), ancho_valor, "[Sub-datos]"))
      imprimir_sub_seccion(valor, nivel = nivel + 1, ancho_clave = ancho_clave, ancho_valor = ancho_valor)
    } else {
      val_str <- formatear_valor(valor)
      lineas_valor <- ajustar_texto_multilinea(val_str, ancho = ancho_valor)
      
      cat(sprintf("│ %-*s │ %-*s │\n", ancho_clave, paste0(indent, "• ", clave), ancho_valor, lineas_valor[1]))
      if (length(lineas_valor) > 1) {
        for (j in 2:length(lineas_valor)) {
          cat(sprintf("│ %-*s │ %-*s │\n", ancho_clave, "", ancho_valor, lineas_valor[j]))
        }
      }
    }
  }
}

# Imprimir Tabla de Frecuencias Completa como DataFrame Tabulado (Dinámico)
imprimir_tabla_frecuencia_df <- function(df, titulo = "TABLA DE FRECUENCIAS ESTADÍSTICA COMPLETA") {
  col_cat <- names(df)[1]
  
  cat("\n┌─ [ ", titulo, " ] ", paste(rep("─", max(5, 85 - nchar(titulo))), collapse = ""), "\n", sep = "")
  
  cat(sprintf("│ %-18s │ %-12s │ %-13s │ %-12s │ %-13s │ %-12s │\n",
              toupper(col_cat), "FREC. ABS (n)", "FREC. REL (f)", "PORCENTAJE%", "FREC. ACUM(N)", "PCT. ACUM%"))
  cat("├────────────────────┼──────────────┼───────────────┼──────────────┼───────────────┼──────────────┤\n")
  
  for (i in 1:nrow(df)) {
    cat(sprintf("│ %-18s │ %12s │ %13s │ %12s │ %13s │ %12s │\n",
                df[[1]][i],
                df$Frec_Absoluta[i],
                df$Frec_Relativa[i],
                df$Porcentaje_Pct[i],
                df$Frec_Acumulada[i],
                df$Pct_Acumulado[i]))
  }
  
  cat("└────────────────────┴──────────────┴───────────────┴──────────────┴───────────────┴──────────────┘\n\n")
}
