# ==============================================================================
# Módulo: Regla Empírica (Cálculos Estadísticos Puros - Sin Gráficos)
# ==============================================================================

contar_en_rango <- function(datos, limite_inferior, limite_superior) {
  datos_limpios <- datos[!is.na(datos)]
  elementos <- datos_limpios[datos_limpios >= limite_inferior & datos_limpios <= limite_superior]
  
  list(
    rango      = c(limite_inferior = limite_inferior, limite_superior = limite_superior),
    cantidad   = length(elementos),
    porcentaje = (length(elementos) / length(datos_limpios)) * 100,
    elementos  = elementos
  )
}

calcular_intervalos_empiricos <- function(media, desviacion) {
  list(
    `68% (1 SD)`   = c(limite_inferior = media - desviacion,     limite_superior = media + desviacion),
    `95% (2 SD)`   = c(limite_inferior = media - 2 * desviacion, limite_superior = media + 2 * desviacion),
    `99.7% (3 SD)` = c(limite_inferior = media - 3 * desviacion, limite_superior = media + 3 * desviacion)
  )
}

evaluar_regla_empirica <- function(datos, media = NULL, desviacion = NULL) {
  datos_limpios <- datos[!is.na(datos)]
  if (is.null(media)) media <- mean(datos_limpios)
  if (is.null(desviacion)) desviacion <- sd(datos_limpios)

  intervalos <- calcular_intervalos_empiricos(media, desviacion)
  
  res_68 <- contar_en_rango(datos_limpios, intervalos$`68% (1 SD)`[1], intervalos$`68% (1 SD)`[2])
  res_95 <- contar_en_rango(datos_limpios, intervalos$`95% (2 SD)`[1], intervalos$`95% (2 SD)`[2])
  res_99 <- contar_en_rango(datos_limpios, intervalos$`99.7% (3 SD)`[1], intervalos$`99.7% (3 SD)`[2])
  
  list(
    Parametros = list(Media = media, Desviacion = desviacion),
    Intervalos = intervalos,
    Conteo = list(
      `68% (1 SD)`   = res_68,
      `95% (2 SD)`   = res_95,
      `99.7% (3 SD)` = res_99
    )
  )
}