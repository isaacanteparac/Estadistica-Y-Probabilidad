# 1. Rango (usando diff y range nativos)
calcular_rango <- function(vector) {
  vector <- vector[!is.na(vector)]
  if (length(vector) < 2) return(NA)
  diff(range(vector))
}

# 2. Varianza (usando var nativa + ajuste si es poblacional)
calcular_varianza <- function(vector, is_muestral = TRUE) {
  vector <- vector[!is.na(vector)]
  n <- length(vector)
  if (n < 2) return(NA)
  
  v_muestral <- var(vector)
  
  if (is_muestral) {
    return(v_muestral)
  } else {
    # Ajuste de (n-1)/n para convertir la varianza muestral a poblacional
    return(v_muestral * (n - 1) / n)
  }
}

# 3. Desviación Estándar (usando sd nativa + ajuste si es poblacional)
calcular_desviacion <- function(vector, is_muestral = TRUE) {
  vector <- vector[!is.na(vector)]
  n <- length(vector)
  if (n < 2) return(NA)
  
  if (is_muestral) {
    return(sd(vector))
  } else {
    # Convertir desviación muestral a poblacional
    return(sd(vector) * sqrt((n - 1) / n))
  }
}

# Función integradora
resumen_dispersion <- function(datos, is_muestral) {
  tipo <- if (is_muestral) "Muestral" else "Poblacional"
  list(
    Tipo_Calculo        = tipo,
    Rango               = calcular_rango(datos),
    Varianza            = calcular_varianza(datos, is_muestral),
    Desviacion_Estandar = calcular_desviacion(datos, is_muestral)
  )
}

# Crear un conjunto de datos numéricos de prueba
datos_ejemplo <- c(2,3,3,4)
datos_ejemplo = sort(datos_ejemplo)
cat("datos:", datos_ejemplo, "\n")
# O invocar todas mediante la función integradora
resultados <- resumen_dispersion(datos_ejemplo, is_muestral = TRUE)
print(resultados)