# 1. Media (usando mean nativa)
calcular_media <- function(vector) {
  mean(vector, na.rm = TRUE)
}

# 2. Mediana (usando median nativa)
calcular_mediana <- function(vector) {
  median(vector, na.rm = TRUE)
}

# 3. Moda (R no tiene función nativa para la moda estadística)
calcular_moda <- function(vector) {
  vector <- vector[!is.na(vector)]
  if (length(vector) == 0) return(NA)
  
  frecuencias <- table(vector)
  max_frec <- max(frecuencias)
  
  if (all(frecuencias == max_frec) && max_frec == 1) return("No hay moda")
  
  modas <- names(frecuencias[frecuencias == max_frec])
  if (is.numeric(vector)) return(as.numeric(modas))
  return(modas)
}

# 4. Determinar el sesgo
determinar_sesgo <- function(vector, tolerancia = 1e-5) {
  media <- calcular_media(vector)
  mediana <- calcular_mediana(vector)
  
  if (is.na(media) || is.na(mediana)) return(NA)
  
  dif <- media - mediana
  if (abs(dif) < tolerancia) return("Simétrica (Media ≈ Mediana)")
  if (dif > 0) return("Sesgo a la Derecha / Positivo (Media > Mediana)")
  return("Sesgo a la Izquierda / Negativo (Media < Mediana)")
}

# Función integradora
resumen_tendencia_y_sesgo <- function(datos) {
  list(
    Media   = calcular_media(datos),
    Mediana = calcular_mediana(datos),
    Moda    = calcular_moda(datos),
    Sesgo   = determinar_sesgo(datos)
  )
}

# Crear un conjunto de datos numéricos de prueba
datos_ejemplo <- c(12, 17, 12, 18, 20, 15)
datos_ejemplo = sort(datos_ejemplo)
cat("datos:", datos_ejemplo, "\n")
# O invocar todas mediante la función integradora
resultados <- resumen_tendencia_y_sesgo(datos_ejemplo)
print(resultados)