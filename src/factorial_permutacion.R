# Función para calcular factoriales y permutaciones
# Retorna un diccionario (named list en R) con los resultados
calcular_permutaciones <- function(n, r = n) {
  if (is.na(n) || is.na(r) || n < 0 || r < 0 || r > n) {
    return(list(
      Error = "Parámetros inválidos: n y r deben ser números no negativos y r <= n",
      n = n,
      r = r,
      Permutaciones = NA
    ))
  }
  
  fact_n <- factorial(n)
  total_permutaciones <- choose(n, r) * factorial(r)
  
  list(
    n             = n,
    r             = r,
    Factorial_N   = fact_n,
    Permutaciones = total_permutaciones
  )
}