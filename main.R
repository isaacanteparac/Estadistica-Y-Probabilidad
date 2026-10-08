# ==============================================================================
# Script Principal (main.R)
# - Incorpora el módulo src/correccion_outlier.R para la corrección detallada de cada outlier.
# ==============================================================================

# 1. Cargar interfaz UI y módulos de cálculo puro (src/)
source("./ui.R")
source("./src/medidas_tendencia_central.R")
source("./src/medidas_dispersion.R")
source("./src/regla_empirica.R")
source("./src/valores_atipicos.R")
source("./src/factorial_permutacion.R")
source("./src/tabla-cruzada.R")
source("./src/correccion_outlier.R")

# 2. Cargar módulos de gráficos desde src/graficos/
source("./src/graficos/barra_diagrama.R")
source("./src/graficos/boxplot_diagrama.R")
source("./src/graficos/campana_diagrama.R")
source("./src/graficos/histograma_grafico.R")
source("./src/graficos/pareto_grafico.R")
source("./src/graficos/pie_grafico.R")
source("./src/graficos/tallos_hojas_grafico.R")

# ==============================================================================
# FUNCIÓN PRINCIPAL: resultado()
# ==============================================================================
resultado <- function(datos, nclases = 0, is_muestral = TRUE, n = 5, r = 3) {
  dict_tendencia   <- resumen_tendencia_y_sesgo(datos)
  dict_dispersion  <- resumen_dispersion(datos, is_muestral = is_muestral)
  dict_empirica    <- evaluar_regla_empirica(datos)
  dict_atipicos    <- analizar_valores_atipicos(datos)
  dict_tabla       <- analizar_tabla_cruzada(datos, nclases = nclases)
  dict_permutacion <- calcular_permutaciones(n = n, r = r)
  dict_correccion  <- correccion_outlier(datos)
  
  list(
    Datos             = datos,
    Tendencia         = dict_tendencia,
    Dispersion        = dict_dispersion,
    ReglaEmpirica     = dict_empirica,
    Atipicos          = dict_atipicos,
    CorreccionOutlier = dict_correccion,
    TablaCruzada      = dict_tabla,
    Permutaciones     = dict_permutacion
  )
}

# ==============================================================================
# VARIABLES PRINCIPALES (MISMO NIVEL)
# ==============================================================================

datoss <- c(12, 16, 5, 16, 21, 29, 38, 14, 47, 0, 24, 15, 13, 8, 2, 
                   11, 22, 17, 31, 10, 4, 10, 15, 7, 20, 9, 22, 18, 28, 19, 
                   34, 26, 17, 11, 64, 19, 18, 24, 49, 50)
datos = sort(datoss)
nclases <- 5

# ==============================================================================
# EJECUCIÓN PRINCIPAL
# ==============================================================================

banner("SISTEMA DE ANÁLISIS ESTADÍSTICO Y VISUALIZACIÓN")

res_master <- resultado(datos, nclases = nclases)

# ------------------------------------------------------------------------------
# IMPRESIÓN POR SECCIONES INDEPENDIENTES
# ------------------------------------------------------------------------------
title("SECCIÓN 1: MEDIDAS DE TENDENCIA CENTRAL Y SESGO")
imprimir_seccion("MEDIDAS DE TENDENCIA CENTRAL Y SESGO", res_master$Tendencia)
separador_seccion()

title("SECCIÓN 2: MEDIDAS DE DISPERSIÓN")
imprimir_seccion("MEDIDAS DE DISPERSIÓN", res_master$Dispersion)
separador_seccion()

title("SECCIÓN 3: EVALUACIÓN DE LA REGLA EMPÍRICA")
imprimir_seccion("REGLA EMPÍRICA E INTERVALOS SD", res_master$ReglaEmpirica)
separador_seccion()

title("SECCIÓN 4: ANÁLISIS DE VALORES ATÍPICOS (OUTLIERS)")
imprimir_seccion("DETECCIÓN DE VALORES ATÍPICOS", res_master$Atipicos)
separador_seccion()

title("SECCIÓN 5: CORRECCIÓN DETALLADA DE OUTLIERS POR PROMEDIO DE VECINOS")
imprimir_seccion("CORRECCIÓN INDIVIDUAL POR CADA OUTLIER DETECTADO", res_master$CorreccionOutlier)
separador_seccion()

title("SECCIÓN 6: TABLA DE FRECUENCIAS ESTADÍSTICA COMPLETA")
if (!is.null(res_master$TablaCruzada$Tabla_Frecuencias_DF)) {
  sub_titulo <- if (res_master$TablaCruzada$Usar_Intervalos) {
    paste0("TABLA DE FRECUENCIAS (nclases = ", nclases, " | P = ", res_master$TablaCruzada$Rango_P, " | Amplitud = ", round(res_master$TablaCruzada$Ancho_Intervalo, 4), ")")
  } else {
    "TABLA DE FRECUENCIAS POR VALORES INDIVIDUALES (nclases = 0)"
  }
  imprimir_tabla_frecuencia_df(res_master$TablaCruzada$Tabla_Frecuencias_DF, sub_titulo)
}
separador_seccion()

title("SECCIÓN 7: FACTORIALES Y PERMUTACIONES")
imprimir_seccion("CÁLCULO DE PERMUTACIONES P(n, r)", res_master$Permutaciones)
separador_seccion()

# ------------------------------------------------------------------------------
# EJECUCIÓN DE GRÁFICOS (SRC/GRAFICOS/)
# ------------------------------------------------------------------------------
title("SECCIÓN 8: GENERACIÓN Y VISUALIZACIÓN DE GRÁFICOS (SRC/GRAFICOS/)")

cat("► 1/7 Generando Histograma...\n")
graficar_histograma(res_master)

cat("► 2/7 Generando Campana de Gauss...\n")
graficar_campana(res_master)

cat("► 3/7 Generando Diagrama de Caja (Boxplot)...\n")
graficar_boxplot(res_master)

cat("► 4/7 Generando Diagrama de Barras...\n")
graficar_barra(res_master)

cat("► 5/7 Generando Diagrama de Pareto...\n")
graficar_pareto(res_master)

cat("► 6/7 Generando Diagrama de Pastel (Pie Chart)...\n")
graficar_pie(res_master)

cat("► 7/7 Generando Diagrama de Tallos y Hojas...\n")
graficar_tallos_hojas(res_master)

banner("PROCESO COMPLETADO EXITOSAMENTE")