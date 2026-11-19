# Sección 7 de la propuesta (producto §5): requisitos técnicos y reproducibilidad.
#
# Verifica la integridad del archivo y de la muestra, registra versiones de R y de
# los paquetes, la semilla y la huella SHA-256 del archivo de datos, y escribe
# resultados_R/s7_entorno.txt. Conviene ejecutarlo primero.

source(if (dir.exists("codigo_R")) "codigo_R/comun.R" else "comun.R", encoding = "UTF-8")

PAQUETES <- c("tidyverse", "readr", "dplyr", "stringr", "ggplot2", "tidyr", "rmarkdown",
              "knitr", "glmnet", "ranger", "pROC", "car", "openssl")
SHA_ESPERADO <- "a236549cd369cd80bd478ff8e1779cbf44c58d5c3f79f7a51a1adbed7d06d1c6"

# Comprobaciones de reproducibilidad: existe el CSV, su SHA-256 es el esperado, los conteos coinciden
# con la documentación de UCI y la deduplicación da el resultado esperado. Registra las versiones
# de R y de los paquetes en resultados_R/s7_entorno.txt.
main <- function() {
  stopifnot("No se encontró el CSV. Ver README (carpeta data/)." = file.exists(RUTA_CSV))
  sha <- sha256_archivo()
  stopifnot("El SHA-256 no coincide con la versión analizada" = sha == SHA_ESPERADO)
  df <- cargar_datos(deduplicar = FALSE)
  if (MODO_RAPIDO) {
    registro("Prueba rápida: submuestra de ", nrow(df), " filas; se omiten los conteos del archivo completo")
  } else {
    stopifnot("El número de filas no coincide con la documentación de UCI" = nrow(df) == 235795)
    stopifnot("Conteo de legítimos distinto a UCI" = sum(df[[ETIQUETA_ORIGINAL]] == 1) == 134850)
    stopifnot("Conteo de phishing distinto a UCI" = sum(df[[ETIQUETA_ORIGINAL]] == 0) == 100945)
  }
  esperadas <- c(IDENTIFICADORES, TEXTO_LIBRE, ALTA_CARDINALIDAD, ETIQUETA_ORIGINAL, PREDICTORES)
  stopifnot("Las columnas no coinciden con el diccionario" =
              setequal(esperadas, setdiff(names(df), OBJETIVO)))
  stopifnot(length(PREDICTORES) == 50, !anyDuplicated(PREDICTORES))
  stopifnot("Hay valores faltantes" = sum(is.na(df)) == 0)
  n_dedup <- sum(!duplicated(df$URL))
  if (!MODO_RAPIDO) stopifnot("Muestra deduplicada inesperada" = n_dedup == 235370)

  lineas <- c("Entorno de ejecución del análisis", R.version.string,
              paste("Plataforma:", R.version$platform))
  for (p in PAQUETES) {
    v <- if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "no instalado"
    lineas <- c(lineas, sprintf("%s: %s", p, v))
  }
  pandoc <- if (requireNamespace("rmarkdown", quietly = TRUE) && asegurar_pandoc())
    as.character(rmarkdown::pandoc_version()) else "no disponible"
  lineas <- c(lineas,
              sprintf("pandoc: %s", pandoc),
              sprintf("Generador aleatorio: %s", paste(RNGkind(), collapse = " / ")),
              sprintf("Semilla aleatoria: %d", SEMILLA),
              sprintf("Archivo de datos: %s", basename(RUTA_CSV)),
              sprintf("SHA-256 del archivo: %s", sha),
              sprintf("Tamaño del archivo: %s bytes", format(file.size(RUTA_CSV), big.mark = ",")),
              sprintf("Filas en el archivo: %d | filas tras deduplicar por URL: %d", nrow(df), n_dedup),
              "Verificaciones superadas: SHA-256, conteos de UCI, columnas, faltantes, predictores y deduplicación.")
  writeLines(lineas, ruta_res("s7_entorno.txt"), useBytes = FALSE)
  cat(lineas, sep = "\n")
  cat("Escrito: resultados_R/s7_entorno.txt\n")
}

if (sys.nframe() == 0L) main()
