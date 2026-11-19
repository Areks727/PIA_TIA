# Utilidades compartidas por las secciones 4 a 7 (traducción a R de comun.py).
#
# Todas las secciones usan estas mismas definiciones (muestra, clase positiva,
# conjunto de predictores y partición) para que las comparaciones sean válidas.
# Los scripts se ejecutan desde la carpeta del proyecto (la que contiene codigo_R/):
#   Rscript codigo_R/seccion_7_reproducibilidad.R

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

SEMILLA <- 2026L

# --- Rutas relativas -----------------------------------------------------------
# La raíz del proyecto es la carpeta de trabajo si contiene codigo_R/; si el script
# se ejecuta desde codigo_R/, la raíz es la carpeta superior.
RAIZ <- if (dir.exists("codigo_R")) "." else if (file.exists("comun.R")) ".." else "."
NOMBRE_CSV <- "PhiUSIIL_Phishing_URL_Dataset.csv"
# Se busca primero en data/ (convención del curso) y después en la raíz del proyecto.
RUTA_CSV <- local({
  cand <- file.path(RAIZ, c("data", "."), NOMBRE_CSV)
  hit <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else cand[1]
})
# Prueba rápida (smoke test): con la variable de entorno PIA_PRUEBA_RAPIDA=1 todas las
# secciones trabajan sobre una submuestra estratificada de N_PRUEBA_RAPIDA filas y
# escriben en resultados_R/prueba_rapida/, sin tocar los resultados reales. Sirve para
# detectar errores de código antes de lanzar una corrida larga.
MODO_RAPIDO <- identical(Sys.getenv("PIA_PRUEBA_RAPIDA"), "1")
N_PRUEBA_RAPIDA <- 5000L
RUTA_RESULTADOS <- file.path(RAIZ, "resultados_R", if (MODO_RAPIDO) "prueba_rapida" else "")
RUTA_RESULTADOS <- sub("/$", "", RUTA_RESULTADOS)
dir.create(RUTA_RESULTADOS, showWarnings = FALSE, recursive = TRUE)
# Ruta de un archivo dentro de resultados_R/ (o de resultados_R/prueba_rapida/ en modo rápido).
ruta_res <- function(archivo) file.path(RUTA_RESULTADOS, archivo)

# --- Definición de variables -----------------------------------------------------
# Documentación de UCI: label = 1 corresponde a URL legítima y label = 0 a phishing.
# La clase positiva del análisis es phishing, por eso se crea phishing = 1 - label.
ETIQUETA_ORIGINAL <- "label"
OBJETIVO <- "phishing"

IDENTIFICADORES <- c("FILENAME", "URL", "Domain")
TEXTO_LIBRE <- c("Title")
ALTA_CARDINALIDAD <- c("TLD")  # 695 niveles; se resume con TLDLegitimateProb y TLDLength

SIMILITUD <- c("URLSimilarityIndex")
LEXICAS_URL <- c(
  "URLLength", "DomainLength", "IsDomainIP", "TLDLength", "NoOfSubDomain",
  "HasObfuscation", "NoOfObfuscatedChar", "ObfuscationRatio",
  "NoOfLettersInURL", "LetterRatioInURL", "NoOfDegitsInURL", "DegitRatioInURL",
  "NoOfEqualsInURL", "NoOfQMarkInURL", "NoOfAmpersandInURL",
  "NoOfOtherSpecialCharsInURL", "SpacialCharRatioInURL", "IsHTTPS",
  "CharContinuationRate", "URLCharProb", "TLDLegitimateProb"
)
DE_PAGINA <- c(
  "DomainTitleMatchScore", "URLTitleMatchScore", "LineOfCode", "LargestLineLength",
  "HasTitle", "HasFavicon", "Robots", "IsResponsive", "NoOfURLRedirect",
  "NoOfSelfRedirect", "HasDescription", "NoOfPopup", "NoOfiFrame",
  "HasExternalFormSubmit", "HasSocialNet", "HasSubmitButton", "HasHiddenFields",
  "HasPasswordField", "Bank", "Pay", "Crypto", "HasCopyrightInfo", "NoOfImage",
  "NoOfCSS", "NoOfJS", "NoOfSelfRef", "NoOfEmptyRef", "NoOfExternalRef"
)
PREDICTORES <- c(LEXICAS_URL, SIMILITUD, DE_PAGINA)  # 50 candidatas numéricas o binarias
COLUMNAS_TEXTO <- c(IDENTIFICADORES, TEXTO_LIBRE, ALTA_CARDINALIDAD)

# --- Carga de datos --------------------------------------------------------------
cargar_datos <- function(deduplicar = TRUE) {
  # Tipos explícitos: texto para identificadores, título y TLD; numérico para el resto.
  # readr::cols explícito: un objeto llamado 'cols' en el entorno global lo ocultaría.
  tipos <- do.call(readr::cols, c(list(.default = col_double()),
                           setNames(rep(list(col_character()), length(COLUMNAS_TEXTO)),
                                    COLUMNAS_TEXTO)))
  df <- read_csv(RUTA_CSV, col_types = tipos, na = c("", "NA"),
                 locale = locale(encoding = "UTF-8"), progress = FALSE)
  df <- as.data.frame(df)
  df[[OBJETIVO]] <- as.integer(1 - df[[ETIQUETA_ORIGINAL]])
  if (deduplicar) {
    df <- df[!duplicated(df$URL), , drop = FALSE]  # conserva la primera aparición
    rownames(df) <- NULL
  }
  if (MODO_RAPIDO) df <- submuestra_estratificada(df, N_PRUEBA_RAPIDA)
  df
}

# Submuestra estratificada por clase (solo para la prueba rápida).
submuestra_estratificada <- function(df, n, semilla = SEMILLA) {
  set.seed(semilla)
  y <- df[[OBJETIVO]]
  idx <- unlist(lapply(split(seq_along(y), y), function(ix)
    ix[sample.int(length(ix), round(as.numeric(n) * length(ix) / length(y)))]), use.names = FALSE)
  out <- df[sort(idx), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Huella SHA-256 del CSV. Sirve para comprobar que todo el equipo analiza exactamente la misma
# versión del archivo (la sección 7 la compara con SHA_ESPERADO).
sha256_archivo <- function(ruta = RUTA_CSV) {
  con <- file(ruta, open = "rb")
  on.exit(close(con))
  h <- openssl::sha256(con)
  paste(as.character(h), collapse = "")  # texto plano, sin atributos de clase
}

# rmarkdown necesita pandoc. Si no está en el PATH, se busca en la instalación por
# usuario de Windows (%LOCALAPPDATA%/Pandoc) sin escribir rutas absolutas en el código.
asegurar_pandoc <- function() {
  if (rmarkdown::pandoc_available()) return(invisible(TRUE))
  cand <- file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc")
  if (nzchar(Sys.getenv("LOCALAPPDATA")) && file.exists(file.path(cand, "pandoc.exe"))) {
    Sys.setenv(RSTUDIO_PANDOC = cand)
  }
  invisible(rmarkdown::pandoc_available())
}

# --- Partición y validación cruzada estratificadas ----------------------------------
# Partición 75/25 estratificada por clase con semilla fija. Devuelve índices de fila.
# El tamaño de prueba por clase es ceiling(0.25 * n_clase) (sklearn redondea hacia
# arriba el total; aquí se redondea por clase y se reporta el tamaño resultante).
particion_estratificada <- function(y, prop_prueba = 0.25, semilla = SEMILLA) {
  set.seed(semilla)
  prueba <- unlist(lapply(split(seq_along(y), y), function(ix) {
    ix[sample.int(length(ix), ceiling(prop_prueba * length(ix)))]
  }), use.names = FALSE)
  prueba <- sort(prueba)
  list(entrenamiento = setdiff(seq_along(y), prueba), prueba = prueba)
}

# Asigna cada observación a uno de k pliegues, estratificando por clase.
pliegues_estratificados <- function(y, k, semilla = SEMILLA) {
  set.seed(semilla)
  f <- integer(length(y))
  for (ix in split(seq_along(y), y)) {
    f[ix] <- sample(rep_len(seq_len(k), length(ix)))
  }
  f
}

# --- Filtro VIF -----------------------------------------------------------------------
# Elimina, una a una, la variable con mayor VIF hasta que todas queden <= umbral.
# VIF_j = diagonal de la inversa de la matriz de correlación (equivale a 1 / (1 - R2_j)).
# Solo usa el entrenamiento y no consulta la variable respuesta.
seleccionar_por_vif <- function(df_train, candidatas, umbral = 10) {
  cols <- candidatas
  eliminadas <- data.frame(variable_eliminada = character(0), VIF_en_entrenamiento = numeric(0))
  repeat {
    R <- suppressWarnings(cor(as.matrix(df_train[, cols])))
    R[is.na(R)] <- 0
    diag(R) <- 1
    inv <- tryCatch(solve(R), error = function(e) MASS::ginv(R))
    vif <- diag(inv)
    i <- which.max(vif)
    if (vif[i] <= umbral) return(list(cols = cols, eliminadas = eliminadas, vif = vif))
    eliminadas <- rbind(eliminadas, data.frame(variable_eliminada = cols[i],
                                               VIF_en_entrenamiento = unname(vif[i])))
    cols <- cols[-i]
  }
}

# --- Métricas -------------------------------------------------------------------------
# Matriz de confusión y métricas operativas para un umbral de decisión (p >= umbral).
metricas_en_umbral <- function(y, p, umbral) {
  yhat <- as.integer(p >= umbral)
  # Conteos en doble precisión: los productos entre conteos enteros desbordan en R (> 2^31).
  vn <- as.numeric(sum(y == 0 & yhat == 0)); fp <- as.numeric(sum(y == 0 & yhat == 1))
  fn <- as.numeric(sum(y == 1 & yhat == 0)); vp <- as.numeric(sum(y == 1 & yhat == 1))
  sens <- vp / (vp + fn)
  esp <- vn / (vn + fp)
  prec <- if ((vp + fp) > 0) vp / (vp + fp) else NA_real_
  f1 <- if (!is.na(prec) && (prec + sens) > 0) 2 * prec * sens / (prec + sens) else NA_real_
  tasa_fp <- fp / (fp + vn)
  tasa_fn <- fn / (fn + vp)
  data.frame(umbral = umbral, VN = vn, FP = fp, FN = fn, VP = vp,
             sensibilidad = sens, especificidad = esp, precision = prec, F1 = f1,
             tasa_falsas_alarmas = tasa_fp,
             FP_por_10000_legitimas = 10000 * tasa_fp,
             FN_por_10000_phishing = 10000 * tasa_fn)
}

# AUC ROC por el estadístico de Mann-Whitney (empates con rango promedio).
auc_roc <- function(y, p) {
  r <- rank(p)
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Precisión promedio (misma definición que average_precision_score de sklearn).
precision_promedio <- function(y, p) {
  o <- order(p, decreasing = TRUE)
  y <- y[o]; p <- p[o]
  ultimo <- c(diff(p) != 0, TRUE)          # último índice de cada umbral distinto
  vp <- cumsum(y)[ultimo]; tot <- seq_along(y)[ultimo]
  prec <- vp / tot; rec <- vp / sum(y)
  sum(diff(c(0, rec)) * prec)
}

# Log-loss (entropía cruzada) promedio: penaliza más las probabilidades seguras y equivocadas.
# Se recorta p a [eps, 1 - eps] para evitar log(0).
perdida_log <- function(y, p, eps = 1e-15) {
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# Puntaje de Brier: error cuadrático medio entre la probabilidad predicha y la clase real (0 o 1).
brier <- function(y, p) mean((p - y)^2)

# --- Estandarización aprendida solo con entrenamiento -----------------------------------
# Igual que StandardScaler de sklearn: media y desviación estándar con divisor n.
ajustar_escala <- function(X) {
  mu <- colMeans(X)
  sd <- sqrt(colMeans(sweep(X, 2, mu)^2))
  sd[sd == 0] <- 1
  list(mu = mu, sd = sd)
}
# Aplica a nuevos datos la media y desviación estándar aprendidas con ajustar_escala()
# (usa siempre los valores del entrenamiento para no filtrar información de la prueba).
aplicar_escala <- function(X, esc) sweep(sweep(X, 2, esc$mu), 2, esc$sd, "/")

# Equivalencia de penalizaciones: sklearn minimiza C * sum(pérdida) + ||b||^2 / 2;
# glmnet (alpha = 0) minimiza sum(pérdida) / n + lambda * ||b||^2 / 2.
# Dividiendo la primera entre C * n: lambda = 1 / (C * n).
lambda_desde_C <- function(C, n) 1 / (C * n)

# Guarda una tabla en resultados_R/ como CSV UTF-8 con BOM, para que Excel respete los acentos.
guardar_csv <- function(x, archivo) {
  write_excel_csv(as.data.frame(x), ruta_res(archivo))  # UTF-8 con BOM, como en Python
}

# --- Registro --------------------------------------------------------------------------
# Cada línea lleva marca de hora. Si se llamó a iniciar_log(), la línea también se añade
# al archivo de log abriéndolo y cerrándolo en cada escritura (sin búfer): si el proceso
# se detiene, el log conserva todo lo escrito hasta ese momento.
.estado_log <- new.env()
.estado_log$archivo <- NULL
# Define el archivo de log (resultados_R/logs/<nombre>.log) donde registro() copiará cada línea.
iniciar_log <- function(nombre) {
  dir.create(ruta_res("logs"), showWarnings = FALSE, recursive = TRUE)
  .estado_log$archivo <- ruta_res(file.path("logs", paste0(nombre, ".log")))
  invisible(.estado_log$archivo)
}
# Escribe líneas en pantalla y, si hay log activo, también en el archivo, sin búfer.
escribir_log <- function(lineas) {
  cat(c(lineas, ""), sep = "\n"); flush.console()
  if (!is.null(.estado_log$archivo))
    cat(c(lineas, ""), file = .estado_log$archivo, sep = "\n", append = TRUE)
}
# Escribe un mensaje con marca de hora; es la función de registro que usan todas las secciones.
registro <- function(...) {
  escribir_log(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...)))
}
# Imprime una tabla (data.frame) en el registro, sin números de fila.
registro_tabla <- function(x, ...) {
  escribir_log(capture.output(print(x, row.names = FALSE, ...)))
}
