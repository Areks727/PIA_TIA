# Lanzador de la sección 6 por etapas.
#
# Cada etapa corre en un proceso de R independiente. El límite de tiempo se controla desde
# este proceso (setTimeLimit no interrumpe el código compilado de glmnet): si una etapa lo
# supera, se detiene su árbol de procesos por PID y se registra como "tiempo agotado".
# El PID de cada etapa se escribe en resultados_R/etapas/<etapa>.pid al lanzarla y el
# resultado de cada intento se añade a resultados_R/etapas/estado.csv.
#
# Uso (desde la carpeta del proyecto):
#   Rscript codigo_R/ejecutar_s6.R todas              # todas las etapas pendientes, en orden
#   Rscript codigo_R/ejecutar_s6.R cv_logistica       # una o varias etapas por nombre
#   Rscript codigo_R/ejecutar_s6.R todas --rehacer    # repite también las ya terminadas
# Una etapa ya terminada (existe su .rds) se omite salvo con --rehacer. Si una etapa falla,
# se omiten las que dependen de ella y se sigue con las demás.

local({
  base <- if (dir.exists("codigo_R")) "codigo_R" else "."
  # Solo se cargan las definiciones (lista de etapas, dependencias y rutas); no se ejecuta nada.
  source(file.path(base, "seccion_6_modelacion_evaluacion.R"), encoding = "UTF-8", local = FALSE)
})
stopifnot("Falta el paquete processx" = requireNamespace("processx", quietly = TRUE))

# Límite de tiempo por etapa, en minutos (holgado respecto de lo medido).
LIMITES_MIN <- c(preparar = 10, separacion = 20, cv_logistica = 30, cv_bosque = 30,
                 ajuste_final = 20, umbrales_oof = 30, bootstrap_prueba = 15, bootstrap = 120,
                 influencia = 60, sens_sin_similitud = 15, sens_lexicas = 15, sens_html = 15,
                 sens_url_recalculadas = 15, sens_razones_nchar = 15, sens_dominio = 15, sens_sin_tld = 15,
                 sens_formato_bosque = 15, sens_formato_logistica = 5, consolidar = 10)
# La etapa sens_formato_logistica solo hace un ajuste logístico: su límite de 5 min es el
# límite por ajuste pedido para ese escenario.

lanzar_etapa <- function(etapa, rscript, script) {
  dir.create(ruta_res("etapas"), showWarnings = FALSE, recursive = TRUE)
  dir.create(ruta_res("logs"), showWarnings = FALSE, recursive = TRUE)
  salida <- ruta_res(file.path("logs", paste0("s6_", etapa, ".salida.txt")))
  limite <- LIMITES_MIN[[etapa]]
  inicio <- Sys.time()
  p <- processx::process$new(rscript, c(script, etapa), stdout = salida, stderr = "2>&1",
                             cleanup_tree = TRUE, supervise = TRUE)
  pid <- p$get_pid()
  writeLines(as.character(pid), ruta_etapa(etapa, "pid"))
  registro(sprintf("-> etapa '%s' lanzada (PID %d, límite %g min, salida en %s)", etapa, pid, limite, salida))
  p$wait(timeout = limite * 60 * 1000)
  if (p$is_alive()) {
    p$kill_tree()
    estado <- "tiempo agotado"; codigo <- NA_integer_
  } else {
    codigo <- p$get_exit_status()
    estado <- if (identical(codigo, 0L) && etapa_hecha(etapa)) "ok" else "error"
  }
  seg <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  fila <- data.frame(etapa = etapa, pid = pid, inicio = format(inicio, "%Y-%m-%d %H:%M:%S"),
                     fin = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), segundos = round(seg),
                     limite_min = limite, codigo_salida = codigo, estado = estado,
                     prueba_rapida = MODO_RAPIDO)
  archivo <- ruta_etapa("estado", "csv")
  write.table(fila, archivo, sep = ",", row.names = FALSE, append = file.exists(archivo),
              col.names = !file.exists(archivo), fileEncoding = "UTF-8")
  if (file.exists(ruta_etapa(etapa, "pid"))) file.remove(ruta_etapa(etapa, "pid"))
  registro(sprintf("<- etapa '%s': %s en %.0f s", etapa, estado, seg))
  if (estado != "ok") {
    cola <- tail(readLines(salida, warn = FALSE, encoding = "UTF-8"), 15)
    registro("   últimas líneas de la salida:"); escribir_log(paste("   |", cola))
  }
  estado
}

# Lanza cada etapa de la sección 6 como un proceso de R independiente con límite de tiempo externo,
# guarda su PID y registra el estado (etapas/estado.csv). Si una etapa falla, las demás no se pierden.
main_lanzador <- function(args) {
  rehacer <- "--rehacer" %in% args
  args <- setdiff(args, "--rehacer")
  if (!length(args)) stop("Indica 'todas' o nombres de etapa: ", paste(ETAPAS_ORDEN, collapse = ", "))
  etapas <- if (identical(args, "todas")) setdiff(ETAPAS_ORDEN, "bootstrap_prueba") else args
  desconocidas <- setdiff(etapas, ETAPAS_ORDEN)
  if (length(desconocidas)) stop("Etapas desconocidas: ", paste(desconocidas, collapse = ", "))
  iniciar_log("s6_lanzador")
  dir.create(ruta_res("etapas"), showWarnings = FALSE, recursive = TRUE)
  writeLines(as.character(Sys.getpid()), ruta_etapa("lanzador", "pid"))
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  script <- normalizePath(file.path(if (dir.exists("codigo_R")) "codigo_R" else ".",
                                    "seccion_6_modelacion_evaluacion.R"))
  registro(sprintf("Lanzador de la sección 6 (PID %d%s): %s", Sys.getpid(),
                   if (MODO_RAPIDO) ", PRUEBA RÁPIDA" else "", paste(etapas, collapse = ", ")))
  estados <- character(0)
  for (e in etapas) {
    if (etapa_hecha(e) && !rehacer && e != "consolidar") { registro("   '", e, "' ya terminada: se omite"); estados[e] <- "ya hecha"; next }
    faltan <- DEPENDENCIAS[[e]][!vapply(DEPENDENCIAS[[e]], etapa_hecha, logical(1))]
    if (length(faltan)) {
      registro(sprintf("   '%s' omitida: falta %s", e, paste(faltan, collapse = ", ")))
      estados[e] <- "omitida"; next
    }
    if (rehacer && etapa_hecha(e)) file.remove(ruta_etapa(e))
    estados[e] <- lanzar_etapa(e, rscript, script)
  }
  file.remove(ruta_etapa("lanzador", "pid"))
  registro("Resumen: ", paste(sprintf("%s=%s", names(estados), estados), collapse = " | "))
  invisible(estados)
}

if (sys.nframe() == 0L) {
  est <- main_lanzador(commandArgs(trailingOnly = TRUE))
  if (any(est %in% c("error"))) quit(status = 1)
}
