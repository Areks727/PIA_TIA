# Prueba rápida (smoke test) de todas las secciones.
#
# Ejecuta las secciones 7, 4, 5 y todas las etapas de la sección 6 sobre una submuestra
# estratificada de 5,000 filas, con 2 pliegues y B = 5 en el bootstrap, y las pruebas de
# desbordamiento con valores grandes. Cada script corre en un proceso aparte con límite de
# tiempo. Los resultados van a resultados_R/prueba_rapida/ (se borra al empezar) y no tocan
# los resultados reales. Tiene que terminar sin errores antes de lanzar una corrida larga.
#
# Uso: Rscript codigo_R/prueba_rapida.R

stopifnot("Falta el paquete processx" = requireNamespace("processx", quietly = TRUE))
base <- if (dir.exists("codigo_R")) "codigo_R" else "."
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
destino <- file.path(if (base == "codigo_R") "." else "..", "resultados_R", "prueba_rapida")
if (dir.exists(destino)) unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)

pasos <- list(
  list("sección 7 (reproducibilidad)", "seccion_7_reproducibilidad.R", character(0), 5),
  list("sección 4 (datos)", "seccion_4_datos.R", character(0), 5),
  list("sección 5 (exploración e inferencia)", "seccion_5_exploracion_inferencia.R", character(0), 5),
  list("pruebas de desbordamiento", "pruebas_desbordamiento.R", character(0), 5),
  list("sección 6, todas las etapas", "ejecutar_s6.R", c("todas", "--rehacer"), 30),
  list("sección 6, medición del bootstrap", "ejecutar_s6.R", c("bootstrap_prueba", "--rehacer"), 10)
)
resultado <- data.frame(paso = character(0), estado = character(0), segundos = numeric(0))
for (p in pasos) {
  salida <- file.path(destino, paste0("salida_", sub("\\.R$", "", p[[2]]), if (length(p[[3]])) paste0("_", p[[3]][1]) else "", ".txt"))
  t0 <- Sys.time()
  pr <- processx::process$new(rscript, c(file.path(base, p[[2]]), p[[3]]), stdout = salida, stderr = "2>&1",
                              env = c("current", PIA_PRUEBA_RAPIDA = "1"), cleanup_tree = TRUE, supervise = TRUE)
  cat(sprintf("[%s] %s (PID %d) ...\n", format(Sys.time(), "%H:%M:%S"), p[[1]], pr$get_pid()))
  pr$wait(timeout = p[[4]] * 60 * 1000)
  estado <- if (pr$is_alive()) { pr$kill_tree(); "tiempo agotado" } else if (pr$get_exit_status() == 0L) "ok" else "error"
  seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("           %s en %.0f s\n", estado, seg))
  if (estado != "ok") cat(paste("           |", tail(readLines(salida, warn = FALSE, encoding = "UTF-8"), 20)), sep = "\n")
  resultado[nrow(resultado) + 1, ] <- list(p[[1]], estado, round(seg))
}

# Comprobaciones adicionales de la sección 6: todas las etapas guardaron su resultado.
estado_s6 <- file.path(destino, "etapas", "estado.csv")
if (file.exists(estado_s6)) {
  e <- read.csv(estado_s6, fileEncoding = "UTF-8")
  cat("\nEtapas de la sección 6:\n"); print(e[, c("etapa", "segundos", "estado")], row.names = FALSE)
  nc <- file.path(destino, "s6_no_convergidos.csv")
  if (file.exists(nc)) cat(sprintf("Ajustes marcados como no convergidos en la prueba rápida: %d\n",
                                   nrow(read.csv(nc, fileEncoding = "UTF-8-BOM"))))
  malas <- e$estado[e$estado != "ok" & e$etapa != "sens_formato_logistica"]
  if (length(malas)) resultado[nrow(resultado) + 1, ] <- list("etapas de la sección 6 con problemas", "error", NA)
}
cat("\nResumen de la prueba rápida:\n"); print(resultado, row.names = FALSE)
if (any(resultado$estado != "ok")) { cat("\nLA PRUEBA RÁPIDA FALLÓ\n"); quit(status = 1) }
cat("\nPrueba rápida superada: se puede lanzar la corrida completa.\n")
