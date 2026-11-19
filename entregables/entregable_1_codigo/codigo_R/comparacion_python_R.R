# Comparación entre las cifras de la versión en Python y las de R.
#
# Las cifras de Python son las de la propuesta (Propuesta_de_Proyecto_completa.docx) y las
# cifras de validación que dio el equipo; están escritas aquí solo como referencia. Las de R
# se leen de resultados_R/ (nunca se copian de Python). Cada fila tiene una tolerancia
# explícita y un estado:
#   - "exacta": debe coincidir; estado "coincide" o "discrepa".
#   - "aproximada": la partición, los pliegues, el bosque y el bootstrap son distintos en R,
#     así que solo se espera que se parezca; estado "parecida" o "discrepa".
# Escribe resultados_R/comparacion_python_R.csv.

source(if (dir.exists("codigo_R")) "codigo_R/comun.R" else "comun.R", encoding = "UTF-8")
# Lee un CSV de resultados_R/.
leer <- function(a) read.csv(ruta_res(a), fileEncoding = "UTF-8-BOM", check.names = FALSE)

filas <- list()
# Agrega una fila a la comparación: cifra de Python, cifra de R, tolerancia y estado
# (coincide / parecida / discrepa).
fila <- function(grupo, cifra, python, r, tipo, tolerancia, criterio = NULL, nota = "") {
  pn <- suppressWarnings(as.numeric(python)); rn <- suppressWarnings(as.numeric(r))
  ok <- if (!is.null(criterio)) criterio
        else if (!is.na(pn) && !is.na(rn)) abs(pn - rn) <= tolerancia + 1e-12
        else identical(as.character(python), as.character(r))
  estado <- if (tipo == "exacta") (if (isTRUE(ok)) "coincide" else "discrepa") else (if (isTRUE(ok)) "parecida" else "discrepa")
  filas[[length(filas) + 1]] <<- data.frame(
    grupo = grupo, cifra = cifra, python = as.character(python), R = as.character(r),
    tipo = tipo, tolerancia = if (is.character(tolerancia)) tolerancia else format(tolerancia),
    estado = estado, nota = nota)
}

# --- Cifras que deben coincidir exactamente -------------------------------------------
ctl <- leer("s4_control_calidad.csv"); v <- setNames(ctl$valor, ctl$concepto)
fila("datos", "filas del archivo", 235795, v[["filas en el archivo"]], "exacta", 0)
fila("datos", "columnas del archivo", 56, v[["columnas en el archivo"]], "exacta", 0)
fila("datos", "legítimas (label = 1)", 134850, v[["label = 1 (legítimo)"]], "exacta", 0)
fila("datos", "phishing (label = 0)", 100945, v[["label = 0 (phishing)"]], "exacta", 0)
fila("datos", "valores faltantes", 0, v[["valores faltantes en todo el archivo"]], "exacta", 0)
fila("datos", "SHA-256 del CSV", "a236549cd369cd80bd478ff8e1779cbf44c58d5c3f79f7a51a1adbed7d06d1c6",
     sha256_archivo(), "exacta", "idéntico")
fila("deduplicación", "filas eliminadas", 425, v[["filas que se eliminan al deduplicar"]], "exacta", 0)
fila("deduplicación", "clase de las eliminadas", "1: 425 (todas phishing)", paste0(v[["clase de las filas eliminadas (phishing=1)"]], " (todas phishing)"), "exacta", "idéntico")
fila("deduplicación", "filas tras deduplicar", 235370, v[["filas tras deduplicar"]], "exacta", 0)
inf <- leer("s5_inferencia.csv"); rownames(inf) <- inf$id
fila("deduplicación", "phishing tras deduplicar", 100520, inf["I1", "n"] * inf["I1", "estimacion"], "exacta", 1e-6)
for (x in list(list("I1", "prevalencia de phishing", c(0.427072, 0.425075, 0.429072), 5e-7),
               list("I2", "diferencia de proporciones de HTTPS", c(-0.508774, -0.511865, -0.505684), 5e-7),
               list("I3", "probabilidad de superioridad de la longitud", c(0.721065, 0.718951, 0.723179), 5e-7),
               list("I4", "razón de momios HasSocialNet", c(756.923, 693.214, 826.487), 5e-4))) {
  r <- unlist(inf[x[[1]], c("estimacion", "IC95_inf", "IC95_sup")])
  for (k in 1:3) fila("inferencia", sprintf("%s %s: %s", x[[1]], x[[2]], c("estimación", "IC 95% inferior", "IC 95% superior")[k]),
                     x[[3]][k], signif(r[[k]], 8), "exacta", x[[4]], nota = "tolerancia = medio dígito del último decimal informado")
}
reg <- leer("s5_reglas_deterministas.csv"); rv <- setNames(reg$valor, reg$indicador)
fila("regla", "regla de dos condiciones: falsas alarmas", 0, rv[["regla [sin HTTPS o similitud < 100]: falsas alarmas (n)"]], "exacta", 0)
fila("regla", "regla de dos condiciones: phishing detectado", 100487, rv[["regla [sin HTTPS o similitud < 100]: phishing detectado (n)"]], "exacta", 0)
fila("regla", "regla de dos condiciones: phishing omitido", 33, rv[["regla [sin HTTPS o similitud < 100]: phishing omitido (n)"]], "exacta", 0)
fila("regla", "phishing con formato comparable a legítimas", 972, rv[["phishing con formato comparable a legítimas (n)"]], "exacta", 0)
sel <- leer("s6_seleccion_predictores.csv")
fila("modelo", "variable eliminada por VIF", "DomainTitleMatchScore", paste(sel$variable_eliminada, collapse = ", "), "exacta", "idéntico")
fila("modelo", "número de predictores finales", 48, nrow(leer("s6_vif_final.csv")), "exacta", 0)
um1 <- leer("s6_umbrales_prueba.csv")[1, ]
fila("partición", "tamaño de la prueba", 58843, um1$VN + um1$FP + um1$FN + um1$VP, "exacta", 0)
fila("partición", "phishing en la prueba", 25130, um1$FN + um1$VP, "exacta", 0)

# --- Cifras que solo deben parecerse --------------------------------------------------
fila("modelo", "VIF de DomainTitleMatchScore", 16.96, round(sel$VIF_en_entrenamiento[1], 2), "aproximada", 0.5, nota = "depende de la partición")
cv <- leer("s6_cv_logistica.csv"); cvs <- leer("s6_regla_1se.csv")
fila("logística", "C elegido", 10, cvs$C_elegido[1], "aproximada", 0,
     nota = "Python: mínimo log-loss medio; R: regla de 1 EE (DE muestral / sqrt(5))")
fila("logística", "log-loss medio de CV con C = 10", 0.00048, signif(cv$logloss_media[cv$C == 10], 3), "aproximada", 3e-4)
fila("logística", "log-loss medio de CV con C = 1", 0.00055, signif(cv$logloss_media[cv$C == 1], 3), "aproximada", 3e-4)
cvb <- leer("s6_cv_bosque.csv"); mb <- cvb[which.min(cvb$logloss_media), ]
fila("bosque", "profundidad máxima elegida", 20, mb$max_depth, "aproximada", 0)
fila("bosque", "mínimo de observaciones por hoja elegido", 1, mb$min_bucket, "aproximada", 0, nota = "ranger min.bucket = min_samples_leaf")
fila("bosque", "log-loss medio de CV del bosque elegido", 0.00300, signif(mb$logloss_media, 3), "aproximada", 2e-3)
res <- leer("s6_resumen_prueba.csv"); rownames(res) <- res$modelo
py_res <- list("logística L2" = c(AUC_ROC = 0.999999948, AP_precision_recall = 0.99999993, log_loss = 0.00031, Brier = 0.000059),
               "bosque aleatorio" = c(AUC_ROC = 1.00000, AP_precision_recall = 1.00000, log_loss = 0.00078, Brier = 0.000042))
tol_res <- c(AUC_ROC = 1e-4, AP_precision_recall = 1e-4, log_loss = 1e-3, Brier = 1e-4)
for (m in names(py_res)) for (k in names(py_res[[m]]))
  fila("prueba", sprintf("%s: %s", m, k), py_res[[m]][[k]], signif(res[m, k], 6), "aproximada", tol_res[[k]])
um <- leer("s6_umbrales_prueba.csv")
py_um <- list("logística L2" = rbind(FP = c(1, 0, 0, 0, 0), FN = c(2, 3, 4, 5, 11)),
              "bosque aleatorio" = rbind(FP = c(12, 0, 0, 0, 0), FN = c(0, 0, 1, 5, 16)))
for (m in names(py_um)) for (j in seq_along(c(0.1, 0.3, 0.5, 0.7, 0.9))) for (e in c("FP", "FN")) {
  u <- c(0.1, 0.3, 0.5, 0.7, 0.9)[j]
  fila("prueba por umbral", sprintf("%s, umbral %.2f: %s", m, u, e), py_um[[m]][e, j],
       um[um$modelo == m & abs(um$umbral - u) < 1e-9, e], "aproximada", 10,
       nota = "tolerancia de 10 URL en 58,843 (partición distinta)")
}
sh <- leer("s6_influencia_sombrero.csv")
fila("influencia", "observaciones con |residuo de Pearson| > 3", 9, sh$n_residuo_pearson_mayor_3, "aproximada", 10)
orr <- leer("s6_logistica_or.csv"); rownames(orr) <- orr$variable
for (x in list(c("NoOfExternalRef", 0.204), c("NoOfImage", 0.208), c("NoOfSelfRef", 0.157), c("NoOfJS", 0.134),
               c("HasSocialNet", 0.032), c("URLSimilarityIndex", 2.2e-4))) {
  col <- if (x[1] == "HasSocialNet") "OR_por_unidad" else "OR_por_DE"
  r <- orr[x[1], col]; p <- as.numeric(x[2])
  fila("razones de momios", sprintf("OR de %s (%s)", x[1], if (x[1] == "HasSocialNet") "presencia" else "por DE"),
       p, signif(r, 3), "aproximada", "misma dirección (OR < 1 o > 1)",
       criterio = !is.na(r) && sign(log(r)) == sign(log(p)),
       nota = "con separación cuasi completa solo la dirección es interpretable")
}
sens <- leer("s6_sensibilidad.csv")
py_sens <- list(
  "principal" = c(48, 1.000000, 1.000000, 0, 4, 0, 1),
  "sin índice de similitud" = c(47, 0.999993, 0.999999, 14, 18, 2, 6),
  "solo variables léxicas del dataset" = c(20, 0.998764, 0.999252, 5, 185, 21, 137),
  "solo variables de página HTML" = c(27, 0.999764, 0.999938, 178, 148, 34, 111),
  "solo variables recalculadas desde la URL" = c(16, 0.996120, 0.997289, 21, 382, 35, 233),
  "partición por dominio (sin dominios compartidos)" = c(48, 0.999991, 1.000000, 0, 10, 0, 2),
  "subconjunto con formato comparable a legítimas" = c(48, 0.999864, 1.000000, 0, 2, 0, 1))
for (e in names(py_sens)) {
  pv <- py_sens[[e]]
  for (m in c("logística L2", "bosque aleatorio")) {
    s <- sens[sens$escenario == e & sens$modelo == m, ]
    lr <- m == "logística L2"
    if (!nrow(s) || is.na(s$AUC)) {
      fila("sensibilidad", sprintf("%s, %s: AUC", e, m), pv[if (lr) 2 else 3], "sin resultado en R", "aproximada", 0.002,
           criterio = FALSE, nota = "ver limitación del escenario")
      next
    }
    nota_e <- if (startsWith(e, "subconjunto")) "R quita columnas constantes y casi constantes en este escenario" else ""
    fila("sensibilidad", sprintf("%s, %s: AUC", e, m), pv[if (lr) 2 else 3], signif(s$AUC, 6), "aproximada", 0.002, nota = nota_e)
    for (k in 1:2) {
      py <- pv[if (lr) 3 + k else 5 + k]; tol <- max(10, 0.5 * py)
      fila("sensibilidad", sprintf("%s, %s: %s a 0.50", e, m, c("FP", "FN")[k]), py, s[[c("FP", "FN")[k]]],
           "aproximada", tol, nota = paste("tolerancia max(10, 50 % del valor de Python)", nota_e))
    }
    if (e != "principal" && !startsWith(e, "subconjunto"))
      fila("sensibilidad", sprintf("%s, %s: número de predictores", e, m), pv[1], s$n_predictores, "exacta", 0)
  }
}
tab <- do.call(rbind, filas)
guardar_csv(tab, "comparacion_python_R.csv")
cat(sprintf("Filas: %d | coincide: %d | parecida: %d | discrepa: %d\n", nrow(tab),
            sum(tab$estado == "coincide"), sum(tab$estado == "parecida"), sum(tab$estado == "discrepa")))
print(tab[tab$estado == "discrepa", c("cifra", "python", "R", "tolerancia")], row.names = FALSE)
