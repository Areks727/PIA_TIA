# Sección 6 de la propuesta (producto §4): modelación y evaluación.
#
# Protocolo: lo que se aprende de los datos (predictores, escalas, lambda, profundidad
# y hojas) se decide solo con el entrenamiento. La partición de prueba se usa para
# reportar. Los umbrales se comparan; no se optimizan sobre la prueba.
#
# Equivalencias con la versión en Python:
# - LogisticRegression(C) de sklearn  ->  glmnet(alpha = 0, lambda = 1 / (C * n)) sobre
#   predictores estandarizados con media y desviación aprendidas en el entrenamiento.
# - RandomForestClassifier(max_depth, min_samples_leaf)  ->  ranger(max.depth, min.bucket),
#   con mtry = floor(sqrt(p)) (valor por omisión en ambos) y bootstrap con reemplazo.
# - StratifiedGroupKFold (sin equivalente directo en R)  ->  asignación aleatoria de
#   dominios a 4 pliegues, estratificada por la clase mayoritaria del dominio.
#
# Ejecución por etapas. Cada etapa guarda su resultado en resultados_R/etapas/<etapa>.rds
# al terminar; si una falla, las anteriores no se pierden y se puede reanudar:
#   Rscript codigo_R/seccion_6_modelacion_evaluacion.R <etapa>
# Lo recomendable es usar el lanzador, que corre cada etapa en un proceso de R aparte,
# con límite de tiempo controlado desde fuera y su número de proceso (PID) en un archivo:
#   Rscript codigo_R/ejecutar_s6.R todas

source(if (dir.exists("codigo_R")) "codigo_R/comun.R" else "comun.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(glmnet); library(ranger); library(ggplot2); library(pROC) })

CANDIDATAS <- setdiff(PREDICTORES, "URLLength")  # excluida: desfase ligado a la clase
UMBRALES <- c(0.10, 0.30, 0.50, 0.70, 0.90)
C_GRID <- c(0.01, 0.1, 1.0, 10.0)
PROFUNDIDAD_GRID <- c(10L, 20L)
HOJAS_GRID <- c(1L, 5L)
N_ARBOLES <- 300L
N_ARBOLES_CV <- 100L
N_SUBMUESTRA_CV <- 30000L
NUM_HILOS <- 4L  # fijo para que la ejecución sea idéntica en otros equipos
# En la prueba rápida (PIA_PRUEBA_RAPIDA=1) se usan 2 pliegues y B = 5.
K_CV <- if (MODO_RAPIDO) 2L else 5L          # validación cruzada de C y predicciones fuera de pliegue
K_CV_BOSQUE <- if (MODO_RAPIDO) 2L else 3L
# B = 200; si el tiempo estimado supera 75 min, el equipo acordó B = 100 (PIA_B_BOOTSTRAP=100),
# y el informe declara el valor de B usado.
B_BOOTSTRAP <- if (MODO_RAPIDO) 5L else if (nzchar(Sys.getenv("PIA_B_BOOTSTRAP")))
  as.integer(Sys.getenv("PIA_B_BOOTSTRAP")) else 200L
B_PRUEBA_TIEMPO <- 5L                        # réplicas para estimar el tiempo del bootstrap
UMBRAL_CASI_CONSTANTE <- 0.999               # escenario de formato comparable (ver abajo)
# Tolerancias de glmnet. La comparación del objetivo penalizado (s6_comparacion_tipo_newton.csv)
# mostró que con thresh = 1e-7 el objetivo queda a < 1.3e-6 del óptimo, pero los coeficientes
# pueden diferir hasta ~0.9 (DE) porque, con separación cuasi completa y predictores
# redundantes, el objetivo es casi plano en algunas direcciones. Por eso los coeficientes que
# se reportan (ajuste final y bootstrap) usan 1e-10; la validación cruzada, las predicciones
# fuera de pliegue y los escenarios (que solo usan predicciones) usan 1e-7.
THRESH_CV <- 1e-7
THRESH_COEF <- 1e-10
# Rejilla de umbrales para elegir el umbral con predicciones fuera de pliegue (entrenamiento).
UMBRALES_FINOS <- sort(unique(round(c(seq(0.01, 0.99, by = 0.01), UMBRALES), 2)))
RAZONES_COSTO <- c(1, 5, 10, 50)            # costo de un FN en unidades de FP
LIMITE_FP_POR_10000 <- 1                    # restricción operativa alternativa
N_REPETICIONES_AZAR <- if (MODO_RAPIDO) 2L else 10L  # controles al azar en la influencia
BLOQUE_BOOTSTRAP <- 25L                     # punto de guardado del bootstrap
# Procesos en paralelo para el bootstrap: se deja al menos un núcleo libre y como máximo 4.
# PIA_PROCESOS permite fijar el número tras medir cuál da mejor tiempo total.
N_TRABAJADORES <- if (nzchar(Sys.getenv("PIA_PROCESOS"))) as.integer(Sys.getenv("PIA_PROCESOS")) else
  max(1L, min(parallel::detectCores() - 1L, 4L))

# ---------------------------------------------------------------------------------
# Ajuste de la logística L2 y comprobación de convergencia
# ---------------------------------------------------------------------------------
# glmnet recomienda una sucesión decreciente de lambdas con arranque en caliente. Todos los
# ajustes (validación cruzada, ajuste final, bootstrap, influencia y escenarios) usan el
# mismo camino: de 1000*lambda a lambda con 10 valores por década.
#
# Por qué 10 por década: con un camino de 4 valores (un salto por década) glmnet divergió
# en silencio en un pliegue de la validación cruzada con C = 10 (jerr = 0, sin avisos,
# pero devianza peor que la del modelo nulo: dev.ratio = -11.6, intercepto ~ -1e6). Subir
# maxit o bajar thresh no lo corrige (maxit no se alcanzaba); con 4 valores por década
# sigue divergiendo y con 10 por década converge. El paso de Newton de glmnet no tiene
# búsqueda lineal: desde un arranque lejano, con separación cuasi completa y predictores
# de colas largas (|z| de hasta ~345), el paso sobrepasa el óptimo. El camino automático
# de glmnet no sirve aquí: se detiene por devmax = 0.999 cientos de veces por encima del
# lambda pedido, y coef(fit, s = lambda) devuelve entonces el último lambda del camino.
PASOS_POR_DECADA <- 10L
LIMITE_COEF <- 100  # |intercepto| o |beta estandarizado| mayor que esto = divergencia
TIPO_NEWTON <- "Newton"  # valor por omisión de glmnet; "modified.Newton" se compara aparte

# glmnet no avisa de la divergencia, así que se comprueba sin depender de jerr ni de avisos.
# Un ajuste se marca como no convergido si ocurre cualquiera de estas condiciones:
#   - dev.ratio < 0 en el lambda pedido, es decir, devianza mayor que la del modelo nulo
#     (dev.ratio = 1 - devianza / devianza nula, así que ambas condiciones son la misma);
#   - |intercepto| > LIMITE_COEF o algún |beta estandarizado| > LIMITE_COEF;
#   - la fracción de devianza explicada baja a lo largo del camino (con lambdas
#     decrecientes debe subir) o hay valores no finitos;
#   - jerr distinto de 0 o camino incompleto (los dos indicadores propios de glmnet).
# Cada caso se registra en el log y en la tabla de no convergidos de la etapa.
.no_conv <- new.env()
.no_conv$filas <- list()
# Revisa un ajuste de glmnet y devuelve el motivo por el que se marca como no convergido
# (código de error, camino de lambdas incompleto, valores no finitos o devianza peor que la
# del modelo nulo). glmnet puede divergir sin avisar (jerr = 0), por eso no basta su código.
motivo_no_convergencia <- function(fit, a0, b, n_camino) {
  dr <- fit$dev.ratio; k <- length(dr)
  devianza <- (1 - dr[k]) * fit$nulldev
  m <- c(
    if (fit$jerr != 0) sprintf("jerr = %d", fit$jerr),
    if (length(fit$lambda) < n_camino) sprintf("camino incompleto (%d de %d lambdas)", length(fit$lambda), n_camino),
    if (!all(is.finite(c(a0, b, dr)))) "valores no finitos",
    if (is.finite(dr[k]) && (dr[k] < 0 || devianza > fit$nulldev))
      sprintf("dev.ratio = %.4f < 0 (devianza peor que la del modelo nulo)", dr[k]),
    if (is.finite(a0) && abs(a0) > LIMITE_COEF) sprintf("|intercepto| = %.4g > %g", abs(a0), LIMITE_COEF),
    if (all(is.finite(b)) && max(abs(b)) > LIMITE_COEF) sprintf("max |beta| = %.4g > %g", max(abs(b)), LIMITE_COEF),
    if (k > 1 && all(is.finite(dr)) && any(diff(dr) < -1e-6)) "la devianza explicada baja a lo largo del camino")
  if (length(m)) paste(m, collapse = "; ") else ""
}

# Versión sin efectos secundarios (se usa también en los procesos paralelos del bootstrap).
ajustar_l2_puro <- function(Z, y, C, thresh = THRESH_CV, tipo = TIPO_NEWTON) {
  lam <- lambda_desde_C(C, nrow(Z))
  camino <- lam * 10^seq(3, 0, length.out = 3L * PASOS_POR_DECADA + 1L)
  t0 <- proc.time()[["elapsed"]]
  fit <- glmnet(Z, y, family = "binomial", alpha = 0, lambda = camino, standardize = FALSE,
                intercept = TRUE, type.logistic = tipo, control = list(thresh = thresh, maxit = 1e6))
  k <- length(fit$lambda)
  a0 <- unname(fit$a0[k]); b <- as.numeric(fit$beta[, k])
  motivo <- motivo_no_convergencia(fit, a0, b, length(camino))
  list(a0 = a0, b = b, lambda = lam, C = C, converge = !nzchar(motivo), motivo = motivo,
       dev_ratio = fit$dev.ratio[k], jerr = fit$jerr, npasses = fit$npasses, n = nrow(Z),
       segundos = proc.time()[["elapsed"]] - t0)
}
# Si el ajuste no convergió, guarda sus datos en la tabla de no convergidos y deja un aviso en el log.
registrar_no_convergido <- function(m, contexto) {
  if (!m$converge) {
    .no_conv$filas[[length(.no_conv$filas) + 1L]] <- data.frame(
      contexto = contexto, C = m$C, lambda = m$lambda, n = m$n, dev_ratio = m$dev_ratio,
      intercepto = m$a0, max_abs_beta = if (all(is.finite(m$b))) max(abs(m$b)) else NA_real_,
      jerr = m$jerr, npasses = m$npasses, motivo = m$motivo)
    registro("   AVISO: ajuste no convergido (", contexto, "): ", m$motivo)
  }
  invisible(m)
}
# Ajusta la logística L2 y registra si no convergió (envoltorio de ajustar_l2_puro).
ajustar_l2 <- function(Z, y, C, thresh = THRESH_CV, contexto = "sin contexto", tipo = TIPO_NEWTON) {
  registrar_no_convergido(ajustar_l2_puro(Z, y, C, thresh, tipo), contexto)
}
# Devuelve la tabla acumulada de ajustes no convergidos (vacía si no hubo ninguno).
tabla_no_convergidos <- function() {
  if (!length(.no_conv$filas)) return(data.frame(contexto = character(0), C = numeric(0), lambda = numeric(0),
    n = integer(0), dev_ratio = numeric(0), intercepto = numeric(0), max_abs_beta = numeric(0),
    jerr = integer(0), npasses = integer(0), motivo = character(0)))
  do.call(rbind, .no_conv$filas)
}

# Probabilidad de phishing de la logística L2 para datos ya estandarizados.
predecir_l2 <- function(m, Z) as.numeric(plogis(m$a0 + Z %*% m$b))

# Pipeline completo: escala aprendida en entrenamiento + logística L2.
ajustar_pipeline_l2 <- function(Xa, ya, C, contexto = "sin contexto", thresh = THRESH_CV) {
  esc <- ajustar_escala(Xa)
  m <- ajustar_l2(aplicar_escala(Xa, esc), ya, C, thresh = thresh, contexto = contexto)
  m$escala <- esc
  m
}
# Probabilidad de phishing: estandariza con la escala del entrenamiento y aplica la logística.
predecir_pipeline_l2 <- function(m, Xb) predecir_l2(m, aplicar_escala(Xb, m$escala))

# Bosque aleatorio de probabilidades (ranger) con la profundidad y el tamaño de hoja (min.bucket)
# dados. min.node.size = 2 equivale al min_samples_split de sklearn. La semilla es fija.
ajustar_bosque <- function(Xa, ya, n_arboles, prof, hojas, importancia = "none") {
  ranger(x = as.data.frame(Xa), y = factor(ya, levels = c(0, 1)), num.trees = n_arboles,
         max.depth = prof, min.bucket = hojas, min.node.size = 2L, probability = TRUE,
         replace = TRUE, sample.fraction = 1, importance = importancia,
         num.threads = NUM_HILOS, seed = SEMILLA)
}
# Probabilidad de phishing (clase 1) según el bosque aleatorio.
predecir_bosque <- function(m, Xb) predict(m, data = as.data.frame(Xb),
                                           num.threads = NUM_HILOS)$predictions[, "1"]

# ---------------------------------------------------------------------------------
# Diagnóstico de separación
# ---------------------------------------------------------------------------------
# Dos evidencias complementarias:
# 1) Máxima verosimilitud sin penalización (glm, IRLS) con un número creciente de
#    iteraciones: si la log-verosimilitud sigue subiendo, los coeficientes crecen sin
#    límite y aparecen probabilidades ajustadas de 0 o 1, el estimador no existe.
# 2) glmnet al relajar la penalización (C creciente): el coeficiente máximo crece.
# Nota: con lambda casi cero glmnet no converge en tiempo razonable en estos datos (otro
# síntoma de la separación), por eso la rejilla de C se detiene en C = 10.
C_DIAG <- c(0.01, 0.1, 1, 10)
# Muestra la separación: ajusta la logística sin penalizar (más iteraciones) y con L2 (C creciente) y reporta coeficiente máximo y URL con probabilidad 0 o 1.
diagnostico_separacion <- function(X, y) {
  Z <- aplicar_escala(X, ajustar_escala(X))
  datos <- data.frame(y = y, Z)
  filas <- list()
  for (it in c(5L, 10L, 25L, 50L)) {
    avisos <- character(0)
    g <- withCallingHandlers(
      glm(y ~ ., data = datos, family = binomial(), control = glm.control(maxit = it, epsilon = 1e-14)),
      warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") })
    pf <- fitted(g)
    filas[[length(filas) + 1]] <- data.frame(
      metodo = "glm sin penalización (IRLS)", iteraciones_o_C = it,
      logverosimilitud = as.numeric(logLik(g)),
      max_abs_coef_por_DE = max(abs(coef(g)[-1]), na.rm = TRUE),
      n_p_ajustada_extrema = sum(pf < 1e-8 | pf > 1 - 1e-8),
      nota = sprintf("converged = %s; avisos: %s", g$converged,
                     if (length(avisos)) paste(unique(avisos), collapse = " | ") else "ninguno"))
    registro(sprintf("   glm con %d iteraciones listo", it))
  }
  for (C in C_DIAG) {
    m <- ajustar_l2(Z, y, C, contexto = sprintf("separación C=%g", C))
    eta <- m$a0 + as.numeric(Z %*% m$b)
    pf <- plogis(eta)
    filas[[length(filas) + 1]] <- data.frame(
      metodo = "glmnet L2 (alpha = 0)", iteraciones_o_C = C,
      logverosimilitud = sum(y * eta - log1p(exp(-abs(eta))) - pmax(eta, 0)),
      max_abs_coef_por_DE = max(abs(m$b)),
      n_p_ajustada_extrema = sum(pf < 1e-8 | pf > 1 - 1e-8),
      nota = sprintf("lambda = %.3e; convergencia: %s", m$lambda, if (m$converge) "sí" else m$motivo))
  }
  do.call(rbind, filas)
}

# ---------------------------------------------------------------------------------
# Validación cruzada (solo entrenamiento) y regla de selección de C
# ---------------------------------------------------------------------------------
# Regla de selección de C (definida por el equipo antes de repetir la validación cruzada
# con la rejilla fina, y fija: no se cambia después de ver resultados): el C más pequeño
# (mayor penalización) cuyo log-loss medio esté dentro de 1 error estándar del mínimo.
# Error estándar = desviación estándar muestral (divisor K - 1) entre pliegues / sqrt(K),
# la misma convención que cv.glmnet. Transparencia: los valores por pliegue del
# diagnóstico se vieron antes de escribir la regla; con la desviación poblacional
# (divisor K) la decisión puede cambiar, por eso se reporta también (solo como referencia).
elegir_C_1EE <- function(res, ee) {
  i <- which.min(res$logloss_media)
  umbral <- res$logloss_media[i] + ee[i]
  list(C = min(res$C[res$logloss_media <= umbral]), C_minimo = res$C[i], umbral = umbral)
}

# Tabla auditable de la regla: para cada C y cada definición de la desviación estándar,
# media, DE, EE = DE / sqrt(K), mínimo, umbral (mínimo + EE del C con el mínimo), si el C
# cumple (media <= umbral) y el margen (media - umbral; negativo = cumple).
tabla_regla_1se <- function(por_pliegue) {
  do.call(rbind, lapply(c("muestral (divisor K - 1) [regla usada]", "poblacional (divisor K) [referencia]"), function(def) {
    r <- do.call(rbind, lapply(split(por_pliegue$logloss, por_pliegue$C), function(x) {
      k <- length(x); de <- if (startsWith(def, "muestral")) sd(x) else sd(x) * sqrt((k - 1) / k)
      data.frame(n_pliegues = k, logloss_media = mean(x), desviacion = de, EE = de / sqrt(k))
    }))
    r <- cbind(definicion_desviacion = def, C = as.numeric(rownames(r)), r)
    i <- which.min(r$logloss_media)
    r$minimo <- r$logloss_media[i]; r$C_del_minimo <- r$C[i]
    r$umbral <- r$minimo + r$EE[i]
    r$cumple <- r$logloss_media <= r$umbral
    r$margen_media_menos_umbral <- r$logloss_media - r$umbral
    r$C_elegido <- min(r$C[r$cumple])
    r
  }))
}

# Validación cruzada estratificada de la logística L2 solo dentro del entrenamiento: para cada valor
# de C y cada pliegue calcula log-loss y AUC. Con esta tabla se aplica la regla del 1 error estándar.
cv_logistica <- function(X, y, k = K_CV) {
  f <- pliegues_estratificados(y, k)
  filas <- list()
  for (C in C_GRID) for (j in seq_len(k)) {
    a <- f != j; b <- f == j
    m <- ajustar_pipeline_l2(X[a, , drop = FALSE], y[a], C, contexto = sprintf("CV C=%g pliegue %d", C, j))
    p <- predecir_pipeline_l2(m, X[b, , drop = FALSE])
    fila <- data.frame(
      C = C, pliegue = j, n_ajuste = sum(a), n_validacion = sum(b),
      logloss = perdida_log(y[b], p), AUC = auc_roc(y[b], p), converge = m$converge,
      motivo = m$motivo, dev_ratio = m$dev_ratio, intercepto = m$a0, max_abs_beta = max(abs(m$b)),
      npasses = m$npasses, segundos = m$segundos)
    filas[[length(filas) + 1]] <- fila
    registro(sprintf("   C=%-5g pliegue %d: log-loss=%.6f AUC=%.6f converge=%s (%.1f s)",
                     C, j, fila$logloss, fila$AUC, m$converge, m$segundos))
  }
  por_pliegue <- do.call(rbind, filas)
  resumen <- do.call(rbind, lapply(split(por_pliegue, por_pliegue$C), function(d) data.frame(
    C = d$C[1], lambda_aprox = lambda_desde_C(d$C[1], round(mean(d$n_ajuste))), n_pliegues = nrow(d),
    n_no_convergidos = sum(!d$converge),
    logloss_media = mean(d$logloss), logloss_de_muestral = sd(d$logloss),
    logloss_EE = sd(d$logloss) / sqrt(nrow(d)),
    logloss_EE_con_de_poblacional = sd(d$logloss) * sqrt((nrow(d) - 1) / nrow(d)) / sqrt(nrow(d)),
    AUC_media = mean(d$AUC), AUC_de_muestral = sd(d$AUC),
    logloss_por_pliegue = paste(sprintf("%.6f", d$logloss), collapse = "; "))))
  rownames(resumen) <- NULL
  regla <- elegir_C_1EE(resumen, resumen$logloss_EE)
  ref_pob <- elegir_C_1EE(resumen, resumen$logloss_EE_con_de_poblacional)
  seleccion <- data.frame(
    regla = c("C más pequeño dentro de 1 EE del mínimo (EE = DE muestral / sqrt(K)) [regla usada]",
              "referencia: misma regla con DE poblacional / sqrt(K) [no se usa]",
              "referencia: mínimo log-loss medio (criterio de la versión en Python) [no se usa]"),
    C = c(regla$C, ref_pob$C, regla$C_minimo),
    umbral_logloss = c(regla$umbral, ref_pob$umbral, NA_real_))
  list(por_pliegue = por_pliegue, resumen = resumen, seleccion = seleccion, C_best = regla$C)
}

# Validación cruzada del bosque (rejilla de profundidad y tamaño de hoja) sobre una submuestra
# estratificada del entrenamiento, por limitación de cómputo.
cv_bosque <- function(X, y, k = K_CV_BOSQUE) {
  # Submuestra estratificada de 30,000 filas del entrenamiento (limitación de cómputo).
  set.seed(SEMILLA)
  n_sub <- min(as.numeric(N_SUBMUESTRA_CV), length(y))
  idx <- sort(unlist(lapply(split(seq_along(y), y), function(ix)
    ix[sample.int(length(ix), round(n_sub * length(ix) / length(y)))]), use.names = FALSE))
  Xs <- X[idx, , drop = FALSE]; ys <- y[idx]
  f <- pliegues_estratificados(ys, k)
  filas <- list()
  for (prof in PROFUNDIDAD_GRID) for (hojas in HOJAS_GRID) {
    res <- vapply(seq_len(k), function(j) {
      a <- f != j; b <- f == j
      m <- ajustar_bosque(Xs[a, ], ys[a], N_ARBOLES_CV, prof, hojas)
      p <- predecir_bosque(m, Xs[b, ])
      c(perdida_log(ys[b], p), auc_roc(ys[b], p))
    }, numeric(2))
    filas[[length(filas) + 1]] <- data.frame(
      max_depth = prof, min_bucket = hojas, n_submuestra = length(ys), n_pliegues = k,
      logloss_media = mean(res[1, ]), logloss_de_muestral = sd(res[1, ]),
      AUC_media = mean(res[2, ]), AUC_de_muestral = sd(res[2, ]),
      logloss_por_pliegue = paste(sprintf("%.6f", res[1, ]), collapse = "; "))
    registro(sprintf("   profundidad=%d hojas=%d: log-loss medio=%.6f", prof, hojas, mean(res[1, ])))
  }
  do.call(rbind, filas)
}

# ---------------------------------------------------------------------------------
# Razones de momios con IC bootstrap (percentil)
# ---------------------------------------------------------------------------------
# Coeficientes de la logística L2 (ajustada en todo el entrenamiento). OR_por_DE para
# continuas; OR_por_unidad (presencia para binarias) en unidades originales. Cada réplica
# se comprueba con el mismo criterio de convergencia; los intervalos se calculan con las
# réplicas que convergieron y se reporta cuántas no lo hicieron.
or_con_bootstrap <- function(X, y, cols, C, B = B_BOOTSTRAP, thresh = THRESH_COEF, ruta_parcial = NULL) {
  esc <- ajustar_escala(X)
  Z <- aplicar_escala(X, esc)
  m0 <- ajustar_l2(Z, y, C, thresh = thresh, contexto = "bootstrap: ajuste completo")
  beta0 <- m0$b
  # Una semilla por réplica, generada con la semilla del análisis: las réplicas son las
  # mismas con o sin procesos en paralelo y al reanudar desde un punto de guardado.
  set.seed(SEMILLA)
  semillas <- sample.int(.Machine$integer.max, B)
  clave <- list(B = B, C = C, n = length(y), thresh = thresh, semilla = SEMILLA, cols = cols)
  est <- list(boot = matrix(NA_real_, B, length(cols), dimnames = list(NULL, cols)), conv = rep(NA, B),
              seg = rep(NA_real_, B), motivo = rep(NA_character_, B), dev = rep(NA_real_, B),
              a0 = rep(NA_real_, B), clave = clave)
  if (!is.null(ruta_parcial) && file.exists(ruta_parcial)) {
    previo <- readRDS(ruta_parcial)
    if (identical(previo$clave, clave)) {
      est <- previo
      registro(sprintf("   reanudando desde el punto de guardado: %d de %d réplicas hechas", sum(!is.na(est$conv)), B))
    }
  }
  replica <- function(b) {
    set.seed(semillas[b])
    idx <- sample.int(length(y), length(y), replace = TRUE)
    m <- ajustar_l2_puro(Z[idx, , drop = FALSE], y[idx], C, thresh = thresh)
    m[c("b", "converge", "motivo", "segundos", "dev_ratio", "a0", "C", "lambda", "n", "jerr", "npasses")]
  }
  cl <- NULL
  if (N_TRABAJADORES > 1L && sum(is.na(est$conv)) > 1L) {
    cl <- parallel::makeCluster(N_TRABAJADORES)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(glmnet)))
    parallel::clusterExport(cl, c("ajustar_l2_puro", "motivo_no_convergencia", "lambda_desde_C",
                                  "PASOS_POR_DECADA", "LIMITE_COEF", "TIPO_NEWTON", "THRESH_CV"), envir = globalenv())
    # Los datos se envían una sola vez; la función de réplica se envía sin su entorno
    # (si no, cada envío arrastraría X y Z completos).
    parallel::clusterExport(cl, c("Z", "y", "C", "thresh", "semillas"), envir = environment())
    environment(replica) <- globalenv()
    registro(sprintf("   %d procesos en paralelo (PID: %s)", N_TRABAJADORES,
                     paste(unlist(parallel::clusterEvalQ(cl, Sys.getpid())), collapse = ", ")))
  }
  t_inicio <- proc.time()[["elapsed"]]; n_previas <- sum(!is.na(est$conv))
  for (ini in seq(1L, B, by = BLOQUE_BOOTSTRAP)) {
    ids <- ini:min(B, ini + BLOQUE_BOOTSTRAP - 1L)
    ids <- ids[is.na(est$conv[ids])]
    if (!length(ids)) next
    res <- if (is.null(cl)) lapply(ids, replica) else parallel::parLapply(cl, ids, replica)
    for (k in seq_along(ids)) {
      b <- ids[k]; r <- res[[k]]
      est$boot[b, ] <- r$b; est$conv[b] <- r$converge; est$seg[b] <- r$segundos
      est$motivo[b] <- r$motivo; est$dev[b] <- r$dev_ratio; est$a0[b] <- r$a0
      registrar_no_convergido(r, sprintf("bootstrap réplica %d", b))
    }
    if (!is.null(ruta_parcial)) saveRDS(est, ruta_parcial)
    hechas <- sum(!is.na(est$conv))
    registro(sprintf("   bootstrap %d/%d (no convergidas hasta ahora: %d; punto de guardado escrito)",
                     hechas, B, sum(!est$conv, na.rm = TRUE)))
  }
  boot <- est$boot; conv <- est$conv; seg <- est$seg
  n_nuevas <- sum(!is.na(conv)) - n_previas
  seg_reloj_por_replica <- if (n_nuevas > 0) (proc.time()[["elapsed"]] - t_inicio) / n_nuevas else NA_real_
  list(tabla = resumir_bootstrap(X, cols, beta0, esc$sd, boot, conv), boot = boot, converge = conv,
       segundos = seg, beta0 = beta0, motivo = est$motivo, ajuste_completo_converge = m0$converge,
       thresh = thresh, segundos_reloj_por_replica = seg_reloj_por_replica,
       trabajadores = if (is.null(cl)) 1L else N_TRABAJADORES, B_real = B, B_planeado = B)
}

# Tabla de razones de momios con IC percentil a partir de las réplicas convergidas.
resumir_bootstrap <- function(X, cols, beta0, sd, boot, conv) {
  bc <- boot[conv, , drop = FALSE]
  lo <- apply(bc, 2, quantile, 0.025, type = 7, names = FALSE)
  hi <- apply(bc, 2, quantile, 0.975, type = 7, names = FALSE)
  binaria <- apply(X, 2, function(z) all(z %in% c(0, 1)))
  tabla <- data.frame(
    variable = cols,
    tipo = ifelse(binaria, "binaria (presencia)", "continua"),
    beta_estandarizado = beta0,
    OR_por_DE = exp(beta0), IC95_inf_OR_por_DE = exp(lo), IC95_sup_OR_por_DE = exp(hi),
    OR_por_unidad = exp(beta0 / sd), IC95_inf_OR_por_unidad = exp(lo / sd),
    IC95_sup_OR_por_unidad = exp(hi / sd),
    IC_incluye_1 = lo <= 0 & hi >= 0
  )
  tabla <- tabla[order(-abs(beta0)), ]
  rownames(tabla) <- NULL
  tabla
}

# Cierre del bootstrap desde el punto de guardado (decisión del equipo, 2026-11-18): se
# detuvo la corrida de 200 en un punto de guardado (finalmente 75 réplicas) y se usan
# las réplicas hechas. Las semillas por réplica no cambian, así que las réplicas son las
# mismas que en la corrida completa. B real = número de réplicas hechas.
bootstrap_desde_parcial <- function(X, y, cols, C, ruta_parcial) {
  if (!file.exists(ruta_parcial)) stop("No existe el punto de guardado ", ruta_parcial)
  est <- readRDS(ruta_parcial)
  k <- est$clave
  if (!identical(k$cols, cols) || k$C != C || k$n != length(y) || k$semilla != SEMILLA)
    stop("El punto de guardado no corresponde a estos datos (columnas, C, n o semilla distintos)")
  hechas <- !is.na(est$conv)
  B_real <- sum(hechas)
  registro(sprintf("Bootstrap desde el punto de guardado: %d réplicas hechas de %d planeadas (thresh = %g)",
                   B_real, k$B, k$thresh))
  esc <- ajustar_escala(X)
  Z <- aplicar_escala(X, esc)
  m0 <- ajustar_l2(Z, y, C, thresh = k$thresh, contexto = "bootstrap: ajuste completo")
  boot <- est$boot[hechas, , drop = FALSE]; conv <- est$conv[hechas]; seg <- est$seg[hechas]
  for (b in which(hechas)[!conv]) registrar_no_convergido(
    list(converge = FALSE, motivo = est$motivo[b], C = C, lambda = lambda_desde_C(C, length(y)), n = length(y),
         dev_ratio = est$dev[b], a0 = est$a0[b], b = est$boot[b, ], jerr = NA, npasses = NA),
    sprintf("bootstrap réplica %d", b))
  list(tabla = resumir_bootstrap(X, cols, m0$b, esc$sd, boot, conv), boot = boot, converge = conv,
       segundos = seg, beta0 = m0$b, motivo = est$motivo[hechas], ajuste_completo_converge = m0$converge,
       thresh = k$thresh, segundos_reloj_por_replica = NA_real_, trabajadores = N_TRABAJADORES,
       B_real = B_real, B_planeado = k$B)
}

# ---------------------------------------------------------------------------------
# Escenarios de sensibilidad
# ---------------------------------------------------------------------------------
# 16 variables calculadas solo a partir del texto de la URL (no usan columnas del dataset).
variables_desde_url <- function(s) {
  host <- str_match(s, "^[a-zA-Z]+://([^/?#]+)")[, 2]
  host[is.na(host)] <- ""
  X <- cbind(
    longitud_url = nchar(s), digitos = str_count(s, "\\d"), letras = str_count(s, "[A-Za-z]"),
    barras = str_count(s, fixed("/")), interrogaciones = str_count(s, fixed("?")),
    igual = str_count(s, fixed("=")), ampersand = str_count(s, fixed("&")),
    guiones = str_count(s, fixed("-")), puntos = str_count(s, fixed(".")),
    porcentaje = str_count(s, fixed("%")), arroba = str_count(s, fixed("@")),
    puntos_host = str_count(host, fixed(".")), longitud_host = nchar(host),
    guiones_host = str_count(host, fixed("-")),
    https = as.integer(startsWith(s, "https://")), termina_barra = as.integer(endsWith(s, "/")))
  storage.mode(X) <- "double"
  X
}

# Las tres razones del archivo son conteo / URLLength redondeado a 3 decimales, y URLLength
# tiene el desfase ligado a la clase. Aquí se recalculan con nchar(URL) como denominador,
# con el mismo redondeo, sin cambiar nada más del modelo.
RAZONES_URL <- c(ObfuscationRatio = "NoOfObfuscatedChar", LetterRatioInURL = "NoOfLettersInURL",
                 DegitRatioInURL = "NoOfDegitsInURL")
# Escenario de sensibilidad: recalcula las razones ObfuscationRatio, LetterRatioInURL y
# DegitRatioInURL dividiendo entre nchar(URL), para quitar el desfase de URLLength.
recalcular_razones <- function(d) {
  L <- nchar(d$URL, type = "chars")
  for (r in names(RAZONES_URL)) d[[r]] <- round(d[[RAZONES_URL[[r]]]] / L, 3)
  d
}

# TRUE si la URL tiene el formato típico de las legítimas: contiene «www.», empieza con https://,
# no tiene ruta (a lo más tres barras) y no termina en «/».
formato_comparable <- function(u) {
  str_detect(u, fixed("www.")) & startsWith(u, "https://") &
    str_count(u, fixed("/")) <= 3 & !endsWith(u, "/")
}

# Escenario de formato comparable: antes de ajustar se quitan, con el entrenamiento del
# subconjunto, las columnas constantes y las casi constantes (más del 99.9 % de las filas
# con el mismo valor). Se eliminan en lugar de ajustarlas sin estandarizar porque sin
# estandarizar la penalización L2 recae sobre la escala original: una columna con DE de
# 0.004 recibiría una penalización ~ 1/0.004^2 veces mayor que en el modelo principal, lo
# que equivale a anularla pero de forma opaca. Además esas columnas solo distinguen a muy
# pocas filas (como máximo el 0.1 %) y, estandarizadas, producen |z| de hasta ~300, la
# condición en la que el paso de Newton de glmnet diverge. Los dos modelos del escenario
# usan las mismas columnas, igual que en el modelo principal.
datos_formato <- function(df, cols) {
  sub <- df[formato_comparable(df$URL), ]
  sp <- particion_estratificada(sub[[OBJETIVO]])
  Xa <- as.matrix(sub[sp$entrenamiento, cols]); ya <- sub[[OBJETIVO]][sp$entrenamiento]
  niveles <- apply(Xa, 2, function(z) length(unique(z)))
  prop_moda <- apply(Xa, 2, function(z) max(table(z)) / length(z))
  quitar <- niveles == 1 | prop_moda > UMBRAL_CASI_CONSTANTE
  eliminadas <- data.frame(variable = cols[quitar], niveles_en_entrenamiento = niveles[quitar],
                           proporcion_valor_modal = prop_moda[quitar],
                           filas_distintas_del_modal = round((1 - prop_moda[quitar]) * nrow(Xa)),
                           motivo = ifelse(niveles[quitar] == 1, "constante",
                                           "casi constante (> 99.9 % con el mismo valor)"))
  usadas <- cols[!quitar]
  list(Xa = Xa[, usadas, drop = FALSE], ya = ya,
       Xb = as.matrix(sub[sp$prueba, usadas]), yb = sub[[OBJETIVO]][sp$prueba],
       usadas = usadas, eliminadas = eliminadas, n_subconjunto = nrow(sub))
}

# Partición por dominio: ningún dominio aparece en entrenamiento y prueba a la vez.
# Los dominios se asignan al azar a 4 pliegues dentro de cada estrato (clase
# mayoritaria del dominio); el pliegue 1 es la prueba (~25 %).
particion_por_dominio <- function(dominio, y, k = 4L) {
  clase_dom <- tapply(y, dominio, function(z) as.integer(mean(z) >= 0.5))
  set.seed(SEMILLA)
  pliegue_dom <- integer(length(clase_dom)); names(pliegue_dom) <- names(clase_dom)
  for (cl in c(0L, 1L)) {
    d <- names(clase_dom)[clase_dom == cl]
    pliegue_dom[d] <- sample(rep_len(seq_len(k), length(d)))
  }
  f <- pliegue_dom[dominio]
  list(entrenamiento = which(f != 1L), prueba = which(f == 1L))
}

# Fila de la tabla de sensibilidad: AUC y matriz de confusión con umbral 0.50 de un modelo en un escenario.
fila_sens <- function(escenario, modelo, yb, p, n_pred, nota = "") {
  m <- metricas_en_umbral(yb, p, 0.5)
  data.frame(escenario = escenario, modelo = modelo, n_predictores = n_pred,
             n_prueba = length(yb), phishing_en_prueba = sum(yb), AUC = auc_roc(yb, p),
             VN = m$VN, FP = m$FP, FN = m$FN, VP = m$VP,
             sensibilidad_0_5 = m$sensibilidad, tasa_falsas_alarmas_0_5 = m$tasa_falsas_alarmas,
             nota = nota)
}

# ---------------------------------------------------------------------------------
# Figuras
# ---------------------------------------------------------------------------------
curva_roc <- function(y, p, modelo) {
  r <- pROC::roc(y, p, levels = c(0, 1), direction = "<", quiet = TRUE)
  data.frame(modelo = modelo, fpr = 1 - r$specificities, tpr = r$sensitivities,
             auc = as.numeric(r$auc))
}

# Genera las curvas ROC (completa y detalle) y la figura de errores por umbral de la partición de prueba.
figuras <- function(yte, p_lr, p_rf, tab) {
  colores <- c("Logística L2" = "#4C78A8", "Bosque aleatorio" = "#E45756")
  roc <- rbind(curva_roc(yte, p_lr, "Logística L2"), curva_roc(yte, p_rf, "Bosque aleatorio"))
  roc <- roc[order(roc$modelo, roc$fpr, roc$tpr), ]
  etiq <- unique(roc[, c("modelo", "auc")])
  roc$modelo_auc <- factor(sprintf("%s (AUC = %.5f)", roc$modelo, roc$auc),
                           levels = sprintf("%s (AUC = %.5f)", etiq$modelo, etiq$auc))
  col2 <- setNames(colores[as.character(etiq$modelo)], levels(roc$modelo_auc))
  tema <- theme_minimal(base_size = 10) + theme(plot.title = element_text(face = "bold", size = 10),
                                               legend.position = "bottom", legend.direction = "vertical")
  g1 <- ggplot(roc, aes(fpr, tpr, colour = modelo_auc)) + geom_step() +
    geom_abline(linetype = "dashed", colour = "grey50") +
    scale_colour_manual(values = col2, name = NULL) +
    labs(x = "Tasa de falsas alarmas (1 − especificidad)",
         y = "Sensibilidad (proporción de phishing detectado)",
         title = "Curvas ROC en la partición de prueba (25 %); diagonal = azar") + tema
  g2 <- g1 + coord_cartesian(xlim = c(0, 0.01), ylim = c(0.95, 1.0005)) +
    labs(x = "Tasa de falsas alarmas (zoom de 0 a 1 %)", y = "Sensibilidad (zoom de 95 a 100 %)",
         title = "Detalle de la esquina superior izquierda")
  ggsave(ruta_res("fig_s6_1a_curva_roc.png"), g1, width = 5.5, height = 5, dpi = 150)
  ggsave(ruta_res("fig_s6_1b_curva_roc_zoom.png"), g2, width = 5.5, height = 5, dpi = 150)

  larga <- rbind(data.frame(tab[, c("modelo", "umbral")], tipo = "Falsas alarmas (FP)", n = tab$FP),
                 data.frame(tab[, c("modelo", "umbral")], tipo = "Ataques omitidos (FN)", n = tab$FN))
  larga$modelo <- ifelse(larga$modelo == "logística L2", "Logística L2", "Bosque aleatorio")
  g3 <- ggplot(larga, aes(umbral, n, colour = modelo)) + geom_line() + geom_point() +
    facet_wrap(~tipo, scales = "free_y") + scale_colour_manual(values = colores, name = NULL) +
    scale_x_continuous(breaks = UMBRALES) +
    labs(x = "Umbral de decisión (probabilidad estimada de phishing)",
         y = "Número de URL en la prueba",
         title = "Errores por umbral en la partición de prueba") +
    theme_minimal(base_size = 10) + theme(legend.position = "top",
                                          plot.title = element_text(face = "bold", size = 10))
  ggsave(ruta_res("fig_s6_2_errores_por_umbral.png"), g3, width = 9, height = 4.2, dpi = 150)
}

source(file.path(if (dir.exists("codigo_R")) "codigo_R" else ".", "complementos_s6.R"), encoding = "UTF-8")

# ---------------------------------------------------------------------------------
# Etapas
# ---------------------------------------------------------------------------------
ruta_etapa <- function(nombre, ext = "rds") ruta_res(file.path("etapas", paste0(nombre, ".", ext)))
# Una etapa está terminada si existe su archivo en resultados_R/etapas/.
etapa_hecha <- function(nombre) file.exists(ruta_etapa(nombre))
# Guarda el resultado de una etapa en disco (RDS) con escritura atómica: se escribe a un archivo
# temporal y luego se renombra, para no dejar un archivo a medias si el proceso se interrumpe.
guardar_etapa <- function(nombre, obj) {
  dir.create(ruta_res("etapas"), showWarnings = FALSE, recursive = TRUE)
  obj$no_convergidos <- tabla_no_convergidos()
  tmp <- paste0(ruta_etapa(nombre), ".tmp")
  saveRDS(obj, tmp)
  if (file.exists(ruta_etapa(nombre))) file.remove(ruta_etapa(nombre))
  file.rename(tmp, ruta_etapa(nombre))  # escritura atómica: no queda un .rds a medias
}
# Lee el resultado de una etapa previa; si no existe, detiene la ejecución con un mensaje claro.
leer_etapa <- function(nombre) {
  r <- ruta_etapa(nombre)
  if (!file.exists(r)) stop("Falta el resultado de la etapa '", nombre, "'. Ejecútala antes.")
  readRDS(r)
}

# Datos comunes: carga, partición 75/25 y predictores elegidos en la etapa 'preparar'.
datos_modelo <- function() {
  prep <- leer_etapa("preparar")
  df <- cargar_datos(deduplicar = TRUE)
  dtr <- df[prep$idx_entrenamiento, ]; dte <- df[prep$idx_prueba, ]
  list(df = df, dtr = dtr, dte = dte, ytr = dtr[[OBJETIVO]], yte = dte[[OBJETIVO]], cols = prep$cols,
       Xtr = as.matrix(dtr[, prep$cols]), Xte = as.matrix(dte[, prep$cols]))
}
# Hiperparámetros elegidos por validación cruzada: C de la logística y profundidad y hojas del bosque.
hiperparametros <- function() {
  rf <- leer_etapa("cv_bosque")
  list(C = leer_etapa("cv_logistica")$C_best, prof = rf$prof_best, hojas = rf$hojas_best)
}

# Etapa 1: carga la muestra deduplicada, hace la partición 75/25 estratificada y aplica el filtro VIF
# solo con el entrenamiento para fijar los predictores del modelo.
etapa_preparar <- function() {
  df <- cargar_datos(deduplicar = TRUE)
  y <- df[[OBJETIVO]]
  part <- particion_estratificada(y)
  dtr <- df[part$entrenamiento, ]; dte <- df[part$prueba, ]
  registro(sprintf("Partición: entrenamiento=%d | prueba=%d | prevalencia entrenamiento=%.4f | prueba=%.4f",
                   nrow(dtr), nrow(dte), mean(dtr[[OBJETIVO]]), mean(dte[[OBJETIVO]])))
  vif <- seleccionar_por_vif(dtr, CANDIDATAS, umbral = 10)
  cols <- vif$cols
  registro(sprintf("Filtro VIF (solo entrenamiento): eliminadas %s (VIF = %s) | predictores finales=%d",
                   paste(vif$eliminadas$variable_eliminada, collapse = ", "),
                   paste(round(vif$eliminadas$VIF_en_entrenamiento, 2), collapse = ", "), length(cols)))
  guardar_csv(vif$eliminadas, "s6_seleccion_predictores.csv")
  guardar_csv(data.frame(variable = names(vif$vif), VIF_final = unname(vif$vif)), "s6_vif_final.csv")
  # Comprobación cruzada con car::vif sobre un modelo lineal auxiliar (no usa la respuesta real).
  set.seed(SEMILLA)
  aux <- data.frame(.r = rnorm(nrow(dtr)), dtr[, cols])
  vif_car <- car::vif(lm(.r ~ ., data = aux))
  dif <- max(abs(vif_car - vif$vif[names(vif_car)]))
  registro(sprintf("   máx |VIF propio - car::vif| = %.2e", dif))
  list(cols = cols, idx_entrenamiento = part$entrenamiento, idx_prueba = part$prueba,
       vif_eliminadas = vif$eliminadas, vif_final = vif$vif, dif_car_vif = dif)
}

# Etapa: diagnóstico de separación (logística sin penalizar y con penalización creciente).
etapa_separacion <- function() {
  d <- datos_modelo()
  sep <- diagnostico_separacion(d$Xtr, d$ytr)
  guardar_csv(sep, "s6_separabilidad.csv")
  registro("Diagnóstico de separabilidad:"); registro_tabla(sep)
  list(separacion = sep)
}

# Etapa: validación cruzada de la logística L2 y elección de C con la regla del 1 error estándar.
etapa_cv_logistica <- function() {
  d <- datos_modelo()
  cv <- cv_logistica(d$Xtr, d$ytr)
  guardar_csv(cv$por_pliegue, "s6_cv_logistica_por_pliegue.csv")
  guardar_csv(cv$resumen, "s6_cv_logistica.csv")
  guardar_csv(cv$seleccion, "s6_cv_logistica_seleccion.csv")
  guardar_csv(tabla_regla_1se(cv$por_pliegue), "s6_regla_1se.csv")
  registro("Validación cruzada de la logística L2 (por pliegue):")
  registro_tabla(cv$por_pliegue[, c("C", "pliegue", "logloss", "AUC", "converge", "dev_ratio",
                                    "intercepto", "max_abs_beta", "segundos")], digits = 6)
  registro("Resumen:"); registro_tabla(cv$resumen[, setdiff(names(cv$resumen), "logloss_por_pliegue")], digits = 6)
  registro("Selección:"); registro_tabla(cv$seleccion, digits = 6)
  registro(sprintf("C seleccionado (regla de 1 EE) = %g (lambda = %.3e)", cv$C_best,
                   lambda_desde_C(cv$C_best, nrow(d$Xtr))))
  cv
}

# Etapa: validación cruzada del bosque aleatorio y elección de profundidad y tamaño de hoja.
etapa_cv_bosque <- function() {
  d <- datos_modelo()
  cv_rf <- cv_bosque(d$Xtr, d$ytr)
  guardar_csv(cv_rf, "s6_cv_bosque.csv")
  mejor <- cv_rf[which.min(cv_rf$logloss_media), ]
  registro(sprintf("CV bosque (%d pliegues, submuestra de %d): profundidad=%d, hojas=%d",
                   K_CV_BOSQUE, mejor$n_submuestra, mejor$max_depth, mejor$min_bucket))
  registro_tabla(cv_rf, digits = 6)
  list(cv = cv_rf, prof_best = mejor$max_depth, hojas_best = mejor$min_bucket)
}

# Etapa: ajusta los modelos finales con los hiperparámetros elegidos, predice en la partición de prueba
# y calcula las métricas por umbral. Aquí se evalúa la prueba; ninguna decisión se toma con ella.
etapa_ajuste_final <- function() {
  d <- datos_modelo(); hp <- hiperparametros()
  sel <- leer_etapa("umbrales_oof")$seleccion
  lr <- ajustar_pipeline_l2(d$Xtr, d$ytr, hp$C, contexto = "ajuste final", thresh = THRESH_COEF)
  registro(sprintf("Logística final: C=%g converge=%s dev.ratio=%.6f max|beta|=%.3f (%.1f s)",
                   hp$C, lr$converge, lr$dev_ratio, max(abs(lr$b)), lr$segundos))
  rf <- ajustar_bosque(d$Xtr, d$ytr, N_ARBOLES, hp$prof, hp$hojas, importancia = "impurity")
  p_lr <- predecir_pipeline_l2(lr, d$Xte)
  p_rf <- predecir_bosque(rf, d$Xte)
  # La prueba se evalúa una sola vez con estas predicciones: en los cinco umbrales fijos y en
  # los umbrales elegidos antes con el entrenamiento (ninguna decisión se toma aquí).
  filas_umbral <- list(); resumen <- list(); filas_elegidos <- list()
  for (nm in c("logística L2", "bosque aleatorio")) {
    p <- if (nm == "logística L2") p_lr else p_rf
    for (u in UMBRALES) filas_umbral[[length(filas_umbral) + 1]] <- cbind(modelo = nm, metricas_en_umbral(d$yte, p, u))
    s_m <- sel[sel$modelo == nm & !is.na(sel$umbral), ]
    for (i in seq_len(nrow(s_m))) filas_elegidos[[length(filas_elegidos) + 1]] <-
      cbind(modelo = nm, criterio_entrenamiento = s_m$criterio[i], metricas_en_umbral(d$yte, p, s_m$umbral[i]))
    r <- pROC::roc(d$yte, p, levels = c(0, 1), direction = "<", quiet = TRUE)
    ci <- as.numeric(pROC::ci.auc(r, method = "delong"))
    resumen[[length(resumen) + 1]] <- data.frame(
      modelo = nm, AUC_ROC = auc_roc(d$yte, p), AUC_IC95_inf_DeLong = ci[1], AUC_IC95_sup_DeLong = ci[3],
      AP_precision_recall = precision_promedio(d$yte, p), log_loss = perdida_log(d$yte, p),
      Brier = brier(d$yte, p))
  }
  # Regla de dos condiciones: puntaje binario (1 = phishing), mismas métricas de resumen.
  p_regla <- as.numeric(d$dte$IsHTTPS == 0 | d$dte$URLSimilarityIndex < 100)
  resumen[[length(resumen) + 1]] <- data.frame(
    modelo = "regla de dos condiciones", AUC_ROC = auc_roc(d$yte, p_regla), AUC_IC95_inf_DeLong = NA_real_,
    AUC_IC95_sup_DeLong = NA_real_, AP_precision_recall = precision_promedio(d$yte, p_regla),
    log_loss = perdida_log(d$yte, p_regla), Brier = brier(d$yte, p_regla))
  tab_umbral <- do.call(rbind, filas_umbral)
  resumen <- do.call(rbind, resumen)
  elegidos <- do.call(rbind, filas_elegidos)
  guardar_csv(elegidos, "s6_prueba_en_umbral_elegido.csv")
  guardar_csv(tab_umbral, "s6_umbrales_prueba.csv")
  guardar_csv(resumen, "s6_resumen_prueba.csv")
  # a) Matriz de confusión completa por umbral y modelo, con la regla de dos condiciones
  #    como línea base (clasificador fijo, sin umbral).
  matriz <- rbind(tab_umbral, metricas_regla(d$dte)[, names(tab_umbral)])
  guardar_csv(matriz, "s6_matriz_confusion_prueba.csv")
  registro("Métricas en prueba (una sola evaluación):"); registro_tabla(resumen, digits = 6)
  registro("Prueba en los umbrales elegidos con el entrenamiento:")
  registro_tabla(elegidos[, c("modelo", "criterio_entrenamiento", "umbral", "VN", "FP", "FN", "VP", "F1")], digits = 5)
  registro_tabla(matriz[, c("modelo", "umbral", "VN", "FP", "FN", "VP", "sensibilidad",
                            "especificidad", "precision", "F1")], digits = 5)
  imp <- data.frame(variable = names(rf$variable.importance), importancia_gini = unname(rf$variable.importance))
  imp <- imp[order(-imp$importancia_gini), ]
  guardar_csv(imp, "s6_bosque_importancia.csv")
  figuras(d$yte, p_lr, p_rf, tab_umbral)
  principal <- rbind(fila_sens("principal", "logística L2", d$yte, p_lr, length(d$cols)),
                     fila_sens("principal", "bosque aleatorio", d$yte, p_rf, length(d$cols)))
  list(lr = lr, C = hp$C, prof = hp$prof, hojas = hp$hojas, p_lr = p_lr, p_rf = p_rf, yte = d$yte,
       tab_umbral = tab_umbral, resumen = resumen, matriz = matriz, importancia = imp, elegidos = elegidos,
       sens_principal = principal)
}

# Umbral elegido solo con predicciones fuera de pliegue del entrenamiento, en la rejilla
# UMBRALES_FINOS (0.01 a 0.99 cada 0.01):
#   - mínimo costo esperado k*FN + FP por cada 10,000 URL, para k = 1, 5, 10 y 50 (si hay
#     empate se toma el umbral más bajo, que detecta más phishing);
#   - restricción operativa: el umbral más bajo con a lo sumo 1 falsa alarma por cada
#     10,000 legítimas (la mayor sensibilidad que respeta la restricción).
# Con tan pocos errores la curva de costo es casi plana, así que además del umbral se
# reporta el rango de umbrales con costo mínimo (empate exacto) y con costo casi mínimo:
# costo total (en conteos, k*FN + FP) a lo sumo el mínimo más k, es decir, como mucho un
# ataque omitido adicional, el error individual más caro.
rango_txt <- function(u) if (!length(u)) NA_character_ else if (length(u) == 1) sprintf("%.2f", u) else
  sprintf("%.2f a %.2f (%d de %d umbrales de la rejilla%s)", min(u), max(u), length(u), length(UMBRALES_FINOS),
          if (length(u) == round((max(u) - min(u)) / 0.01) + 1) "" else "; no contiguos")
# Elige el umbral usando solo predicciones del entrenamiento: el de menor costo k*FN + FP, el rango
# de umbrales de costo mínimo o casi mínimo y el que cumple el límite de falsas alarmas.
seleccion_umbrales <- function(costo, tab_fina) {
  a <- do.call(rbind, lapply(split(costo, list(costo$modelo, costo$razon_costo_FN_FP), drop = TRUE), function(d) {
    k <- d$razon_costo_FN_FP[1]
    total <- k * d$FN + d$FP
    rango_min <- d$umbral[total == min(total)]
    rango_casi <- d$umbral[total <= min(total) + k]
    d <- d[order(d$costo_por_10000, d$umbral), ][1, ]
    data.frame(modelo = d$modelo, criterio = sprintf("mínimo costo k*FN + FP, k = %g", d$razon_costo_FN_FP),
               k = d$razon_costo_FN_FP, umbral = d$umbral, costo_por_10000 = d$costo_por_10000,
               FP = d$FP, FN = d$FN, FP_por_10000_legitimas = d$FP_por_10000_legitimas,
               rango_costo_minimo = rango_txt(rango_min),
               rango_costo_casi_minimo = rango_txt(rango_casi),
               casi_minimo_inf = min(rango_casi), casi_minimo_sup = max(rango_casi))
  }))
  b <- do.call(rbind, lapply(split(tab_fina, tab_fina$modelo), function(d) {
    ok <- d[d$FP_por_10000_legitimas <= LIMITE_FP_POR_10000, ]
    d <- if (nrow(ok)) ok[which.min(ok$umbral), ] else d[NA_integer_, ]
    data.frame(modelo = d$modelo, criterio = sprintf("a lo sumo %g FP por 10,000 legítimas (umbral más bajo)", LIMITE_FP_POR_10000),
               k = NA_real_, umbral = d$umbral, costo_por_10000 = NA_real_, FP = d$FP, FN = d$FN,
               FP_por_10000_legitimas = d$FP_por_10000_legitimas,
               rango_costo_minimo = NA_character_,
               rango_costo_casi_minimo = rango_txt(ok$umbral),
               casi_minimo_inf = if (nrow(ok)) min(ok$umbral) else NA_real_,
               casi_minimo_sup = if (nrow(ok)) max(ok$umbral) else NA_real_)
  }))
  out <- rbind(a, b)
  out <- out[order(out$modelo, is.na(out$k), out$k), ]
  rownames(out) <- NULL
  out
}

# Etapa: predicciones fuera de pliegue (OOF) en el entrenamiento y tabla de costo esperado para
# varias razones FN:FP; de ahí salen los umbrales sin usar la partición de prueba.
etapa_umbrales_oof <- function() {
  d <- datos_modelo(); hp <- hiperparametros()
  oof <- predicciones_oof(d$Xtr, d$ytr, hp$C, hp$prof, hp$hojas)
  preds <- list("logística L2" = oof$lr, "bosque aleatorio" = oof$rf)
  tab_oof <- tabla_umbrales(d$ytr, preds)
  tab_oof <- rbind(tab_oof, metricas_regla(d$dtr)[, names(tab_oof)])
  guardar_csv(tab_oof, "s6_umbrales_oof_entrenamiento.csv")
  tab_fina <- tabla_umbrales(d$ytr, preds, umbrales = UMBRALES_FINOS)
  costo <- costo_por_umbral(tab_fina, razones = RAZONES_COSTO)
  guardar_csv(costo, "s6_costo_por_umbral_oof.csv")
  sel <- seleccion_umbrales(costo, tab_fina)
  guardar_csv(sel, "s6_umbral_elegido_entrenamiento.csv")
  registro("Umbrales fuera de pliegue (entrenamiento):")
  registro_tabla(tab_oof[, c("modelo", "umbral", "VN", "FP", "FN", "VP", "F1")], digits = 5)
  registro("Umbrales elegidos con el entrenamiento (fuera de pliegue):"); registro_tabla(sel, digits = 5)
  list(oof = oof, tab_oof = tab_oof, costo = costo, seleccion = sel)
}

# Etapa: bootstrap de los coeficientes de la logística L2 para obtener razones de momios con IC.
# Guarda puntos de control cada 25 réplicas y registra el B real y las réplicas no convergidas.
etapa_bootstrap <- function(B = B_BOOTSTRAP, escribir = TRUE) {
  d <- datos_modelo(); C <- leer_etapa("cv_logistica")$C_best
  registro(sprintf("Bootstrap de coeficientes: B = %d, C = %g", B, C))
  orb <- if (escribir && identical(Sys.getenv("PIA_BOOTSTRAP_DESDE_PARCIAL"), "1"))
    bootstrap_desde_parcial(d$Xtr, d$ytr, d$cols, C, ruta_etapa("bootstrap_parcial")) else
    or_con_bootstrap(d$Xtr, d$ytr, d$cols, C, B,
                     ruta_parcial = if (escribir) ruta_etapa("bootstrap_parcial") else NULL)
  B <- orb$B_real  # B real (puede ser menor que el planeado si se cerró desde el punto de guardado)
  n_nc <- sum(!orb$converge)
  orb$estimacion_min_200 <- B_BOOTSTRAP * mean(orb$segundos) / orb$trabajadores / 60
  registro(sprintf(paste("Réplicas no convergidas: %d de %d | segundos por ajuste: %.1f | procesos: %d |",
                         "reloj por réplica: %.1f s | estimación para %d réplicas: %.1f min"),
                   n_nc, B, mean(orb$segundos), orb$trabajadores, orb$segundos_reloj_por_replica,
                   B_BOOTSTRAP, orb$estimacion_min_200))
  if (!escribir) return(orb)
  guardar_csv(orb$tabla, "s6_logistica_coef_crudos.csv")  # trazabilidad (anexo técnico)
  # b) Grupos redundantes (solo entrenamiento) y razones de momios marcadas.
  gr <- grupos_redundantes(d$dtr, d$cols)
  guardar_csv(gr$aristas, "s6_redundancia_aristas.csv")
  bc <- orb$boot[orb$converge, , drop = FALSE]
  dir_g <- direccion_grupos(gr$grupos, gr$R, d$cols, setNames(orb$beta0, d$cols), bc)
  guardar_csv(dir_g, "s6_or_grupos_redundantes.csv")
  or_marcada <- marcar_or(orb$tabla, gr$grupos)
  guardar_csv(or_marcada, "s6_logistica_or.csv")
  guardar_csv(data.frame(B = B, B_planeado = orb$B_planeado, C = C, thresh = orb$thresh, procesos_paralelos = orb$trabajadores,
                         replicas_convergidas = sum(orb$converge), replicas_no_convergidas = n_nc,
                         ajuste_completo_converge = orb$ajuste_completo_converge,
                         segundos_medios_por_ajuste = mean(orb$segundos)), "s6_bootstrap_convergencia.csv")
  registro("Grupos redundantes (dirección conjunta):")
  registro_tabla(dir_g[, c("grupo", "variables", "suma_beta_estandarizado", "IC95_inf_suma",
                           "IC95_sup_suma", "direccion")], digits = 3)
  registro("Razones de momios interpretables (primeras 12 por |beta|):")
  registro_tabla(head(or_marcada[is.na(or_marcada$grupo_redundante), c("variable", "tipo", "OR_por_DE",
    "IC95_inf_OR_por_DE", "IC95_sup_OR_por_DE", "direccion")], 12), digits = 3)
  c(orb, list(grupos = gr$grupos, aristas = gr$aristas, direccion_grupos = dir_g, or_marcada = or_marcada))
}

# Etapa: análisis de influencia (reajuste sin las observaciones marcadas, controles al azar y
# aproximación de un paso).
etapa_influencia <- function() {
  d <- datos_modelo(); af <- leer_etapa("ajuste_final")
  boot <- if (etapa_hecha("bootstrap")) { bs <- leer_etapa("bootstrap"); bs$boot[bs$converge, , drop = FALSE] } else NULL
  infl <- analisis_influencia(d$Xtr, d$ytr, d$Xte, af$lr, boot, repeticiones = N_REPETICIONES_AZAR)
  guardar_csv(infl$rango_azar, "s6_influencia_control_rango.csv")
  registro(sprintf("Control al azar (%d repeticiones), rango:", N_REPETICIONES_AZAR))
  registro_tabla(infl$rango_azar, digits = 3)
  comp <- infl$comparacion
  guardar_csv(comp[, setdiff(names(comp), "delta_beta")], "s6_influencia_reajuste.csv")
  guardar_csv(infl$validacion_un_paso, "s6_influencia_validacion_un_paso.csv")
  guardar_csv(infl$resumen_sombrero, "s6_influencia_sombrero.csv")
  guardar_csv(cbind(infl$detalle, URL = d$dtr$URL[infl$detalle$fila_entrenamiento]), "s6_influencia_detalle.csv")
  registro(sprintf("Influencia: residuos de Pearson con |r| > 3 = %d", length(infl$marcadas)))
  registro_tabla(comp[, setdiff(names(comp), c("delta_beta", "cambios_clasificacion_prueba"))], digits = 3)
  registro("Validación de la aproximación de un paso (correlación con reajustes reales):")
  registro_tabla(infl$validacion_un_paso, digits = 3)
  list(comparacion = comp, validacion_un_paso = infl$validacion_un_paso, rango_azar = infl$rango_azar,
       resumen_sombrero = infl$resumen_sombrero, detalle = infl$detalle,
       h = infl$sombrero$h, cook = infl$sombrero$cook, w = infl$sombrero$w, marcadas = infl$marcadas)
}

# Ajusta los modelos de un escenario con los hiperparámetros ya elegidos (sin nueva CV).
escenario <- function(nombre, Xa, ya, Xb, yb, modelos = c("lr", "rf"), nota = "") {
  hp <- hiperparametros()
  registro(sprintf("Escenario: %s (%d predictores; entrenamiento = %d; prueba = %d, phishing = %d)",
                   nombre, ncol(Xa), length(ya), length(yb), sum(yb)))
  filas <- list(); conv <- NA
  if ("lr" %in% modelos) {
    m <- ajustar_pipeline_l2(Xa, ya, hp$C, contexto = paste("escenario:", nombre))
    conv <- m$converge
    filas$lr <- fila_sens(nombre, "logística L2", yb, predecir_pipeline_l2(m, Xb), ncol(Xa),
                          nota = if (m$converge) nota else paste("NO CONVERGIÓ:", m$motivo))
    registro(sprintf("   logística: AUC=%.5f FP/FN a 0.5 = %d/%d converge=%s (%.1f s)",
                     filas$lr$AUC, filas$lr$FP, filas$lr$FN, m$converge, m$segundos))
  }
  if ("rf" %in% modelos) {
    rf <- ajustar_bosque(Xa, ya, N_ARBOLES, hp$prof, hp$hojas)
    filas$rf <- fila_sens(nombre, "bosque aleatorio", yb, predecir_bosque(rf, Xb), ncol(Xa), nota = nota)
    registro(sprintf("   bosque: AUC=%.5f FP/FN a 0.5 = %d/%d", filas$rf$AUC, filas$rf$FP, filas$rf$FN))
  }
  list(filas = do.call(rbind, unname(filas)), converge_logistica = conv)
}

# Fábrica de etapas de sensibilidad que reajustan ambos modelos con un subconjunto de columnas.
etapa_sens_columnas <- function(nombre, cols_fun) function() {
  d <- datos_modelo(); cs <- cols_fun(d$cols)
  escenario(nombre, as.matrix(d$dtr[, cs]), d$ytr, as.matrix(d$dte[, cs]), d$yte)
}
# Etapa de sensibilidad: solo variables recalculadas desde el texto de la URL.
etapa_sens_url_recalculadas <- function() {
  d <- datos_modelo()
  escenario("solo variables recalculadas desde la URL", variables_desde_url(d$dtr$URL), d$ytr,
            variables_desde_url(d$dte$URL), d$yte)
}
# Etapa de sensibilidad: razones recalculadas con nchar(URL) en lugar de URLLength.
etapa_sens_razones_nchar <- function() {
  d <- datos_modelo()
  rtr <- recalcular_razones(d$dtr); rte <- recalcular_razones(d$dte)
  cambio <- data.frame(variable = names(RAZONES_URL),
                       prop_filas_que_cambian_entrenamiento = vapply(names(RAZONES_URL), function(v)
                         mean(rtr[[v]] != d$dtr[[v]]), numeric(1)),
                       max_abs_cambio = vapply(names(RAZONES_URL), function(v)
                         max(abs(rtr[[v]] - d$dtr[[v]])), numeric(1)))
  registro("Cambio en las razones al usar nchar(URL) como denominador:"); registro_tabla(cambio, digits = 4)
  e <- escenario("razones recalculadas con nchar(URL)", as.matrix(rtr[, d$cols]), d$ytr,
                 as.matrix(rte[, d$cols]), d$yte,
                 nota = "ObfuscationRatio, LetterRatioInURL y DegitRatioInURL = conteo / nchar(URL), 3 decimales")
  e$cambio_razones <- cambio
  e
}
# Etapa de sensibilidad: partición por dominio, sin dominios compartidos entre entrenamiento y prueba.
etapa_sens_dominio <- function() {
  d <- datos_modelo(); y <- d$df[[OBJETIVO]]
  pd <- particion_por_dominio(d$df$Domain, y)
  compartidos <- length(intersect(d$df$Domain[pd$entrenamiento], d$df$Domain[pd$prueba]))
  registro(sprintf("Partición por dominio: dominios compartidos = %d", compartidos))
  e <- escenario("partición por dominio (sin dominios compartidos)",
                 as.matrix(d$df[pd$entrenamiento, d$cols]), y[pd$entrenamiento],
                 as.matrix(d$df[pd$prueba, d$cols]), y[pd$prueba])
  e$dominios_compartidos <- compartidos
  e
}
NOMBRE_FORMATO <- "subconjunto con formato comparable a legítimas"
# Fábrica de la etapa de sensibilidad del subconjunto de formato comparable (un modelo por etapa).
etapa_sens_formato <- function(modelo) function() {
  d <- datos_modelo()
  f <- datos_formato(d$df, d$cols)
  registro(sprintf("Subconjunto de formato comparable: %d filas; entrenamiento = %d (phishing = %d)",
                   f$n_subconjunto, length(f$ya), sum(f$ya)))
  registro("Columnas eliminadas en este escenario (calculado en su entrenamiento):")
  registro_tabla(f$eliminadas, digits = 6)
  guardar_csv(f$eliminadas, "s6_formato_columnas_eliminadas.csv")
  e <- escenario(NOMBRE_FORMATO, f$Xa, f$ya, f$Xb, f$yb, modelos = modelo,
                 nota = sprintf("sin %d columnas constantes o casi constantes", nrow(f$eliminadas)))
  e$eliminadas <- f$eliminadas
  e
}

ESCENARIOS <- c("sens_sin_similitud", "sens_lexicas", "sens_html", "sens_url_recalculadas",
                "sens_dominio", "sens_razones_nchar", "sens_sin_tld", "sens_formato_bosque",
                "sens_formato_logistica")

# Etapa final: reúne los resultados de las etapas terminadas en las tablas finales de resultados_R/
# (sensibilidad, ajustes no convergidos y lo que depende del bootstrap).
etapa_consolidar <- function() {
  af <- leer_etapa("ajuste_final"); prep <- leer_etapa("preparar")
  filas <- list(cbind(af$sens_principal, ajustes_no_convergidos_escenario = nrow(af$no_convergidos)))
  pendientes <- character(0)
  for (e in ESCENARIOS) {
    if (etapa_hecha(e)) {
      r <- leer_etapa(e)
      filas[[length(filas) + 1]] <- cbind(r$filas, ajustes_no_convergidos_escenario = nrow(r$no_convergidos))
      next
    }
    pendientes <- c(pendientes, e)
    if (e == "sens_formato_logistica")
      filas[[length(filas) + 1]] <- data.frame(
        escenario = NOMBRE_FORMATO, modelo = "logística L2", n_predictores = NA, n_prueba = NA,
        phishing_en_prueba = NA, AUC = NA, VN = NA, FP = NA, FN = NA, VP = NA, sensibilidad_0_5 = NA,
        tasa_falsas_alarmas_0_5 = NA,
        nota = "sin resultado: el ajuste no terminó dentro del límite de tiempo (limitación no resuelta)",
        ajustes_no_convergidos_escenario = NA)
  }
  sens <- do.call(rbind, filas)
  # Un ajuste logístico no convergido no aporta métricas: se dejan en NA y queda la nota.
  nc_fila <- grepl("^NO CONVERGIÓ", sens$nota)
  sens[nc_fila, c("AUC", "VN", "FP", "FN", "VP", "sensibilidad_0_5", "tasa_falsas_alarmas_0_5")] <- NA
  guardar_csv(sens, "s6_sensibilidad.csv")
  registro("Escenarios de sensibilidad:")
  registro_tabla(sens[, c("escenario", "modelo", "n_predictores", "AUC", "FP", "FN", "ajustes_no_convergidos_escenario")], digits = 6)
  if (length(pendientes)) registro("Escenarios sin resultado: ", paste(pendientes, collapse = ", "))
  # Tabla global de ajustes no convergidos (todas las etapas).
  nc <- do.call(rbind, lapply(setdiff(ETAPAS_ORDEN, "consolidar"), function(e) {
    if (!etapa_hecha(e)) return(NULL)
    t <- leer_etapa(e)$no_convergidos
    if (is.null(t) || !nrow(t)) return(NULL)
    cbind(etapa = e, t)
  }))
  if (is.null(nc)) nc <- cbind(etapa = character(0), tabla_no_convergidos())
  guardar_csv(nc, "s6_no_convergidos.csv")
  registro(sprintf("Ajustes no convergidos en todas las etapas: %d", nrow(nc)))
  bs <- if (etapa_hecha("bootstrap")) leer_etapa("bootstrap") else NULL
  if (!is.null(bs) && etapa_hecha("influencia")) {
    comp <- leer_etapa("influencia")$comparacion
    sdb <- apply(bs$boot[bs$converge, , drop = FALSE], 2, sd)
    comp$max_abs_delta_beta_en_DE_bootstrap <- vapply(comp$delta_beta, function(db)
      if (is.null(db)) NA_real_ else max(abs(db) / sdb), numeric(1))
    guardar_csv(comp[, setdiff(names(comp), "delta_beta")], "s6_influencia_reajuste.csv")
    registro("Influencia: columna en DE bootstrap completada con las réplicas convergidas")
  }
  saveRDS(list(cols = prep$cols, C_best = af$C, prof_best = af$prof, hojas_best = af$hojas,
               idx_entrenamiento = prep$idx_entrenamiento, idx_prueba = prep$idx_prueba,
               lr = af$lr, p_lr = af$p_lr, p_rf = af$p_rf, yte = af$yte,
               boot = bs$boot, boot_converge = bs$converge, grupos = bs$grupos,
               oof = if (etapa_hecha("umbrales_oof")) leer_etapa("umbrales_oof")$oof,
               influencia = if (etapa_hecha("influencia"))
                 leer_etapa("influencia")[c("comparacion", "validacion_un_paso", "resumen_sombrero")],
               no_convergidos = nc, sensibilidad = sens),
          ruta_res("s6_objetos.rds"))
  list(sensibilidad = sens, no_convergidos = nc, pendientes = pendientes)
}

ETAPAS <- list(
  preparar = etapa_preparar,
  separacion = etapa_separacion,
  cv_logistica = etapa_cv_logistica,
  cv_bosque = etapa_cv_bosque,
  umbrales_oof = etapa_umbrales_oof,
  ajuste_final = etapa_ajuste_final,
  influencia = etapa_influencia,
  sens_sin_similitud = etapa_sens_columnas("sin índice de similitud", function(cols) setdiff(cols, SIMILITUD)),
  sens_lexicas = etapa_sens_columnas("solo variables léxicas del dataset", function(cols) intersect(cols, LEXICAS_URL)),
  sens_html = etapa_sens_columnas("solo variables de página HTML", function(cols) intersect(cols, DE_PAGINA)),
  sens_url_recalculadas = etapa_sens_url_recalculadas,
  sens_dominio = etapa_sens_dominio,
  sens_razones_nchar = etapa_sens_razones_nchar,
  sens_sin_tld = etapa_sens_columnas("sin TLDLegitimateProb (cálculo no documentado por UCI)",
                                     function(cols) setdiff(cols, "TLDLegitimateProb")),
  sens_formato_bosque = etapa_sens_formato("rf"),
  sens_formato_logistica = etapa_sens_formato("lr"),
  bootstrap_prueba = function() etapa_bootstrap(B = B_PRUEBA_TIEMPO, escribir = FALSE),
  bootstrap = etapa_bootstrap,
  consolidar = etapa_consolidar
)
ETAPAS_ORDEN <- names(ETAPAS)
# Etapas de las que depende cada una (el lanzador omite una etapa si falta alguna).
DEPENDENCIAS <- c(
  list(preparar = character(0), separacion = "preparar", cv_logistica = "preparar", cv_bosque = "preparar",
       ajuste_final = c("cv_logistica", "cv_bosque", "umbrales_oof"), umbrales_oof = c("cv_logistica", "cv_bosque"),
       bootstrap_prueba = "cv_logistica", bootstrap = "cv_logistica",
       influencia = "ajuste_final", consolidar = "ajuste_final"),
  setNames(rep(list(c("cv_logistica", "cv_bosque")), length(ESCENARIOS)), ESCENARIOS))

# Ejecuta una etapa por su nombre: abre su log, la corre y guarda su resultado.
ejecutar_etapa <- function(nombre) {
  if (!nombre %in% ETAPAS_ORDEN)
    stop("Etapa desconocida: ", nombre, ". Etapas: ", paste(ETAPAS_ORDEN, collapse = ", "))
  iniciar_log(paste0("s6_", nombre))
  .no_conv$filas <- list()
  t0 <- Sys.time()
  registro(sprintf("=== Sección 6, etapa '%s' (PID %d%s) ===", nombre, Sys.getpid(),
                   if (MODO_RAPIDO) ", PRUEBA RÁPIDA" else ""))
  obj <- ETAPAS[[nombre]]()
  obj$segundos_etapa <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  guardar_etapa(nombre, obj)
  registro(sprintf("Etapa '%s' terminada en %.0f s | ajustes no convergidos en la etapa: %d",
                   nombre, obj$segundos_etapa, nrow(tabla_no_convergidos())))
  invisible(obj)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) {
    message("Uso: Rscript codigo_R/seccion_6_modelacion_evaluacion.R <etapa>\n",
            "Etapas en orden: ", paste(ETAPAS_ORDEN, collapse = ", "), "\n",
            "Para correrlas con límite de tiempo: Rscript codigo_R/ejecutar_s6.R todas")
    quit(status = 2)
  }
  ejecutar_etapa(args[1])
}
