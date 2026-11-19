# Complementos de la sección 6 (cierran los puntos que quedaron parciales en la propuesta):
#   a) matriz de confusión completa y F1 por umbral y modelo, más la regla de dos condiciones
#      como línea base, y umbrales evaluados con predicciones fuera de pliegue (solo entrenamiento);
#   b) grupos de predictores redundantes: sus razones de momios individuales no se interpretan;
#      se reporta la dirección conjunta del grupo;
#   c) influencia: reajuste sin las observaciones marcadas (método principal) y apalancamiento /
#      distancia de Cook sobre la aproximación ponderada del ajuste penalizado (secundario).
# Requiere que comun.R y las funciones de seccion_6_modelacion_evaluacion.R estén cargadas.

# ---------------------------------------------------------------------------------
# a) Línea base y umbrales fuera de pliegue
# ---------------------------------------------------------------------------------
# Regla descriptiva "sin HTTPS o URLSimilarityIndex < 100 = phishing" evaluada como
# clasificador fijo (no se ajusta nada, así que no hay riesgo de fuga).
metricas_regla <- function(d) {
  pred <- as.numeric(d$IsHTTPS == 0 | d$URLSimilarityIndex < 100)
  cbind(modelo = "regla de dos condiciones", metricas_en_umbral(d[[OBJETIVO]], pred, 0.5)[, -1],
        umbral = NA_real_)
}

# Predicciones fuera de pliegue (K_CV = 5 pliegues) dentro del entrenamiento, con los
# hiperparámetros ya elegidos. Sirven para elegir el umbral sin tocar la prueba.
predicciones_oof <- function(X, y, C, prof, hojas, k = K_CV) {
  f <- pliegues_estratificados(y, k, semilla = SEMILLA + 1L)
  p_lr <- p_rf <- numeric(length(y))
  for (j in seq_len(k)) {
    a <- f != j; b <- f == j
    p_lr[b] <- predecir_pipeline_l2(ajustar_pipeline_l2(X[a, ], y[a], C,
                                       contexto = sprintf("fuera de pliegue %d", j)), X[b, ])
    p_rf[b] <- predecir_bosque(ajustar_bosque(X[a, ], y[a], N_ARBOLES, prof, hojas), X[b, ])
    registro(sprintf("   pliegue %d/%d fuera de muestra listo", j, k))
  }
  list(lr = p_lr, rf = p_rf)
}

# Métricas de cada modelo en cada umbral (matriz de confusión, sensibilidad, especificidad, F1, etc.).
tabla_umbrales <- function(y, preds, umbrales = UMBRALES) {
  do.call(rbind, lapply(names(preds), function(nm)
    do.call(rbind, lapply(umbrales, function(u) cbind(modelo = nm, metricas_en_umbral(y, preds[[nm]], u))))))
}

# Costo esperado por cada 10,000 URL con la prevalencia observada, para varias razones
# de costo FN:FP. Solo se calcula con predicciones fuera de pliegue del entrenamiento.
costo_por_umbral <- function(tab_oof, razones = c(1, 5, 10, 50)) {
  do.call(rbind, lapply(razones, function(r) {
    d <- tab_oof
    n <- d$VN + d$FP + d$FN + d$VP
    d$razon_costo_FN_FP <- r
    d$costo_por_10000 <- 10000 * (d$FP * 1 + d$FN * r) / n
    d
  })) |>
    dplyr::group_by(modelo, razon_costo_FN_FP) |>
    dplyr::mutate(umbral_de_menor_costo = umbral[which.min(costo_por_10000)]) |>
    dplyr::ungroup() |> as.data.frame()
}

# ---------------------------------------------------------------------------------
# b) Grupos de predictores redundantes
# ---------------------------------------------------------------------------------
# Derivaciones exactas que se comprueban en el entrenamiento (variable derivada -> origen).
# Las razones se comprueban como conteo / URLLength redondeado a 3 decimales (diferencia
# máxima permitida 5e-4, la del redondeo). Nota: URLLength es la columna con desfase ligado
# a la clase, así que estas razones heredan ese denominador.
es_razon_de <- function(razon, conteo) function(d)
  max(abs(d[[razon]] - d[[conteo]] / d$URLLength)) <= 5e-4 + 1e-12
DERIVACIONES <- list(
  list(derivada = "HasObfuscation", origen = "NoOfObfuscatedChar",
       regla = "HasObfuscation = 1 si NoOfObfuscatedChar > 0",
       prueba = function(d) all(d$HasObfuscation == as.numeric(d$NoOfObfuscatedChar > 0))),
  list(derivada = "ObfuscationRatio", origen = "NoOfObfuscatedChar",
       regla = "ObfuscationRatio = NoOfObfuscatedChar / URLLength (3 decimales)",
       prueba = es_razon_de("ObfuscationRatio", "NoOfObfuscatedChar")),
  list(derivada = "LetterRatioInURL", origen = "NoOfLettersInURL",
       regla = "LetterRatioInURL = NoOfLettersInURL / URLLength (3 decimales)",
       prueba = es_razon_de("LetterRatioInURL", "NoOfLettersInURL")),
  list(derivada = "DegitRatioInURL", origen = "NoOfDegitsInURL",
       regla = "DegitRatioInURL = NoOfDegitsInURL / URLLength (3 decimales)",
       prueba = es_razon_de("DegitRatioInURL", "NoOfDegitsInURL"))
)

# Componentes conexas del grafo cuyas aristas son |Spearman| >= umbral (en entrenamiento)
# o derivaciones exactas comprobadas.
grupos_redundantes <- function(dtr, cols, umbral = 0.9) {
  R <- suppressWarnings(cor(as.matrix(dtr[, cols]), method = "spearman"))
  idx <- which(abs(R) >= umbral & upper.tri(R), arr.ind = TRUE)
  aristas <- data.frame(variable_1 = cols[idx[, 1]], variable_2 = cols[idx[, 2]],
                        criterio = sprintf("|Spearman| = %.4f (entrenamiento)", abs(R[idx])),
                        spearman = R[idx])
  for (dv in DERIVACIONES) {
    if (all(c(dv$derivada, dv$origen) %in% cols) && isTRUE(dv$prueba(dtr))) {
      aristas <- rbind(aristas, data.frame(variable_1 = dv$origen, variable_2 = dv$derivada,
                                           criterio = paste("derivación exacta:", dv$regla),
                                           spearman = R[dv$origen, dv$derivada]))
    }
  }
  # Componentes conexas por unión de conjuntos.
  padre <- setNames(cols, cols)
  raiz <- function(v) { while (padre[[v]] != v) v <- padre[[v]]; v }
  for (i in seq_len(nrow(aristas))) {
    a <- raiz(aristas$variable_1[i]); b <- raiz(aristas$variable_2[i])
    if (a != b) padre[[b]] <- a
  }
  comp <- vapply(cols, raiz, character(1))
  en_grupo <- comp[comp %in% comp[duplicated(comp)]]
  grupos <- split(names(en_grupo), en_grupo)
  grupos <- grupos[order(vapply(grupos, function(g) match(g[1], cols), numeric(1)))]
  names(grupos) <- sprintf("G%d", seq_along(grupos))
  list(grupos = grupos, aristas = aristas, R = R)
}

# Dirección conjunta de cada grupo: suma de coeficientes estandarizados alineados por el
# signo de su correlación con la primera variable del grupo (un aumento conjunto de 1 DE
# en el sentido en que las variables se mueven juntas). IC bootstrap percentil con las
# mismas réplicas de los coeficientes individuales.
direccion_grupos <- function(grupos, R, cols, beta, boot) {
  do.call(rbind, lapply(names(grupos), function(g) {
    v <- grupos[[g]]
    s <- sign(R[v[1], v]); s[s == 0] <- 1
    j <- match(v, cols)
    est <- sum(s * beta[j])
    bs <- as.numeric(boot[, j, drop = FALSE] %*% s)
    lo <- quantile(bs, 0.025, names = FALSE); hi <- quantile(bs, 0.975, names = FALSE)
    direccion <- if (lo > 0) "aumenta la probabilidad de phishing"
                 else if (hi < 0) "disminuye la probabilidad de phishing"
                 else "dirección no determinada (IC incluye 0)"
    data.frame(grupo = g, variables = paste(v, collapse = " + "),
               signos_de_alineacion = paste(ifelse(s > 0, "+", "-"), collapse = " "),
               suma_beta_estandarizado = est, IC95_inf_suma = lo, IC95_sup_suma = hi,
               proporcion_replicas_positivas = mean(bs > 0), direccion = direccion)
  }))
}

# Marca la tabla de razones de momios: las variables de un grupo redundante no tienen OR
# individual interpretable; se dejan sus columnas de OR vacías y se remite al grupo.
marcar_or <- function(tabla, grupos) {
  pertenece <- setNames(rep(NA_character_, nrow(tabla)), tabla$variable)
  for (g in names(grupos)) pertenece[grupos[[g]]] <- g
  tabla$grupo_redundante <- unname(pertenece[tabla$variable])
  tabla$interpretable <- ifelse(is.na(tabla$grupo_redundante),
                                "solo dirección (separación cuasi-completa)",
                                "no interpretable por separado: ver grupo")
  cols_or <- grep("^(OR_|IC95_)", names(tabla), value = TRUE)
  tabla[!is.na(tabla$grupo_redundante), c(cols_or, "beta_estandarizado")] <- NA
  tabla$direccion <- ifelse(!is.na(tabla$grupo_redundante), "ver grupo",
                            ifelse(tabla$IC_incluye_1, "no determinada (IC incluye 1)",
                                   ifelse(tabla$OR_por_DE > 1, "aumenta", "disminuye")))
  tabla$IC_incluye_1[!is.na(tabla$grupo_redundante)] <- NA
  tabla
}

# ---------------------------------------------------------------------------------
# c) Influencia
# ---------------------------------------------------------------------------------
# Aproximación ponderada del ajuste penalizado en su solución (última iteración de IRLS):
#   A = Zc' W Zc + n * lambda * D,  Zc = [1, Z] (Z estandarizada),  D = diag(0, 1, ..., 1)
#   h_ii = w_i * zc_i' A^{-1} zc_i,   w_i = p_i (1 - p_i)
#   Cook_i (un paso, Pregibon) = r_Pi^2 * h_ii / (gl_ef * (1 - h_ii)^2),   gl_ef = tr(H)
#   Delta beta_i (un paso) = beta_(-i) - beta = -A^{-1} zc_i (y_i - p_i) / (1 - h_ii)
# El intercepto no se penaliza (D[1,1] = 0), igual que en glmnet.
sombrero_penalizado <- function(Z, y, m) {
  n <- nrow(Z)
  p <- predecir_l2(m, Z)
  w <- p * (1 - p)
  Zc <- cbind(1, Z)
  D <- diag(c(0, rep(1, ncol(Z))))
  A <- crossprod(Zc * sqrt(w)) + n * m$lambda * D
  Ainv <- solve(A)
  h <- w * rowSums((Zc %*% Ainv) * Zc)
  rp <- (y - p) / sqrt(pmax(w, 1e-300))
  gl <- sum(h)
  cook <- rp^2 * h / (gl * (1 - h)^2)
  list(h = h, cook = cook, w = w, p = p, rp = rp, gl_efectivos = gl, Ainv = Ainv, Zc = Zc)
}

# Aproximación de un paso del cambio en los coeficientes al quitar la observación i:
# delta_beta = -A^-1 z_i (y_i - p_i) / (1 - h_ii). Se valida contra reajustes reales.
delta_beta_un_paso <- function(s, i, y) {
  -as.numeric(s$Ainv %*% s$Zc[i, ] * (y[i] - s$p[i]) / (1 - s$h[i]))
}

# Reajusta la logística L2 sin un conjunto de observaciones y compara coeficientes y
# predicciones con el ajuste completo. La escala se mantiene fija (la del entrenamiento
# completo) para aislar el efecto de las observaciones; lambda = 1 / (C * n_restante).
TOL_INFLUENCIA <- 1e-12  # tolerancia estricta: los cambios buscados son pequeños
comparar_reajuste <- function(Z, y, m0, quitar, Zte, etiqueta, boot_sd) {
  if (!length(quitar)) {
    return(data.frame(conjunto = etiqueta, n_quitadas = 0L, phishing_entre_quitadas = 0L,
                      max_abs_delta_beta = NA_real_, variable_max_delta = NA_character_,
                      max_abs_delta_beta_en_DE_bootstrap = NA_real_, cambios_de_signo = NA_integer_,
                      max_abs_delta_p_entrenamiento = NA_real_, max_abs_delta_p_prueba = NA_real_,
                      cambios_clasificacion_prueba = NA_character_, reajuste_converge = NA,
                      delta_beta = I(list(NULL))))
  }
  # m0: ajuste completo con la misma tolerancia estricta (referencia).
  m1 <- ajustar_l2(Z[-quitar, , drop = FALSE], y[-quitar], m0$C, thresh = TOL_INFLUENCIA,
                   contexto = paste("influencia, reajuste sin:", etiqueta))
  db <- m1$b - m0$b
  p0 <- predecir_l2(m0, Zte); p1 <- predecir_l2(m1, Zte)
  ptr0 <- predecir_l2(m0, Z); ptr1 <- predecir_l2(m1, Z)
  cambios <- vapply(UMBRALES, function(u) sum((p0 >= u) != (p1 >= u)), numeric(1))
  data.frame(
    conjunto = etiqueta, n_quitadas = length(quitar),
    phishing_entre_quitadas = sum(y[quitar]),
    max_abs_delta_beta = max(abs(db)),
    variable_max_delta = colnames(Z)[which.max(abs(db))],
    max_abs_delta_beta_en_DE_bootstrap = if (all(is.na(boot_sd))) NA_real_ else max(abs(db) / boot_sd),
    cambios_de_signo = sum(sign(m1$b) != sign(m0$b)),
    max_abs_delta_p_entrenamiento = max(abs(ptr1 - ptr0)),
    max_abs_delta_p_prueba = max(abs(p1 - p0)),
    cambios_clasificacion_prueba = paste(sprintf("%.2f:%d", UMBRALES, cambios), collapse = "; "),
    reajuste_converge = m1$converge,
    delta_beta = I(list(db))
  )
}

# Influencia: reajusta la logística sin las observaciones con residuo de Pearson > 3 y sin las de mayor
# distancia de Cook aproximada, y lo compara con quitar el mismo número al azar (varias repeticiones).
analisis_influencia <- function(Xtr, ytr, Xte, lr, boot, repeticiones = 10L) {
  Z <- aplicar_escala(Xtr, lr$escala)
  Zte <- aplicar_escala(Xte, lr$escala)
  colnames(Z) <- colnames(Xtr)
  # Sin bootstrap (la influencia se corre antes) la columna en DE bootstrap queda en NA y
  # se completa en la etapa consolidar.
  boot_sd <- if (is.null(boot)) rep(NA_real_, ncol(Z)) else apply(boot, 2, sd)
  m0 <- ajustar_l2(Z, ytr, lr$C, thresh = TOL_INFLUENCIA,  # mismo modelo, tolerancia estricta
                   contexto = "influencia, ajuste de referencia")
  s <- sombrero_penalizado(Z, ytr, m0)
  marcadas <- which(abs(s$rp) > 3)
  top_cook <- order(-s$cook)[1:10]

  # Método principal: reajuste sin las observaciones marcadas.
  comp <- list(comparar_reajuste(Z, ytr, m0, marcadas, Zte, "residuo de Pearson |r| > 3", boot_sd),
               comparar_reajuste(Z, ytr, m0, top_cook, Zte, "10 mayores distancias de Cook (aprox.)", boot_sd))
  # Control: quitar el mismo número de observaciones al azar (al menos 10), repetido con
  # semillas distintas para tener el rango de cambios que produce quitar filas cualesquiera.
  n_azar <- max(10L, length(marcadas))
  for (r in seq_len(repeticiones)) {
    set.seed(SEMILLA + r)
    azar <- sample.int(length(ytr), n_azar)
    comp[[length(comp) + 1]] <- comparar_reajuste(
      Z, ytr, m0, azar, Zte, sprintf("control: %d observaciones al azar (repetición %d)", n_azar, r), boot_sd)
  }
  comp <- do.call(rbind, comp)
  ctrl <- comp[startsWith(comp$conjunto, "control"), ]
  metricas <- c("max_abs_delta_beta", "max_abs_delta_beta_en_DE_bootstrap", "cambios_de_signo",
                "max_abs_delta_p_entrenamiento", "max_abs_delta_p_prueba")
  rango_azar <- data.frame(metrica = metricas, repeticiones = nrow(ctrl),
                           minimo = vapply(metricas, function(v) min(ctrl[[v]]), numeric(1)),
                           mediana = vapply(metricas, function(v) median(ctrl[[v]]), numeric(1)),
                           maximo = vapply(metricas, function(v) max(ctrl[[v]]), numeric(1)),
                           residuo_pearson_mayor_3 = as.numeric(comp[1, metricas]),
                           diez_mayores_cook = as.numeric(comp[2, metricas]))

  # Validación de la aproximación de un paso: eliminación individual de las 5 mayores Cook.
  val <- do.call(rbind, lapply(top_cook[1:5], function(i) {
    mi <- ajustar_l2(Z[-i, ], ytr[-i], lr$C, thresh = TOL_INFLUENCIA,
                     contexto = sprintf("influencia, sin la fila %d", i))
    real <- mi$b - m0$b
    aprox <- delta_beta_un_paso(s, i, ytr)[-1]
    data.frame(fila_entrenamiento = i, clase = ytr[i], p_ajustada = s$p[i], h = s$h[i], cook = s$cook[i],
               max_abs_delta_beta_real = max(abs(real)), max_abs_delta_beta_un_paso = max(abs(aprox)),
               correlacion_real_vs_un_paso = suppressWarnings(cor(real, aprox)),
               reajuste_converge = mi$converge)
  }))

  resumen_h <- data.frame(
    n = length(ytr), gl_efectivos = s$gl_efectivos, p_mas_intercepto = ncol(Z) + 1,
    h_media = mean(s$h), h_p99 = quantile(s$h, 0.99, names = FALSE), h_max = max(s$h),
    umbral_2p_n = 2 * s$gl_efectivos / length(ytr), n_h_mayor_2p_n = sum(s$h > 2 * s$gl_efectivos / length(ytr)),
    prop_pesos_menores_1e_6 = mean(s$w < 1e-6), prop_pesos_menores_1e_3 = mean(s$w < 1e-3),
    cook_max = max(s$cook), n_cook_mayor_4_n = sum(s$cook > 4 / length(ytr)),
    n_residuo_pearson_mayor_3 = length(marcadas))
  detalle <- data.frame(fila_entrenamiento = c(marcadas, top_cook),
                        motivo = c(rep("residuo |r| > 3", length(marcadas)), rep("top 10 Cook", 10)),
                        clase = ytr[c(marcadas, top_cook)], p_ajustada = s$p[c(marcadas, top_cook)],
                        residuo_pearson = s$rp[c(marcadas, top_cook)], h = s$h[c(marcadas, top_cook)],
                        cook = s$cook[c(marcadas, top_cook)])
  list(comparacion = comp, validacion_un_paso = val, resumen_sombrero = resumen_h, rango_azar = rango_azar,
       detalle = detalle, sombrero = s, marcadas = marcadas)
}
