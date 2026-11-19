# Comparación de type.logistic = "Newton" (por omisión) y "modified.Newton" en glmnet.
#
# "modified.Newton" usa la cota superior 0.25 para los pesos p(1 - p), lo que vuelve el
# paso más conservador. Se compara en el pliegue que divergió (pliegue 2, C = 10) y en dos
# casos sin problemas. Como dev.ratio no incluye la penalización, se compara el OBJETIVO
# PENALIZADO que minimiza glmnet (alpha = 0, pesos 1/n):
#   objetivo = -(1/n) * log-verosimilitud + (lambda / 2) * sum(beta^2)   (intercepto sin penalizar)
#            = devianza media / 2 + (lambda / 2) * sum(beta^2)
# El menor objetivo es el más cercano al óptimo (el problema es convexo, el óptimo es único).
# Como referencia del óptimo se añade Newton con camino fino y tolerancia estricta (1e-10).
# Escribe resultados_R/s6_comparacion_tipo_newton.csv. No interviene en el modelo principal.

source(if (dir.exists("codigo_R")) "codigo_R/seccion_6_modelacion_evaluacion.R" else "seccion_6_modelacion_evaluacion.R",
       encoding = "UTF-8")
iniciar_log("s6_comparar_tipo_newton")
prep <- leer_etapa("preparar")
df <- cargar_datos(TRUE)
X <- as.matrix(df[prep$idx_entrenamiento, prep$cols]); y <- df[[OBJETIVO]][prep$idx_entrenamiento]
f <- pliegues_estratificados(y, K_CV)  # mismos pliegues que la validación cruzada

objetivo_penalizado <- function(Z, y, a0, b, lambda) {
  eta <- a0 + as.numeric(Z %*% b)
  nll <- mean(log1p(exp(-abs(eta))) + pmax(eta, 0) - y * eta)  # -(1/n) log-verosimilitud, estable
  c(objetivo = nll + lambda / 2 * sum(b^2), devianza_media = 2 * nll, penalizacion = lambda / 2 * sum(b^2))
}

variantes <- list(list("Newton, camino fino, thresh 1e-7 (método principal)", "Newton", 1e-7),
                  list("modified.Newton, camino fino, thresh 1e-7", "modified.Newton", 1e-7),
                  list("Newton, camino fino, thresh 1e-10 (referencia del óptimo)", "Newton", 1e-10))
filas <- list()
for (caso in list(c(pliegue = 2, C = 10), c(pliegue = 1, C = 10), c(pliegue = 2, C = 1))) {
  j <- caso[["pliegue"]]; C <- caso[["C"]]
  a <- f != j; b <- f == j
  esc <- ajustar_escala(X[a, ]); Za <- aplicar_escala(X[a, ], esc); Zb <- aplicar_escala(X[b, ], esc)
  for (v in variantes) {
    m <- ajustar_l2(Za, y[a], C, thresh = v[[3]], tipo = v[[2]], contexto = paste("comparación:", v[[1]]))
    ob <- objetivo_penalizado(Za, y[a], m$a0, m$b, m$lambda)
    p <- plogis(m$a0 + as.numeric(Zb %*% m$b))
    filas[[length(filas) + 1]] <- data.frame(
      pliegue = j, C = C, variante = v[[1]], converge = m$converge,
      objetivo_penalizado = ob[["objetivo"]], devianza_media = ob[["devianza_media"]],
      penalizacion = ob[["penalizacion"]], dev_ratio = m$dev_ratio, max_abs_beta = max(abs(m$b)),
      logloss_validacion = perdida_log(y[b], p), segundos = m$segundos, b = I(list(m$b)))
    registro(sprintf("pliegue %d C=%-3g %-58s objetivo=%.10f (devianza media %.8f + penalización %.8f) %.1f s",
                     j, C, v[[1]], ob[["objetivo"]], ob[["devianza_media"]], ob[["penalizacion"]], m$segundos))
  }
}
tab <- do.call(rbind, filas)
tab$diferencia_objetivo_vs_referencia <- NA_real_
tab$max_abs_dif_beta_vs_referencia <- NA_real_
for (i in seq_len(nrow(tab))) {
  r <- which(tab$pliegue == tab$pliegue[i] & tab$C == tab$C[i] & grepl("referencia", tab$variante))
  tab$diferencia_objetivo_vs_referencia[i] <- tab$objetivo_penalizado[i] - tab$objetivo_penalizado[r]
  tab$max_abs_dif_beta_vs_referencia[i] <- max(abs(tab$b[[i]] - tab$b[[r]]))
}
tab$b <- NULL
guardar_csv(tab, "s6_comparacion_tipo_newton.csv")
registro_tabla(tab[, c("pliegue", "C", "variante", "objetivo_penalizado", "diferencia_objetivo_vs_referencia",
                       "max_abs_dif_beta_vs_referencia", "segundos")], digits = 8)
