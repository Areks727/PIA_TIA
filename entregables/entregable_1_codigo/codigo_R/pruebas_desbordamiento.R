# Pruebas de desbordamiento de enteros con valores grandes.
#
# En R, sum() y length() devuelven enteros de 32 bits, y el producto de dos enteros
# desborda (da NA) por encima de 2^31 - 1 = 2,147,483,647. Con ~100,000 observaciones por
# clase, n1 * n0 ~ 1e10 desborda. Estas pruebas usan conteos de ese orden y fallan si
# alguna función devuelve NA o un valor distinto del esperado.

source(if (dir.exists("codigo_R")) "codigo_R/seccion_5_exploracion_inferencia.R" else "seccion_5_exploracion_inferencia.R",
       encoding = "UTF-8")
source(if (dir.exists("codigo_R")) "codigo_R/seccion_6_modelacion_evaluacion.R" else "seccion_6_modelacion_evaluacion.R",
       encoding = "UTF-8")

fallos <- 0L
comprobar <- function(nombre, ok) {
  ok <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (ok) "ok" else "FALLA", nombre))
  if (!ok) fallos <<- fallos + 1L
}

n1 <- 100520L; n0 <- 134850L  # tamaños reales de cada clase tras deduplicar
comprobar("el producto entero n1 * n0 desborda (control de que la prueba es útil)",
          is.na(suppressWarnings(n1 * n0)))

# auc_roc: separación perfecta (AUC = 1), puntajes constantes (AUC = 0.5) e invertidos (AUC = 0).
y <- c(rep(1L, n1), rep(0L, n0))
comprobar("auc_roc = 1 con separación perfecta", abs(auc_roc(y, c(rep(1, n1), rep(0, n0))) - 1) < 1e-12)
comprobar("auc_roc = 0.5 con puntajes constantes", abs(auc_roc(y, rep(0.3, n1 + n0)) - 0.5) < 1e-12)
comprobar("auc_roc = 0 con puntajes invertidos", abs(auc_roc(y, c(rep(0, n1), rep(1, n0)))) < 1e-12)

# U de Mann-Whitney: con todos los valores empatados U = n1 * n0 / 2.
comprobar("u_mann_whitney = n1*n0/2 con empates totales",
          abs(u_mann_whitney(rep(5L, n1), rep(5L, n0)) - as.numeric(n1) * n0 / 2) < 1e-6)

# I3 completo con datos grandes: debe dar un AUC finito en [0, 1] y un IC finito.
set.seed(1)
d3 <- data.frame(phishing = y, longitud_url = c(rpois(n1, 60), rpois(n0, 25)), URLLength = c(rpois(n1, 60), rpois(n0, 24)))
f3 <- inf_mann_whitney(d3)
comprobar("inf_mann_whitney da estimación e IC finitos", all(is.finite(c(f3$estimacion, f3$IC95_inf, f3$IC95_sup))))

# Wilson con los conteos reales de I1 (validación: 0.427072 [0.425075, 0.429072]).
w <- wilson(100520L, 235370L)
comprobar("wilson reproduce I1", all(abs(round(w, 6) - c(0.427072, 0.425075, 0.429072)) < 1e-6))

# Métricas con conteos grandes.
m <- metricas_en_umbral(y, c(rep(0.9, n1), rep(0.1, n0)), 0.5)
comprobar("metricas_en_umbral con n grande", all(is.finite(unlist(m[, -1]))) && m$F1 == 1 && m$VP == n1)

# Costo esperado por cada 10,000 URL.
tab <- cbind(modelo = "x", metricas_en_umbral(y, c(rep(0.9, n1 - 10), rep(0.1, 10), rep(0.1, n0)), 0.5))
cst <- costo_por_umbral(tab, razones = 50)
comprobar("costo_por_umbral con n grande", abs(cst$costo_por_10000 - 10000 * 50 * 10 / (n1 + n0)) < 1e-9)

# Tamaños de submuestra y lambda.
comprobar("tamaño de submuestra estratificada sin desbordar",
          round(as.numeric(N_SUBMUESTRA_CV) * 75390L / 176527L) == 12812)
comprobar("lambda_desde_C con n entero grande", is.finite(lambda_desde_C(10, 176527L)))

cat(sprintf("\nPruebas de desbordamiento: %d fallos\n", fallos))
if (fallos > 0L) quit(status = 1)
