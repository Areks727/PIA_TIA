# Sección 5 de la propuesta (producto §3, etapas 3 y 4): exploración e inferencia.
#
# Trabaja con la muestra deduplicada. La longitud de la URL se recalcula como
# nchar(URL) porque la columna URLLength tiene un desfase ligado a la clase (ver
# sección 4). Genera tablas descriptivas y figuras (con título, ejes, unidades y
# leyenda) y cuatro análisis inferenciales, cada uno con hipótesis, nivel de
# significancia, supuestos, estadístico, intervalo y valor p.

source(if (dir.exists("codigo_R")) "codigo_R/comun.R" else "comun.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(ggplot2); library(tidyr) })

ALFA <- 0.05
Z <- qnorm(1 - ALFA / 2)
VARS_NUM <- c("longitud_url", "NoOfSubDomain", "NoOfExternalRef", "NoOfImage", "LineOfCode",
              "URLSimilarityIndex", "CharContinuationRate", "TLDLegitimateProb",
              "NoOfDegitsInURL", "NoOfJS")
VARS_BIN <- c("IsHTTPS", "HasSocialNet", "HasCopyrightInfo", "HasTitle",
              "IsDomainIP", "HasFavicon", "HasPasswordField", "HasSubmitButton")
VARS_HEAT <- c("longitud_url", "DomainLength", "NoOfSubDomain", "CharContinuationRate",
               "URLCharProb", "TLDLegitimateProb", "URLSimilarityIndex", "NoOfExternalRef",
               "NoOfImage", "LineOfCode", "NoOfSelfRef", "HasSocialNet")
COLORES <- c("Legítimo" = "#4C78A8", "Phishing" = "#E45756")

# Convierte la clase 0/1 en un factor con etiquetas en español (para tablas y gráficas).
etiqueta_clase <- function(y) factor(ifelse(y == 1, "Phishing", "Legítimo"),
                                     levels = c("Legítimo", "Phishing"))

# Intervalo de confianza de Wilson para una proporción k/n. Se convierte a numérico antes de
# multiplicar porque n * n desborda con enteros.
wilson <- function(k, n) {
  k <- as.numeric(k); n <- as.numeric(n)  # n * n desborda con enteros
  p <- k / n
  den <- 1 + Z^2 / n
  centro <- (p + Z^2 / (2 * n)) / den
  mitad <- Z * sqrt(p * (1 - p) / n + Z^2 / (4 * n * n)) / den
  c(p = p, inf = centro - mitad, sup = centro + mitad)
}

# Resumen por clase de cada variable numérica: mediana, cuartiles, percentiles 5 y 95,
# media y desviación estándar (cuantiles tipo 7, igual que pandas).
resumen_numerico <- function(df) {
  do.call(rbind, lapply(VARS_NUM, function(v) do.call(rbind, lapply(c(1, 0), function(cl) {
    x <- df[[v]][df[[OBJETIVO]] == cl]
    q <- quantile(x, c(.25, .75, .05, .95), type = 7, names = FALSE)  # type 7 = pandas
    data.frame(variable = v, clase = ifelse(cl == 1, "phishing", "legítimo"), n = length(x),
               mediana = median(x), Q1 = q[1], Q3 = q[2], P5 = q[3], P95 = q[4],
               media = mean(x), desv_est = sd(x))
  }))))
}

# Proporción de cada característica binaria en cada clase y su diferencia en puntos porcentuales.
resumen_binario <- function(df) {
  do.call(rbind, lapply(VARS_BIN, function(v) {
    p1 <- mean(df[[v]][df[[OBJETIVO]] == 1]); p0 <- mean(df[[v]][df[[OBJETIVO]] == 0])
    data.frame(variable = v, proporcion_en_phishing = p1, proporcion_en_legitimo = p0,
               diferencia_pp = 100 * (p1 - p0))
  }))
}

# Arma una fila de la tabla de inferencia (prueba, hipótesis, estadístico, valor p, estimación,
# IC 95 %, supuestos) y decide si se rechaza H0 con el nivel ALFA.
fila_inferencia <- function(id, prueba, variable, H0, estadistico, valor_p, estimacion,
                            inf, sup, n, supuestos, decision = NULL, ...) {
  if (is.null(decision)) decision <- if (valor_p < ALFA) "rechaza H0" else "no rechaza H0"
  data.frame(id = id, prueba = prueba, variable = variable, H0 = H0, alfa = ALFA,
             estadistico = estadistico, valor_p = valor_p, decision = decision,
             estimacion = estimacion, IC95_inf = inf, IC95_sup = sup, n = n,
             supuestos = supuestos, ...)
}

# I1. Prevalencia de phishing con intervalo de Wilson.
inf_prevalencia <- function(df) {
  n <- nrow(df); k <- sum(df[[OBJETIVO]])
  w <- wilson(k, n)
  fila_inferencia("I1", "IC de Wilson 95% para la prevalencia de phishing", OBJETIVO,
                  "no aplica (estimación)", NA_real_, NA_real_, w[["p"]], w[["inf"]], w[["sup"]], n,
                  "observaciones independientes tras deduplicar; muestra grande",
                  decision = "no aplica")
}

# I2. Diferencia de proporciones de IsHTTPS entre clases (prueba z) y razón de momios.
inf_dos_proporciones <- function(df, v = "IsHTTPS") {
  ph <- df[[v]][df[[OBJETIVO]] == 1]; lg <- df[[v]][df[[OBJETIVO]] == 0]
  n1 <- as.numeric(length(ph)); n2 <- as.numeric(length(lg))
  x1 <- as.numeric(sum(ph)); x2 <- as.numeric(sum(lg))
  p1 <- x1 / n1; p2 <- x2 / n2
  pool <- (x1 + x2) / (n1 + n2)
  z <- (p1 - p2) / sqrt(pool * (1 - pool) * (1 / n1 + 1 / n2))
  pval <- 2 * pnorm(-abs(z))
  se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  celdas <- c(x1, n1 - x1, x2, n2 - x2)
  cero <- min(celdas) == 0
  h <- if (cero) 0.5 else 0  # Haldane-Anscombe solo si alguna celda es cero
  a <- celdas + h
  orr <- (a[1] / a[2]) / (a[3] / a[4])
  se_log <- sqrt(sum(1 / a))
  ct <- suppressWarnings(chisq.test(matrix(celdas, 2, byrow = TRUE), correct = FALSE))
  fila_inferencia(
    "I2", sprintf("Comparación de dos proporciones: %s según clase (z)", v), v,
    "p(phishing) = p(legítimo)", z, pval, p1 - p2, (p1 - p2) - Z * se, (p1 - p2) + Z * se, n1 + n2,
    sprintf(paste("muestras independientes; n*p >= 10 en todas las celdas (mínimo esperado %.0f);",
                  "chi-cuadrada de contraste = %.2f, mismo valor p"), min(ct$expected), ct$statistic),
    proporcion_phishing = p1, proporcion_legitimo = p2, razon_momios = orr,
    RM_IC95_inf = exp(log(orr) - Z * se_log), RM_IC95_sup = exp(log(orr) + Z * se_log),
    nota_razon_momios = if (cero) "celda en cero: se aplica +0.5 a cada celda (Haldane-Anscombe)"
                        else "sin celdas en cero")
}

# U de Mann-Whitney de x1 frente a x0 con rangos promedio (igual que scipy).
u_mann_whitney <- function(x1, x0) {
  r <- rank(c(x1, x0))
  n1 <- as.numeric(length(x1))  # doble precisión: n1 * (n1 + 1) desborda con enteros
  sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2
}

# I3. Longitud de la URL entre clases: Mann-Whitney y probabilidad de superioridad.
inf_mann_whitney <- function(df, v = "longitud_url") {
  ph <- df[[OBJETIVO]] == 1
  x1 <- as.numeric(df[[v]][ph]); x0 <- as.numeric(df[[v]][!ph])
  n1 <- as.numeric(length(x1)); n0 <- as.numeric(length(x0))  # evita desbordamiento entero en n1 * n0
  # Aproximación normal con corrección por continuidad y por empates (como scipy).
  wt <- wilcox.test(x1, x0, alternative = "two.sided", exact = FALSE, correct = TRUE)
  U <- unname(wt$statistic)
  A <- U / (n1 * n0)
  q1 <- A / (2 - A); q2 <- 2 * A * A / (1 + A)
  se <- sqrt((A * (1 - A) + (n1 - 1) * (q1 - A * A) + (n0 - 1) * (q2 - A * A)) / (n1 * n0))
  ua <- u_mann_whitney(df$URLLength[ph], df$URLLength[!ph])
  fila_inferencia(
    "I3", sprintf("Mann-Whitney: longitud de la URL (%s) según clase", v), v,
    "P(X_phishing > X_legítimo) = 0.5", U, wt$p.value, A, A - Z * se, A + Z * se, n1 + n0,
    sprintf(paste("variable continua; independencia; no exige normalidad; IC por aproximación de",
                  "Hanley y McNeil. Con la columna URLLength (con desfase ligado a la clase) el AUC",
                  "sería %.4f, sobreestimado por el artefacto"), ua / (n1 * n0)))
}

# I4. Independencia entre HasSocialNet y clase (chi-cuadrada) y razón de momios.
inf_chi_cuadrada <- function(df, v = "HasSocialNet") {
  tab <- table(factor(df[[v]], levels = c(0, 1)), factor(df[[OBJETIVO]], levels = c(1, 0)))
  ct <- suppressWarnings(chisq.test(tab, correct = FALSE))
  a <- as.numeric(tab["0", "1"]); b <- as.numeric(tab["0", "0"])
  c <- as.numeric(tab["1", "1"]); d <- as.numeric(tab["1", "0"])
  orr <- (a / b) / (c / d)
  se <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  fila_inferencia(
    "I4", sprintf("Chi-cuadrada de independencia: %s y clase", v), v,
    sprintf("%s y clase son independientes", v), unname(ct$statistic), ct$p.value, orr,
    exp(log(orr) - Z * se), exp(log(orr) + Z * se), nrow(df),
    sprintf(paste("gl = %d; celdas esperadas >= 5 (mínimo %.0f); muestreo independiente;",
                  "estimación = razón de momios de phishing con %s=0 frente a %s=1"),
            unname(ct$parameter), min(ct$expected), v, v))
}

# Indicadores descriptivos de reglas simples y de formato (no ajustan modelos).
formato_comparable <- function(u) {
  str_detect(u, fixed("www.")) & startsWith(u, "https://") &
    str_count(u, fixed("/")) <= 3 & !endsWith(u, "/")
}

# Indicadores descriptivos de reglas simples (sin ajustar modelos): índice de similitud = 100,
# ausencia de HTTPS, la regla «sin HTTPS o similitud < 100» y el formato comparable.
# Es la base de la regla de referencia contra la que se comparan los modelos.
reglas_deterministas <- function(df) {
  ph <- df[[OBJETIVO]] == 1; lg <- !ph
  formato <- formato_comparable(df$URL)
  sim100 <- df$URLSimilarityIndex == 100
  sinhttps <- df$IsHTTPS == 0
  conip <- df$IsDomainIP == 1
  regla <- sinhttps | !sim100
  tabla <- data.frame(indicador = c(
    "legítimas con URLSimilarityIndex = 100 (%)",
    "phishing con URLSimilarityIndex = 100 (n)",
    "phishing con URLSimilarityIndex = 100 (%)",
    "legítimas sin HTTPS (n)", "phishing sin HTTPS (n)", "phishing sin HTTPS (%)",
    "URL con IP como dominio, phishing (n)", "URL con IP como dominio, legítimo (n)",
    "regla [sin HTTPS o similitud < 100]: falsas alarmas (n)",
    "regla [sin HTTPS o similitud < 100]: phishing detectado (n)",
    "regla [sin HTTPS o similitud < 100]: phishing detectado (%)",
    "regla [sin HTTPS o similitud < 100]: phishing omitido (n)",
    "phishing con formato comparable a legítimas (n)",
    "phishing con formato comparable a legítimas (%)",
    "legítimas con formato comparable (n)"),
    valor = c(100 * mean(sim100[lg]), sum(sim100 & ph), 100 * mean(sim100[ph]),
              sum(sinhttps & lg), sum(sinhttps & ph), 100 * mean(sinhttps[ph]),
              sum(conip & ph), sum(conip & lg),
              sum(regla & lg), sum(regla & ph), 100 * mean(regla[ph]), sum(!regla & ph),
              sum(formato & ph), 100 * mean(formato[ph]), sum(formato & lg)))
  guardar_csv(tabla, "s5_reglas_deterministas.csv")
  for (i in seq_len(nrow(tabla))) registro("  ", tabla$indicador[i], ": ", signif(tabla$valor[i], 6))
  tabla
}

# Tema común de las gráficas de la sección 5.
tema <- function() theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 11), legend.position = "top")

# Genera y guarda las figuras de la sección 5 (clases, longitud de URL, similitud, correlación,
# características binarias) en resultados_R/.
figuras <- function(df) {
  df$clase <- etiqueta_clase(df[[OBJETIVO]])
  n <- nrow(df)

  cuentas <- as.data.frame(table(clase = df$clase))
  g1 <- ggplot(cuentas, aes(clase, Freq, fill = clase)) + geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%s\n(%.1f %%)", format(Freq, big.mark = ","), 100 * Freq / n)),
              vjust = -0.2, size = 3.3) +
    scale_fill_manual(values = COLORES, guide = "none") +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.18))) +
    labs(x = NULL, y = "Número de URL",
         title = sprintf("Distribución de clases tras deduplicar (n = %s)", format(n, big.mark = ","))) +
    tema()
  ggsave(ruta_res("fig_s5_1_clases.png"), g1, width = 6, height = 4.2, dpi = 150)

  g2 <- ggplot(df, aes(log10(longitud_url), fill = clase)) +
    geom_histogram(aes(y = after_stat(density)), position = "identity", alpha = 0.55,
                   breaks = seq(log10(10), log10(200), length.out = 61)) +
    scale_fill_manual(values = COLORES, name = NULL) +
    labs(x = "log10 de la longitud de la URL (caracteres)", y = "Densidad (área igual a 1 por clase)",
         title = "Longitud de la URL según clase (nchar(URL))") + tema()
  ggsave(ruta_res("fig_s5_2_longitud_url.png"), g2, width = 7, height = 4.2, dpi = 150)

  bigotes <- df |> group_by(clase) |>
    summarise(lo = { q <- quantile(URLSimilarityIndex, c(.25, .75)); max(min(URLSimilarityIndex), q[1] - 1.5 * diff(q)) },
              hi = { q <- quantile(URLSimilarityIndex, c(.25, .75)); min(max(URLSimilarityIndex), q[2] + 1.5 * diff(q)) })
  g3 <- ggplot(df, aes(clase, URLSimilarityIndex, fill = clase)) +
    geom_boxplot(outlier.shape = NA, width = 0.5) +
    coord_cartesian(ylim = c(min(bigotes$lo), max(bigotes$hi))) +
    scale_fill_manual(values = COLORES, guide = "none") +
    labs(x = NULL, y = "Índice de similitud de URL (escala 0 a 100)",
         title = "Índice de similitud de URL según clase (sin valores atípicos)") + tema()
  ggsave(ruta_res("fig_s5_3_similitud_por_clase.png"), g3, width = 6, height = 4.2, dpi = 150)

  R <- cor(as.matrix(df[, VARS_HEAT]), method = "spearman")
  larga <- as.data.frame(as.table(R))
  larga$Var1 <- factor(larga$Var1, levels = rev(VARS_HEAT))
  larga$Var2 <- factor(larga$Var2, levels = VARS_HEAT)
  g4 <- ggplot(larga, aes(Var2, Var1, fill = Freq)) + geom_tile() +
    geom_text(aes(label = sprintf("%.2f", Freq)), size = 2.2) +
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1),
                         name = "Correlación de Spearman\n(adimensional)") +
    labs(x = NULL, y = NULL, title = "Correlación de Spearman entre variables seleccionadas") +
    tema() + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
                   axis.text.y = element_text(size = 7), legend.position = "right")
  ggsave(ruta_res("fig_s5_4_correlacion.png"), g4, width = 8, height = 7, dpi = 150)

  tb <- resumen_binario(df) |>
    pivot_longer(c(proporcion_en_legitimo, proporcion_en_phishing), names_to = "clase", values_to = "p") |>
    mutate(clase = ifelse(clase == "proporcion_en_phishing", "Phishing", "Legítimo"),
           variable = factor(variable, levels = VARS_BIN))
  g5 <- ggplot(tb, aes(variable, 100 * p, fill = clase)) + geom_col(position = "dodge", width = 0.8) +
    scale_fill_manual(values = COLORES, name = NULL) +
    labs(x = NULL, y = "URL con la característica (%)",
         title = "Presencia de características binarias según clase") +
    tema() + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8))
  ggsave(ruta_res("fig_s5_5_binarias_por_clase.png"), g5, width = 8, height = 4.5, dpi = 150)
}

# Flujo de la sección 5: carga la muestra deduplicada, calcula la longitud con nchar(URL), los resúmenes,
# las inferencias I1 a I4, las reglas descriptivas y las figuras; escribe los s5_*.csv.
main <- function() {
  registro("=== Sección 5: exploración e inferencia (muestra deduplicada) ===")
  df <- cargar_datos(deduplicar = TRUE)
  df$longitud_url <- nchar(df$URL, type = "chars")
  registro(sprintf("n = %d | phishing = %d (%.4f)", nrow(df), sum(df[[OBJETIVO]]), mean(df[[OBJETIVO]])))

  res_num <- resumen_numerico(df); res_bin <- resumen_binario(df)
  guardar_csv(res_num, "s5_resumen_numerico_por_clase.csv")
  guardar_csv(res_bin, "s5_resumen_binario_por_clase.csv")

  R <- cor(as.matrix(df[, PREDICTORES]), method = "spearman")
  idx <- which(abs(R) >= 0.9 & upper.tri(R), arr.ind = TRUE)
  pares <- data.frame(variable_1 = PREDICTORES[idx[, 1]], variable_2 = PREDICTORES[idx[, 2]],
                      spearman = round(R[idx], 4))
  pares <- pares[order(match(pares$variable_1, PREDICTORES), match(pares$variable_2, PREDICTORES)), ]
  guardar_csv(pares, "s5_pares_correlacion_altos.csv")
  registro(sprintf("Pares de predictores con |Spearman| >= 0.9: %d", nrow(pares)))

  inferencia <- bind_rows(inf_prevalencia(df), inf_dos_proporciones(df), inf_mann_whitney(df),
                          inf_chi_cuadrada(df))
  guardar_csv(inferencia, "s5_inferencia.csv")
  for (i in seq_len(nrow(inferencia))) {
    f <- inferencia[i, ]
    registro("  ", f$id, ": ", f$prueba)
    registro(sprintf("      estimación=%.6f IC95=[%.6f, %.6f] p=%.3g %s", f$estimacion,
                     f$IC95_inf, f$IC95_sup, f$valor_p, f$decision))
    registro("      supuestos: ", f$supuestos)
  }
  registro("Reglas descriptivas y de formato:")
  reglas <- reglas_deterministas(df)
  figuras(df)
  registro("Figuras y tablas escritas en resultados_R/")
  invisible(list(n = nrow(df), resumen_numerico = res_num, resumen_binario = res_bin,
                 pares = pares, inferencia = inferencia, reglas = reglas))
}

if (sys.nframe() == 0L) main()
