# Sección 4 de la propuesta (producto §2): caso de estudio y conjunto de datos.
#
# Audita el archivo tal como se recibió (antes de deduplicar): codificación de la
# clase, duplicados, inconsistencias, escalas, riesgo de fuga e inclusión o
# exclusión de variables. No ajusta modelos ni usa la partición de prueba.

source(if (dir.exists("codigo_R")) "codigo_R/comun.R" else "comun.R", encoding = "UTF-8")

MOJIBAKE <- "Ã|à¸|à¹|â€|Ð|Ñ"
NOTA_COLUMNAS <- paste("UCI reporta 54 variables; el archivo tiene 56 columnas porque también",
                       "incluye FILENAME (nombre del archivo de origen) y label (la respuesta)")
NOTAS <- c(
  FILENAME = paste("columna adicional a las 54 variables de UCI;", NOTA_COLUMNAS),
  label = paste("columna adicional a las 54 variables de UCI;", NOTA_COLUMNAS),
  URLLength = "desfase respecto a nchar(URL) ligado a la clase: 100 % de legítimas con -1; excluida del modelo",
  URLSimilarityIndex = "escala 0 a 100; el predictor con mayor poder univariado",
  DomainTitleMatchScore = "escala 0 a 100; correlacionada con URLTitleMatchScore",
  URLTitleMatchScore = "escala 0 a 100",
  IsDomainIP = "toda URL con IP como dominio es phishing en esta muestra",
  TLDLegitimateProb = "constante dentro de cada TLD; su origen exacto no está documentado",
  HasObfuscation = "idéntica a NoOfObfuscatedChar > 0 (redundante)",
  ObfuscationRatio = "derivada de NoOfObfuscatedChar y la longitud de la URL"
)

# Clasifica una variable en su grupo (léxica de la URL, de la página HTML o similitud)
# para el diccionario de variables.
grupo_de <- function(v) {
  if (v %in% LEXICAS_URL) return("léxica (URL)")
  if (v %in% DE_PAGINA) return("página (HTML)")
  if (v %in% SIMILITUD) return("similitud")
  "no aplica"
}

# Rasgos de formato de la URL que se usan en las secciones 4 a 6.
formato_url <- function(u) {
  data.frame(
    con_www = str_detect(u, fixed("www.")),
    https = startsWith(u, "https://"),
    con_ruta_mas_de_3_barras = str_count(u, fixed("/")) > 3,
    termina_en_barra = endsWith(u, "/")
  )
}

# Auditoría del archivo tal como se recibió: SHA-256, conteos, faltantes, duplicados, escalas,
# formato de la URL por clase y diccionario de variables (qué se incluye o excluye y por qué).
# Escribe s4_control_calidad.csv, s4_formato_por_clase.csv y s4_diccionario_variables.csv.
main <- function() {
  registro("=== Sección 4: caso de estudio y calidad de datos ===")
  registro("Archivo: ", basename(RUTA_CSV))
  registro("SHA-256: ", sha256_archivo())
  df <- cargar_datos(deduplicar = FALSE)
  n <- nrow(df)
  control <- data.frame(concepto = character(0), valor = character(0))
  reg <- function(concepto, valor) {
    txt <- if (length(valor) > 1 || is.list(valor))
      paste(names(valor), unlist(valor), sep = ": ", collapse = "; ") else as.character(valor)
    control[nrow(control) + 1, ] <<- c(concepto, txt)
    registro("  ", concepto, ": ", txt)
  }

  y <- df[[OBJETIVO]]
  reg("filas en el archivo", n)
  reg("columnas en el archivo", ncol(df) - 1)  # se resta la columna derivada 'phishing'
  reg("variables según UCI", 54)
  reg("nota sobre el número de columnas", NOTA_COLUMNAS)
  reg("label = 1 (legítimo)", sum(df[[ETIQUETA_ORIGINAL]] == 1))
  reg("label = 0 (phishing)", sum(df[[ETIQUETA_ORIGINAL]] == 0))
  reg("prevalencia de phishing (sin depurar)", round(mean(y), 4))
  reg("valores faltantes en todo el archivo", sum(is.na(df)))
  reg("filas duplicadas exactas", sum(duplicated(df[, setdiff(names(df), OBJETIVO)])))

  rep_mask <- duplicated(df$URL) | duplicated(df$URL, fromLast = TRUE)
  rep <- df[rep_mask, ]
  reg("filas con URL repetida", nrow(rep))
  reg("URL distintas repetidas", length(unique(rep$URL)))
  contradictorias <- rep |> group_by(URL) |> summarise(k = n_distinct(label)) |> filter(k > 1)
  reg("URL repetidas con etiquetas contradictorias", nrow(contradictorias))
  elim <- df[duplicated(df$URL), ]
  reg("filas que se eliminan al deduplicar", nrow(elim))
  tab_elim <- table(factor(elim[[OBJETIVO]], levels = c(1, 0)))
  reg("clase de las filas eliminadas (phishing=1)", as.list(tab_elim[tab_elim > 0]))
  reg("filas tras deduplicar", n - nrow(elim))

  dif <- df$URLLength - nchar(df$URL, type = "chars")
  reg("URLLength - nchar(URL) = -1", sum(dif == -1))
  reg("URLLength - nchar(URL) = 0", sum(dif == 0))
  reg("URLLength - nchar(URL) con otros valores", sum(dif != -1 & dif != 0))
  reg("legítimas con URLLength - nchar(URL) = -1", sum(dif == -1 & y == 0))
  reg("phishing con URLLength - nchar(URL) = 0", sum(dif == 0 & y == 1))

  for (v in c("URLSimilarityIndex", "DomainTitleMatchScore", "URLTitleMatchScore")) {
    reg(paste("rango de", v), sprintf("%.4f a %.4f", min(df[[v]]), max(df[[v]])))
  }

  titulo <- df$Title
  reg("Title con patrón de mojibake", sum(str_detect(titulo, MOJIBAKE), na.rm = TRUE))
  reg("Title con entidades HTML (&#)", sum(str_detect(titulo, fixed("&#")), na.rm = TRUE))
  reg("faltantes en Title", sum(is.na(titulo)))

  reg("niveles de TLD", length(unique(df$TLD)))
  reg("dominios distintos (Domain)", length(unique(df$Domain)))

  por_tld <- df |> group_by(TLD) |>
    summarise(prop_legitimo = mean(label), tldprob = first(TLDLegitimateProb),
              n_valores = n_distinct(TLDLegitimateProb), .groups = "drop")
  reg("TLD con más de un valor de TLDLegitimateProb", sum(por_tld$n_valores > 1))
  reg("correlación TLDLegitimateProb vs proporción legítima observada por TLD",
      round(cor(por_tld$prop_legitimo, por_tld$tldprob), 4))
  reg("mediana de |TLDLegitimateProb - proporción legítima observada|",
      round(median(abs(por_tld$prop_legitimo - por_tld$tldprob)), 4))

  reg("URL con IP como dominio: phishing", sum(df$IsDomainIP == 1 & y == 1))
  reg("URL con IP como dominio: legítimo", sum(df$IsDomainIP == 1 & y == 0))
  reg("HasObfuscation == (NoOfObfuscatedChar > 0)",
      all(df$HasObfuscation == as.integer(df$NoOfObfuscatedChar > 0)))
  constantes <- PREDICTORES[vapply(PREDICTORES, function(v) length(unique(df[[v]])) <= 1, logical(1))]
  reg("predictores constantes", if (length(constantes)) paste(constantes, collapse = ", ") else "ninguno")

  fmt <- formato_url(df$URL)
  fmt$URLLength_igual_nchar_URL_menos_1 <- dif == -1
  fmt$IsHTTPS_igual_1 <- df$IsHTTPS == 1
  tabla_formato <- data.frame(
    rasgo = names(fmt),
    legitimo = vapply(fmt, function(z) mean(z[y == 0]), numeric(1)),
    phishing = vapply(fmt, function(z) mean(z[y == 1]), numeric(1))
  )
  guardar_csv(tabla_formato, "s4_formato_por_clase.csv")
  registro("Rasgos de formato de la URL por clase (proporciones):")
  print(transform(tabla_formato, legitimo = round(legitimo, 4), phishing = round(phishing, 4)),
        row.names = FALSE)

  registros <- lapply(setdiff(names(df), OBJETIVO), function(v) {
    s <- df[[v]]
    rd <- if (v %in% IDENTIFICADORES) c("identificador", "excluir")
      else if (v %in% TEXTO_LIBRE) c("texto libre", "excluir")
      else if (v %in% ALTA_CARDINALIDAD) c("categórica de alta cardinalidad", "excluir")
      else if (v == ETIQUETA_ORIGINAL) c("respuesta original", "base para phishing = 1 - label")
      else if (v == "URLLength") c("predictor con artefacto", "excluir (desfase ligado a la clase)")
      else c("predictor candidato", "candidato (filtro VIF en sección 6)")
    es_num <- is.numeric(s)
    binaria <- es_num && all(unique(s) %in% c(0, 1))
    data.frame(
      variable = v, rol = rd[1], grupo = grupo_de(v),
      tipo = if (binaria) "binaria 0/1" else if (es_num) "numérica" else "texto o categórica",
      niveles_distintos = length(unique(s)), faltantes = sum(is.na(s)),
      minimo = if (es_num) format(min(s)) else "", mediana = if (es_num) format(median(s)) else "",
      maximo = if (es_num) format(max(s)) else "", decision = rd[2],
      nota = if (v %in% names(NOTAS)) NOTAS[[v]] else ""
    )
  })
  diccionario <- do.call(rbind, registros)
  guardar_csv(diccionario, "s4_diccionario_variables.csv")
  guardar_csv(control, "s4_control_calidad.csv")
  registro("Escrito: resultados_R/s4_diccionario_variables.csv y s4_control_calidad.csv")
  invisible(list(control = control, diccionario = diccionario, formato = tabla_formato))
}

if (sys.nframe() == 0L) main()
