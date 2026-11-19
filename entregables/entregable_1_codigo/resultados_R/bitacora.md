# Bitácora de la traducción a R (PhiUSIIL)

Fuente de verdad del estado del trabajo. Leer antes de retomar. Fechas en hora local (CST).

## Reglas vigentes (del equipo)
- Decisiones metodológicas del primer prompt: no se cambian sin preguntar (clase phishing = 1 − label; deduplicar por URL conservando la primera; excluir FILENAME, URL, Domain, Title, TLD y URLLength; 75/25 estratificado con semilla 2026; CV solo en entrenamiento; VIF ≤ 10 en entrenamiento; logística L2 con glmnet alpha = 0; bosque ranger de 300 árboles; mismos predictores; bootstrap B = 200; umbrales 0.10/0.30/0.50/0.70/0.90; la prueba se usa una vez).
- Detenerse y preguntar solo si: falla una cifra de validación o una comprobación exacta; > 5 % de ajustes o réplicas no convergen en una etapa; el bootstrap estimado supera 60 min; hay que cambiar una decisión metodológica; hay que instalar paquetes, borrar archivos o tocar algo fuera del proyecto.
- No tocar los [completar]. Presentación: preguntar formato. Propuesta .docx: copia previa, listar cambios y esperar visto bueno.
- Pendientes de verificar (decirlo siempre): nombre del docente (solo verificado contra el documento del curso), referencia Prasad y Chandra (2024) y su DOI (verificada en Crossref: Computers & Security 136, 103545, doi 10.1016/j.cose.2023.103545), tamaño 54.2 MiB (UCI muestra "54.2 MB").

## Entorno
- R 4.6.1 (paquetes compilados con 4.6.2: solo avisos), glmnet 5.2, ranger, pROC, car, processx 3.9.0 (ya instalado, se usa para lanzar etapas).
- Equipo: 8 núcleos lógicos, 4 físicos, 23.7 GB de RAM. ranger con num.threads = 4 fijo (deja núcleos libres). Bootstrap con 4 procesos en paralelo (detectCores() − 1, máximo 4).

## Organización del código (codigo_R/)
- comun.R: datos, partición, VIF, métricas, log con marca de hora sin búfer (resultados_R/logs/), modo de prueba rápida (PIA_PRUEBA_RAPIDA=1: submuestra estratificada de 5,000 filas, resultados en resultados_R/prueba_rapida/).
- seccion_4, 5, 7: sin cambios de método. Conteos convertidos a numeric antes de multiplicar.
- seccion_6_modelacion_evaluacion.R: funciones y 19 etapas; cada etapa guarda resultados_R/etapas/<etapa>.rds (escritura atómica).
- ejecutar_s6.R: lanza cada etapa en un proceso de R aparte (processx), escribe el PID en etapas/<etapa>.pid, aplica el límite de tiempo desde fuera y detiene el árbol de procesos por PID; registra cada intento en etapas/estado.csv.
- prueba_rapida.R: smoke test de todas las secciones (2 pliegues, B = 5). Debe pasar antes de una corrida larga.
- pruebas_desbordamiento.R: productos de conteos con n ~ 1e5 por clase.
- comparar_tipo_newton.R: Newton frente a modified.Newton.

## Decisiones tomadas y por qué
1. **Camino de lambdas fino (10 por década, de 1000·λ a λ) en todos los ajustes.** Causa de la divergencia del pliegue 2 con C = 10: con un salto por década el paso de Newton de glmnet (sin búsqueda lineal) sobrepasa el óptimo; glmnet no avisa (jerr = 0) y devuelve dev.ratio = −11.6, intercepto ≈ −1e6. No era maxit (no se alcanzaba), ni thresh, ni columnas constantes (ninguna en el pliegue; |z| máx ≈ 345 en todos). 4 por década sigue divergiendo; 10 por década converge. El camino automático se corta por devmax = 0.999 cientos de veces por encima de λ.
2. **Comprobación de divergencia propia** (sin depender de jerr ni avisos): no convergido si dev.ratio < 0 (devianza peor que la nula), |intercepto| > 100, algún |β| estandarizado > 100, devianza explicada que baja a lo largo del camino, valores no finitos, jerr ≠ 0 o camino incompleto. Cada caso queda en el log y en la tabla de la etapa; consolidar los junta en s6_no_convergidos.csv.
3. **Regla de elección de C** (fijada por el equipo antes de ver la CV fina): el C más pequeño con log-loss medio ≤ mínimo + 1 EE, EE = DE muestral / √5 (como cv.glmnet). Declaración: los valores por pliegue del diagnóstico se vieron antes de escribir la regla. Yo había anticipado que la definición de la DE decidiría entre C = 1 y C = 10 (a partir de valores redondeados); con los valores exactos ambas definiciones dan C = 10. Tabla auditable: s6_regla_1se.csv.
4. **Newton frente a modified.Newton** (s6_comparacion_tipo_newton.csv): objetivo penalizado = −(1/n)·logverosimilitud + (λ/2)·Σβ² (intercepto sin penalizar). Newton con camino fino queda a ≤ 1.3e−6 del óptimo de referencia (Newton, thresh 1e−10); modified.Newton tiene un objetivo 2.5 a 3 veces mayor (no llega al óptimo) y es 6 a 10 veces más lento. Se mantiene Newton con camino fino.
5. **Tolerancia para coeficientes: thresh = 1e−10 en el ajuste final y en el bootstrap.** Con thresh = 1e−7 el objetivo está a ≤ 1.3e−6 del óptimo pero los β difieren hasta 0.94 DE de la referencia (objetivo casi plano por separación y redundancia). CV, predicciones fuera de pliegue y escenarios siguen con 1e−7 (solo usan predicciones). No es un cambio de método: es precisión numérica del mismo estimador.
6. **Bosque: min.bucket ∈ {1, 5} (con min.node.size = 2).** El mensaje del equipo dice "min.node.size en {1, 5}"; se mantiene min.bucket porque es el equivalente de min_samples_leaf de sklearn (la rejilla de Python). En ranger, min.node.size es el tamaño mínimo para dividir un nodo, no el de la hoja. Pendiente de confirmar con el equipo.
7. **Umbral elegido con el entrenamiento** (predicciones fuera de pliegue, 5 pliegues, semilla 2027): rejilla 0.01 a 0.99 cada 0.01 más los cinco umbrales fijos. (a) mínimo costo k·FN + FP por 10,000 URL para k = 1, 5, 10, 50; empates al umbral más bajo. (b) umbral más bajo con ≤ 1 FP por 10,000 legítimas. La prueba se evalúa una vez en los cinco umbrales fijos y en esos umbrales.
8. **Bootstrap:** C = 10 fijo; una semilla por réplica generada con set.seed(2026), así las réplicas no dependen del paralelismo ni de reanudar; punto de guardado cada 25 réplicas (etapas/bootstrap_parcial.rds, se reanuda solo si coinciden B, C, n, thresh, semilla y columnas). Los IC usan las réplicas convergidas y se reporta cuántas no convergieron.
9. **Influencia:** principal = reajuste sin |r de Pearson| > 3 y sin las 10 mayores Cook, contra 10 controles al azar del mismo tamaño (semillas 2027 a 2036) con su rango; secundario = sombrero ponderado del ajuste penalizado. Validación de la aproximación de un paso contra 5 reajustes reales (debe correlacionar positivamente, ≈ 0.96); se presenta como aproximación, no como cálculo exacto. Tolerancia 1e−12.
10. **Escenario de formato comparable:** se quitan, en su entrenamiento, columnas constantes y casi constantes (> 99.9 % con el mismo valor). Se eliminan en vez de ajustarlas sin estandarizar: sin estandarizar la penalización L2 recae en la escala original (con DE 0.004 la penalización efectiva sería ~1/0.004² veces mayor), lo que las anula de forma opaca; además estandarizadas producen |z| ~ 300, la condición en la que diverge el paso de Newton. Ambos modelos usan las mismas columnas. C ya elegido, sin CV, límite de 5 min. Si no converge: limitación no resuelta, se muestra solo el bosque.
11. **Escenario de razones con nchar(URL):** ObfuscationRatio, LetterRatioInURL y DegitRatioInURL = conteo / nchar(URL), redondeadas a 3 decimales como en el archivo; el modelo principal no cambia.

## Registro de etapas
### 2026-11-18 — Prueba rápida
- Primera corrida: falló el lanzador (escribía el PID antes de crear etapas/). Corregido. Segunda corrida: todo ok en 3.5 min; la comprobación marcó como no convergido el ajuste logístico del escenario de formato (jerr = −1, maxit agotado), lo que muestra que la comprobación funciona.

### 2026-11-18 21:40 — preparar y cv_logistica (datos completos)
- Partición 176,527 / 58,843; prevalencia 0.4271 en ambas. VIF elimina DomainTitleMatchScore (17.08; Python 16.96). |VIF propio − car::vif| = 3.27e−12.
- CV logística, 5 pliegues, camino fino, thresh 1e−7: 0 de 20 ajustes no convergidos. Log-loss medio: C = 0.01: 0.004296; 0.1: 0.001234; 1: 0.000486; 10: 0.000349 (EE 0.000134). Umbral 0.000483; C = 1 queda 3.05e−6 por encima → **C = 10**. Con DE poblacional, umbral 0.000469 → también C = 10. Python: C = 10 por mínimo (log-loss 0.00048; C = 1: 0.00055).
- Pliegue 2 con C = 10: log-loss 0.000606 (antes 14.30 con el camino grueso).
- Tiempo: 8 s y 150 s.

### 2026-11-18 21:55 — Verificaciones previas a la etapa 3
- s6_regla_1se.csv: DE muestral → umbral 0.000483059, C = 1 queda +3.05e−6 por encima; DE poblacional → umbral 0.000468956, C = 1 queda +1.72e−5 por encima. Ambas: C = 10.
- Objetivo penalizado (s6_comparacion_tipo_newton.csv), diferencia frente a la referencia (Newton, thresh 1e−10): Newton 1e−7: 7.0e−7, 1.2e−6, 5.0e−8; modified.Newton: 3.9e−4, 4.0e−4, 1.9e−4 (objetivo 2.5 a 3 veces mayor). Se mantiene Newton con camino fino. Ver decisión 5 (thresh 1e−10 para coeficientes).

### 2026-11-18 22:08–22:17 — Etapa 3 (separacion, cv_bosque, umbrales_oof, ajuste_final) y medición del bootstrap
- Ajustes no convergidos: 0 en todas las etapas.
- separacion (104 s): glm sin penalizar no converge; log-verosimilitud −795, −49.8, −4469, −2703 con 5/10/25/50 iteraciones, max|β| hasta 7.4e14, las 176,527 filas con p extrema desde 25 iteraciones. glmnet: max|β| 2.63, 4.82, 8.15, 12.77 para C = 0.01, 0.1, 1, 10.
- cv_bosque (20 s, 3 pliegues, submuestra 30,000, semilla 2026, num.threads 4): profundidad 20, min.bucket 1 (log-loss 0.002605). Igual que Python (20 y 1; log-loss 0.00300).
- umbrales_oof (191 s, 5 pliegues, semilla 2027): umbral elegido logística: k = 1 → 0.15; k = 5 y 10 → 0.11; k = 50 → 0.02; ≤ 1 FP/10,000 → 0.10. Bosque: 0.27 para todos los k (costo 0 en OOF); ≤ 1 FP/10,000 → 0.17.
- ajuste_final (76 s; logística C = 10, thresh 1e−10, max|β| 13.55): prueba evaluada una vez. AUC logística 0.99999986 (DeLong 0.9999996 a 1), AP 0.9999998, log-loss 0.000223, Brier 4.27e−5. Bosque: AUC 1, log-loss 0.000807, Brier 3.85e−5. Regla: AUC 0.99982, FP 0, FN 9.
- FP/FN en prueba (logística | bosque): 0.10: 3/1 | 8/0; 0.30: 0/2 | 0/0; 0.50: 0/4 | 0/0; 0.70: 0/4 | 0/3; 0.90: 0/6 | 0/21. Python: 1/2 | 12/0; 0/3 | 0/0; 0/4 | 0/1; 0/5 | 0/5; 0/11 | 0/16.
- Prueba en umbrales elegidos con entrenamiento: logística 0.15 → 1 FP, 1 FN; 0.11 → 1/1; 0.02 → 37/1; 0.10 → 3/1. Bosque 0.27 → 0/0; 0.17 → 0/0.
- bootstrap_prueba (5 réplicas, 4 procesos, thresh 1e−10): 84 s por ajuste en paralelo (31 s en serie); estimación 70 min para 200 → **supera 60 min: se detiene y se pregunta al equipo**. Mediciones adicionales con 8 réplicas: thresh 1e−7 → ~31 min; 1e−9 → ~65 min. 0 réplicas no convergidas en las tres mediciones.
- Procesos de R huérfanos: ninguno.

### 2026-11-18 — Decisiones del equipo tras la etapa 3
- Bootstrap con thresh = 1e−10. B = 200 si la estimación total es ≤ 75 min; si no, B = 100 y el informe declara B y que los IC con pocas réplicas son menos estables. Antes, medir 3 procesos frente a 4 con 5 réplicas y usar el de mejor tiempo total. Puntos de guardado cada 25. El bootstrap se corre al final (después de influencia y escenarios); mientras tanto el informe marca las razones de momios como pendientes.
- Aceptado min.bucket ∈ {1, 5}. Verificado en la documentación instalada de ranger 0.18.0 (?ranger, sección Arguments y Details): min.node.size es el tamaño mínimo de nodo para dividir; min.bucket es el tamaño mínimo de nodo terminal; los Details aclaran que pueden quedar nodos menores que min.node.size y que para restringir las hojas se usa min.bucket. En bosques de probabilidad min.node.size vale 10 por omisión; aquí se fija en 2 (= min_samples_split de sklearn).
- Umbrales por costo: no se presentan como precisos. Se reporta el rango con costo mínimo (empate exacto) y con costo casi mínimo, definido como costo total en conteos (k·FN + FP) ≤ mínimo + k, es decir, a lo sumo un ataque omitido adicional (el error individual más caro). Para la restricción de FP se reporta el rango de umbrales que la cumplen.
- Influencia sin depender del bootstrap: la columna "cambio máximo en DE bootstrap" se completa en consolidar cuando el bootstrap termine.

### 2026-11-18 22:29–23:01 — umbrales_oof (rehecha con rangos), etapa 5 (influencia) y etapa 6 (escenarios)
- Ajustes no convergidos: 0 en todas estas etapas. Una etapa con tiempo agotado: sens_formato_logistica.
- umbrales_oof (206 s): mismos umbrales que antes. Rangos de costo mínimo / casi mínimo: bosque 0.27–0.45 / 0.24–0.49 (k = 1), 0.17–0.49 (k = 10), 0.12–0.49 (k = 50); logística k = 1: 0.15 / 0.14–0.16; k = 10: 0.11–0.12 / 0.08–0.13; k = 50: 0.02 / 0.02–0.05. La curva del bosque es plana; la de la logística es más estrecha.
- influencia (957 s, tolerancia 1e−12, 10 controles al azar, semillas 2027–2036): solo 2 observaciones con |r de Pearson| > 3 (ambas phishing; Python tenía 9 con otra partición). Quitarlas cambia β hasta 4.69 (LargestLineLength), 4 cambios de signo y una predicción de prueba cambia hasta 1.00 en probabilidad; quitar las 10 mayores Cook: Δβ 4.80, 6 cambios de signo. Controles al azar: Δβ máx entre 1.9e−7 y 1.3e−4, 0 cambios de signo. Aproximación de un paso: correlación con reajustes reales entre 0.871 y 0.999 (positiva). Sombrero: ver s6_influencia_sombrero.csv. La columna en DE bootstrap se completa en consolidar.
- Escenarios (FP/FN a 0.50, logística | bosque; AUC logística / bosque):
  - sin similitud (47): 1/6 | 0/6; 0.99999 / 1.00000
  - léxicas (20): 3/187 | 11/154; 0.99860 / 0.99891
  - HTML (27): 198/154 | 27/102; 0.99973 / 0.99992
  - 16 recalculadas desde la URL: 12/375 | 10/241; 0.99577 / 0.99771
  - dominio (0 dominios compartidos; prueba 60,294): 0/0 | 0/0; 1 / 1
  - razones con nchar(URL) (48): 0/3 | 0/0; 1 / 1 (cambian 0.04 % de ObfuscationRatio, 79.4 % de LetterRatioInURL y 9.0 % de DegitRatioInURL). Principal: 0/4 | 0/0 → sin ventaja artificial apreciable.
  - sin TLDLegitimateProb (47): 0/2 | 0/0; 1 / 1
  - formato comparable (40 columnas tras quitar 2 constantes y 6 casi constantes; prueba 33,956 con 243 phishing): bosque 0/0, AUC 1. Logística: **tiempo agotado a los 5 min** (limitación no resuelta). Causa sospechada (no verificada): solo 729 phishing en 101,866 filas de entrenamiento, con separación casi completa; casi todos los pesos p(1 − p) son ~0 y el descenso por coordenadas de glmnet avanza muy despacio con λ = 1/(10·101,866). En la prueba rápida el mismo ajuste terminó con jerr = −1 (maxit agotado en el primer λ).
- Decisión: en consolidar, una fila logística no convergida queda sin métricas (NA) y con su nota.
- Procesos de R huérfanos tras el lote: ninguno.

### 2026-11-18 23:02 — Medición del bootstrap y lanzamiento
- 3 procesos: 67.3 s por ajuste → 74.8 min para 200; 4 procesos: 84 s por ajuste → 70.0 min. Se usan 4 procesos (mejor tiempo total) y **B = 200** (≤ 75 min). 0 réplicas no convergidas en ambas mediciones.
- Secciones 7, 4 y 5 vueltas a correr con el código actual: todas las cifras de validación coinciden (SHA, conteos, I1 a I4); pandoc 3.14 detectado.

### 2026-11-18 23:06–23:53 — Bootstrap cerrado con B = 75 y consolidar
- Decisión del equipo: simplificar al mínimo del curso y cerrar el bootstrap en el primer punto de guardado ≥ 100. Después, ante el retraso, el equipo decidió cerrar con las 75 réplicas del punto de guardado sin esperar al de 100.
- Por qué no se esperó al punto de 100: el bloque 4 (réplicas 76 a 100) llevaba más de 18 min (los anteriores tardaron 8 a 10). Tres de los cuatro procesos estaban ociosos y uno seguía con una réplica muy lenta. parLapply reparte las réplicas por adelantado, así que un proceso lento retiene a todo el bloque, y no había forma de saber cuánto faltaba.
- Detención (23:52): el último punto de guardado se había escrito a las 23:33:48 y el bloque 4 seguía en curso, así que no había escritura en marcha. Se detuvieron por PID exacto: lanzador 3392 (para que no lanzara consolidar sin bootstrap), su supervisor 13824, coordinador 2860, procesos hijos 9604, 14172, 2664 y 2352, y conhost 25044. No se tocó ningún otro proceso de R; un Rscript ajeno (9468) ya había terminado por su cuenta.
- Copia: etapas/bootstrap_parcial_75.rds (idéntica al punto de guardado). Cierre con PIA_BOOTSTRAP_DESDE_PARCIAL=1: se usan las filas con réplica hecha y el mismo código de resumen, sin cambiar semillas.
- Resultado: **B = 75 réplicas hechas, 1 no convergida (réplica 56: dev.ratio −3.93, intercepto 1.9e4; divergencia silenciosa detectada por la comprobación propia), 74 usadas en los IC.** Tiempo: 46 min de cómputo (23:06–23:52) más 35 s del cierre; consolidar terminó sin errores en 2 s. 1 de 75 = 1.3 % < 5 %.
- Grupos redundantes: G2 (letras) y G3 (dígitos) aumentan la probabilidad de phishing; G1 (ofuscación) y G4 (= y ?) con dirección no determinada (IC incluye 0).
- consolidar y el informe leen el B real de s6_bootstrap_convergencia.csv (columnas B, B_planeado, replicas_convergidas, replicas_no_convergidas).
- **Mejora sugerida (no aplicada):** usar parallel::parLapplyLB en lugar de parLapply, que reparte las réplicas de forma dinámica; así una réplica lenta no retiene a los demás procesos.
- Comparación Python–R (comparacion_python_R.csv): 122 cifras; 40 coinciden exactamente (todas las exactas), 78 parecidas, 4 discrepan (todas aproximadas: escenario sin similitud logística FP 1 frente a 14 y FN 6 frente a 18; solo URL bosque FP 10 frente a 35; formato comparable logística sin resultado en R).

### 2026-11-18 — Simplificación del informe y del resumen (pedido del equipo)
- Copias: informe_antes_de_simplificar.Rmd, resumen_ejecutivo_antes_de_simplificar.Rmd, codigo_R/seccion_6_modelacion_evaluacion_antes_de_simplificar.R.
- El informe deja: I1 a I4, los dos modelos, matriz de confusión con F1 en cinco umbrales (más la regla base), métricas globales, razones de momios con IC y grupos, separabilidad, VIF e influencia (solo reajuste sin las marcadas). Sensibilidad: solo sin índice de similitud y partición por dominio; las demás y la comparación con Python van al anexo en tabla, sin comentario. Se quitaron del informe (quedan aquí): tolerancias de glmnet, camino de lambdas, tipos de Newton, tablas de costo con varias razones, rangos de umbral, controles al azar, aproximación de un paso y apalancamiento.
- Anexo en archivo hijo informe_anexo.Rmd (parámetro anexo = TRUE/FALSE) para medir las páginas sin anexo.
- Compilación: sin errores ni avisos de R. informe: 18 páginas con anexo y **11 sin anexo** (impresión carta con Edge), una menos que el mínimo de 12 del curso. Resumen ejecutivo: 1 página.
- Guía de estudio: resultados_R/guia_de_estudio.md.
- Procesos de R tras la compilación: ninguno.

### 2026-11-19 — Corrección en el informe y lista de cambios para la propuesta
- Error corregido: para las variables binarias (IsHTTPS, HasSocialNet…) el informe mostraba la razón de momios por desviación estándar aunque el texto decía «por presencia». Ahora usa OR_por_unidad (cambio de 0 a 1) en binarias y OR_por_DE en continuas. Ejemplo: IsHTTPS pasa de 0.0046 (por DE) a 2.1e−06 (por presencia); HasSocialNet, de 0.12 a 0.014. La dirección no cambia. Recompilado: sin errores ni avisos; 18 páginas con anexo y 11 sin anexo.
- Lista de cambios para la propuesta en resultados_R/cambios_propuesta.md (pendiente de visto bueno). Diferencia notable con Python: NoOfJS tiene dirección no determinada en R (IC 0.018 a 1.13); en Python era 0.134 (0.094 a 0.215).

### 2026-11-19 00:00–00:10 — Propuesta .docx actualizada e informe con diccionario
- Copia previa: Propuesta_de_Proyecto_completa_antes_de_R.docx (idéntica al original). Edición del XML con Python estándar (los scripts de la guía de docx necesitan defusedxml y lxml, que no están instalados y no se instalaron): solo cambia el texto de las corridas afectadas, se conservan estilos.
- Se aplicaron los 37 cambios de cambios_propuesta.md más los ajustes del equipo: 6.6 (el bosque omite menos en 0.30, 0.50 y 0.70: 0/2, 0/4 y 3/4; sin «empata»), 6.4 (2 frases tras la tabla: diferencias con Python y NoOfJS no determinada), 6.5 (3 filas, nota de 2 líneas sobre el escenario de formato, se quitaron los dos puntos de escenarios eliminados), B6 con la fuente resultados_R/s6_influencia_detalle.csv.
- Errores propios detectados al revisar el PDF y corregidos antes de entregar: (1) en la tabla de umbrales la sensibilidad y la especificidad salían intercambiadas en algunas filas porque el llenado reemplazaba la primera celda con XML idéntico; ahora se llena por posición y se comprobó celda a celda contra el CSV (0 discrepancias); (2) las filas de glmnet en la tabla de separabilidad salían como «Sin penalizar»; (3) encabezados cortados a media palabra (anchos ajustados).
- Validación: XML bien formado; Word 16 abre el archivo sin errores (19 páginas, 20 tablas); PDF exportado (Propuesta_de_Proyecto_completa.pdf) y páginas con tablas revisadas. [completar]: 11 antes y 11 después.
- No cambiado (fuera de la lista aprobada; se informa al equipo): figuras 3 y 4 siguen siendo las de Python (la figura 4 muestra 12 FP y 16 FN, distinto del texto nuevo); 6.2 dice que «el informe final eliminará redundancias por correlación de rangos» (en R se agrupan, no se eliminan); la «Advertencia» de 6.4 promete contrastar con razones univariadas.
- Informe: el diccionario de variables (columna, rol, grupo, incluida, motivo) pasa a la sección 3.2 y sale del anexo; anexos reetiquetados A (otros escenarios), B (comparación con Python), C (reproducibilidad). Compilación sin errores ni avisos: **14 páginas sin anexo** y 17 con anexo; resumen ejecutivo 1 página.
- Procesos de R o Word abiertos al terminar: ninguno.

### 2026-11-19 00:10–00:20 — Figuras de R en la propuesta y dos oraciones corregidas
- Figuras reemplazadas por las de R (mismo ancho; alto según la proporción de cada imagen): figura 1 = fig_s5_2_longitud_url.png; figura 2 = fig_s5_3_similitud_por_clase.png; figura 3 = fig_s6_1a_curva_roc.png y fig_s6_1b_curva_roc_zoom.png lado a lado en el mismo párrafo (3.15 pulgadas cada una); figura 4 = fig_s6_2_errores_por_umbral.png.
- Pies comprobados contra los CSV: figura 1 (cuartiles de legítimas 24 a 30, 72.1 %: s5_resumen_numerico_por_clase.csv y s5_inferencia.csv) y figura 2 (mediana del phishing 51.48 → 51.5; sin valores atípicos, como dice el título de la figura de R) siguen siendo ciertos y no cambian. Figura 3: se quitó «el bosque tiene un punto de operación ligeramente mejor»; ahora dice que ambas curvas quedan pegadas a la esquina y se superponen en el detalle, y que el AUC no las distingue. Figura 4: «Entre 0.30 y 0.50 el bosque no comete errores y la logística omite de 2 a 4 ataques, sin falsas alarmas…; a 0.10: bosque 8, logística 3; a 0.90: bosque 21, logística 6» (s6_matriz_confusion_prueba.csv).
- Oraciones desactualizadas: (1) sección 6.2, «Por eso el informe final eliminará redundancias por correlación de rangos además del VIF.» Frente a s6_redundancia_aristas.csv, s6_or_grupos_redundantes.csv y s6_vif_final.csv: se formaron 4 grupos (G1 a G4) y sus 9 variables siguen entre los 48 predictores; no se eliminaron, se interpretan en grupo. Nueva: «Por eso las variables con correlación de rangos de 0.9 o más con otra, o derivadas de otra, no se eliminan: se interpretan en grupo (sección 6.4; resultados_R/s6_or_grupos_redundantes.csv).» (2) sección 6.4, «Advertencia…; el informe final las contrastará con razones univariadas y con validación externa.» No hay archivo de razones univariadas en resultados_R (solo la de HasSocialNet en I4, s5_inferencia.csv) y no hay validación externa. Nueva: «…; la única razón univariada disponible es la de los enlaces a redes sociales (I4, sección 5.2), y la validación externa queda pendiente (sección 6.6).» La nota de 6.4 con las correlaciones 0.93 y 0.997 coincide con s6_redundancia_aristas.csv (0.9305 y 0.9968).
- Validación: XML bien formado; Word abre el archivo sin errores (19 páginas, 5 imágenes en línea); cada imagen está en la misma página que su pie (páginas 7, 8, 11, 11 y 12; las imágenes y los títulos de pie tienen «conservar con el siguiente»). PDF: Propuesta_de_Proyecto_completa.pdf. [completar]: 11. La tabla de razones de momios de 6.4 queda partida entre las páginas 12 y 13 (última fila); no afecta a figuras.
- Procesos de Word o R abiertos al terminar: ninguno.

### 2026-11-19 00:15–00:30 — Cierre
- README.md reemplazado por el de la versión en R (orden de ejecución, etapas con tiempos medidos, paquetes, cómo se obtuvo B = 75 y cómo reproducirlo o completar las 200). El anterior quedó en README_python.md; README_R.md queda como nota que remite a README.md.
- Propuesta: tablas de razones de momios (6.4) y de sensibilidad (6.5) con «conservar con el siguiente» en todas sus filas: quedan enteras y junto a su fuente. Líneas «Fuente» de las secciones 4 y 5 → resultados_R/. Validada en Word (19 páginas, 20 tablas, 5 imágenes, cada imagen con su pie en la misma página) y PDF revisado.
- Auditoría de consistencia (informe.Rmd, informe_anexo.Rmd, resumen_ejecutivo.Rmd, propuesta, README.md, README_R.md, bitácora). Corregido: en la propuesta, 5 líneas «Fuente: resultados/», 5 nombres .py en la tabla de etapas (sección 5), «carpeta codigo/» en la introducción de la sección 4, 3 referencias a README_R.md → README.md y la referencia de scikit-learn (Pedregosa, de la versión en Python, sin citas en el texto) sustituida por Friedman et al. (2010) y Wright y Ziegler (2017), tomadas de citation() en R; en informe_anexo.Rmd, README_R.md → README.md.
- Compilación final: informe y resumen sin errores ni avisos de R; informe 14 páginas sin anexo (17 con anexo); resumen 1 página.
- estructura_actual.md creado en la raíz del proyecto.

### Sin corregir, con motivo
- Bitácora: las entradas anteriores conservan «B = 200», «C = 1», cifras de Python y la carpeta resultados/ porque son el registro histórico de lo que se hizo en cada momento; las cifras vigentes son las de esta sección y de «Estado actual».
- README.md menciona «B = 200»: es correcto, es el valor por omisión del código; el texto explica que el resultado entregado usa B = 75.
- Informe y propuesta mencionan «Python» solo para la comparación de cifras (anexo B del informe, comparación de razones de momios en 6.4 de la propuesta): es intencional.
- resultados_R/guia_de_estudio.md dice «las 100 del primer punto de guardado» y «¿100 réplicas son suficientes?»; el valor real es B = 75 (74 usadas). No se corrigió porque el equipo pidió no trabajar en la guía y no estaba en la lista de la auditoría.
- Propuesta: tres tablas largas se reparten entre dos páginas (grupos de variables en 4.3, características binarias en 5.1 y rúbrica en 9); ninguna fila se corta. En el original también había tablas largas repartidas entre páginas.
- comparacion_python_R.csv contiene cifras de Python por diseño (es la comparación).
- informe.pdf en la raíz es una copia generada para contar páginas; no es un entregable del curso.

## Estado actual
- Validación: todas las cifras exactas coinciden con las de Python (40 de 40: conteos, SHA-256, deduplicación, I1 a I4, regla de dos condiciones, VIF eliminado); 78 cifras aproximadas parecidas y 4 que discrepan (escenarios; ver comparacion_python_R.csv).
- Modelos: logística L2 con C = 10 (regla de 1 error estándar) y bosque de 300 árboles (profundidad 20, hoja 1). Prueba evaluada una vez: logística 0 FP / 4 FN y bosque 0 / 0 con umbral 0.50; regla base 0 / 9.
- Bootstrap: B = 75 (74 usadas, 1 no convergida), C = 10, thresh 1e−10; punto de guardado en etapas/bootstrap_parcial_75.rds.
- Escenario de formato comparable: la logística no terminó en 5 minutos (limitación no resuelta; solo bosque).
- Entregables: informe.Rmd/informe.html (14 páginas sin anexo), resumen_ejecutivo.pdf (1 página), Propuesta_de_Proyecto_completa.docx (+ PDF; original en _antes_de_R.docx), README.md, código en codigo_R/, resultados en resultados_R/.
- Procesos de R: ninguno en ejecución.

## Lo que queda para el equipo
- Llenar los [completar] (grupo, fecha, enlace al repositorio, integrantes) y la tabla de contribuciones.
- Presentación (no se generó, por decisión del equipo).
- Verificar el nombre del docente (solo se contrastó con el documento del curso).
- Revisar a mano las 2 observaciones influyentes (resultados_R/s6_influencia_detalle.csv).
- Decidir si se completa el bootstrap a 200 réplicas (ver README.md; puede pasar de 2 horas).
- Corregir en guia_de_estudio.md la mención a 100 réplicas (el valor real es 75, con 74 usadas).
