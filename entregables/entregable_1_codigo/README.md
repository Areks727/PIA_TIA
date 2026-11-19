# Auditoría estadística de detección de phishing (PhiUSIIL): versión en R

> **Entregable 1 · Código fuente.** Esta carpeta contiene todo lo necesario para reproducir el análisis: `codigo_R/` (scripts comentados por sección), `informe.Rmd`, `informe_anexo.Rmd` y `resumen_ejecutivo.Rmd` (que leen sus cifras de `resultados_R/`), los resultados de la ejecución de referencia en `resultados_R/` y `data/` (instrucciones para descargar el conjunto de datos). Los scripts llevan comentarios que explican qué hace cada función principal.
> Equipo: Alejandro Martinez Moya, David Emiliano Rangel Tovar y Fernando Garza Chavez. Docente: Dr. Álvaro E. Cordero-Franco.

Este proyecto audita la detección de phishing con el conjunto PhiUSIIL Phishing URL (UCI, licencia CC BY 4.0). El análisis está en R:

- `informe.Rmd` compila a `informe.html` (informe final; el anexo está en `informe_anexo.Rmd`).
- `resumen_ejecutivo.Rmd` compila a `resumen_ejecutivo.html`, que se pasa a `resumen_ejecutivo.pdf`.

Todas las cifras del informe y del resumen se leen de `resultados_R/`; ninguna se escribe a mano. La versión anterior en Python se describe en `README_python.md`.

## Requisitos

- **R y paquetes:** R 4.6.1 con tidyverse, rmarkdown, knitr, glmnet, ranger, pROC, car, openssl y processx. Las versiones exactas quedan en `resultados_R/s7_entorno.txt`.
- **Pandoc,** que lo usa rmarkdown. Si no está en el PATH, `asegurar_pandoc()` lo busca en `%LOCALAPPDATA%/Pandoc`.
- **Edge o Chrome,** solo para convertir el resumen a PDF (`codigo_R/a_pdf.R`).
- **Equipo de referencia:** 8 núcleos lógicos (4 físicos) y 24 GB de RAM. ranger usa 4 hilos fijos y el bootstrap usa 4 procesos en paralelo.

## Datos

1. Descargar el conjunto desde https://archive.ics.uci.edu/dataset/967/phiusiil+phishing+url+dataset.
2. Colocar `PhiUSIIL_Phishing_URL_Dataset.csv` en `data/` (o en la carpeta del proyecto).
3. La sección 7 verifica el SHA-256 `a236549cd369cd80bd478ff8e1779cbf44c58d5c3f79f7a51a1adbed7d06d1c6`. El archivo pesa 56,854,345 bytes = 54.2 MiB; UCI lo muestra como 54.2 MB.

## Orden de ejecución (desde la carpeta del proyecto)

| Paso | Comando | Tiempo aproximado |
|---|---|---|
| 1. Prueba rápida (5,000 filas, 2 pliegues, B = 5); debe pasar antes | `Rscript codigo_R/prueba_rapida.R` | 4 min |
| 2. Reproducibilidad (SHA-256, conteos, entorno) | `Rscript codigo_R/seccion_7_reproducibilidad.R` | < 1 min |
| 3. Datos y diccionario | `Rscript codigo_R/seccion_4_datos.R` | < 1 min |
| 4. Descriptivo e inferencia I1 a I4 | `Rscript codigo_R/seccion_5_exploracion_inferencia.R` | < 1 min |
| 5. Modelación y evaluación, por etapas | `Rscript codigo_R/ejecutar_s6.R todas` | ver abajo |
| 6. Comparación con las cifras de Python | `Rscript codigo_R/comparacion_python_R.R` | < 1 min |
| 7. Informe y resumen | `Rscript -e "source('codigo_R/comun.R'); asegurar_pandoc(); rmarkdown::render('informe.Rmd'); rmarkdown::render('resumen_ejecutivo.Rmd')"` | 1 min |
| 8. PDF del resumen | `Rscript codigo_R/a_pdf.R resumen_ejecutivo.html` | < 1 min |

### Sección 6 por etapas

`ejecutar_s6.R` lanza cada etapa en un proceso de R aparte. El límite de tiempo se controla desde fuera y el PID se escribe en `resultados_R/etapas/<etapa>.pid` mientras la etapa corre. Cada etapa guarda su resultado en `resultados_R/etapas/<etapa>.rds`. Si una falla, las demás se conservan; se reanuda con `Rscript codigo_R/ejecutar_s6.R <etapa>` (añadir `--rehacer` para repetir una etapa ya hecha). El estado de cada intento queda en `resultados_R/etapas/estado.csv` y los logs, con marca de hora, en `resultados_R/logs/`.

| Etapa | Qué hace | Tiempo medido |
|---|---|---|
| preparar | Partición 75/25 (semilla 2026) y filtro VIF | 8 s |
| separacion | Diagnóstico de separación | 1.7 min |
| cv_logistica | Validación cruzada de C (5 pliegues; regla de 1 error estándar) | 2.5 min |
| cv_bosque | Validación cruzada del bosque (3 pliegues, 30,000 filas) | 20 s |
| umbrales_oof | Predicciones fuera de pliegue en entrenamiento | 3.4 min |
| ajuste_final | Modelos finales y una sola evaluación en la prueba | 1.3 min |
| influencia | Reajuste sin las observaciones marcadas | 16 min |
| sens_* (9 escenarios) | Escenarios de sensibilidad (el de formato comparable tiene límite de 5 min) | 13 min en total |
| bootstrap | Razones de momios con IC bootstrap (C = 10) | ver abajo |
| consolidar | Tablas finales, sensibilidad y ajustes no convergidos | < 1 min |

Sin el bootstrap, la sección 6 tarda unos 40 minutos.

## Bootstrap: cómo se obtuvo el resultado con B = 75

- **El código, por omisión, corre B = 200 réplicas.** Usa 4 procesos en paralelo, guarda un punto de control cada 25 réplicas y da a cada réplica su propia semilla, generada con `set.seed(2026)`. Por eso las réplicas son las mismas con o sin paralelismo y al reanudar.
- **El resultado entregado viene de una corrida de 200 cerrada en el punto de guardado de 75 réplicas,** por tiempo. De ellas, 74 se usaron y 1 no convergió y se excluyó de los intervalos.
  - Esa corrida tardó 46 minutos en llegar a 75 réplicas.
  - El bloque siguiente se retrasó más de 18 minutos por una sola réplica lenta.
  - El cierre se hizo con las mismas semillas por réplica.
- **El punto de guardado se conserva** en `resultados_R/etapas/bootstrap_parcial_75.rds`.

**Reproducir exactamente el resultado entregado (B = 75), en unos 40 segundos:**

```
cp resultados_R/etapas/bootstrap_parcial_75.rds resultados_R/etapas/bootstrap_parcial.rds
PIA_BOOTSTRAP_DESDE_PARCIAL=1 Rscript codigo_R/ejecutar_s6.R bootstrap consolidar --rehacer
```

En PowerShell, la segunda línea es `$env:PIA_BOOTSTRAP_DESDE_PARCIAL = "1"; Rscript codigo_R/ejecutar_s6.R bootstrap consolidar --rehacer`.

**Obtener las 200 réplicas:**

```
Rscript codigo_R/ejecutar_s6.R bootstrap consolidar --rehacer
```

Sin la variable `PIA_BOOTSTRAP_DESDE_PARCIAL`, la etapa calcula el bootstrap completo.

- **Reanudación:** si existe `resultados_R/etapas/bootstrap_parcial.rds` con la misma configuración, la etapa reanuda desde ahí. Las réplicas 1 a 75 son las mismas que las entregadas.
- **Tiempo:** la estimación previa era de unos 70 minutos para 200 réplicas con 4 procesos. En la corrida real algunas réplicas tardaron mucho más, así que puede pasar de 2 horas.
- **Límite del lanzador:** el lanzador detiene el bootstrap a los 120 minutos. Al volver a lanzar el mismo comando, reanuda desde el último punto de guardado.
- **Después:** volver a correr `codigo_R/comparacion_python_R.R` y compilar el informe. El informe lee el B real de `resultados_R/s6_bootstrap_convergencia.csv`.

## Archivos

| Archivo | Qué hace |
|---|---|
| `codigo_R/comun.R` | Carga de datos, clase phishing = 1 − label, deduplicación, partición, VIF, métricas, log y modo de prueba rápida |
| `codigo_R/seccion_7_reproducibilidad.R` | Verifica SHA-256, conteos, columnas y deduplicación; registra el entorno |
| `codigo_R/seccion_4_datos.R` | Auditoría de calidad, formato por clase y diccionario de variables |
| `codigo_R/seccion_5_exploracion_inferencia.R` | Resúmenes, figuras, reglas descriptivas e inferencia I1 a I4 |
| `codigo_R/seccion_6_modelacion_evaluacion.R` | Modelos, evaluación, umbrales, influencia, sensibilidad y bootstrap, por etapas |
| `codigo_R/complementos_s6.R` | Regla base, umbrales fuera de pliegue, grupos redundantes e influencia |
| `codigo_R/ejecutar_s6.R` | Lanzador de etapas con límite de tiempo y PID |
| `codigo_R/prueba_rapida.R` | Prueba rápida de todas las secciones |
| `codigo_R/pruebas_desbordamiento.R` | Pruebas de desbordamiento de enteros con valores grandes |
| `codigo_R/comparar_tipo_newton.R` | Comparación técnica de dos variantes de glmnet (no interviene en los resultados) |
| `codigo_R/comparacion_python_R.R` | Cifras de Python frente a R con tolerancias explícitas |
| `codigo_R/a_pdf.R` | HTML a PDF con el navegador y conteo de páginas |
| `resultados_R/bitacora.md` | Bitácora de decisiones y de cada etapa |

## Semillas

- 2026 en la partición, los pliegues, el bosque y el bootstrap.
- 2027 en las predicciones fuera de pliegue.
- 2027 a 2036 en los controles al azar de la influencia.

## Pendientes de verificar

- Nombre del docente: solo se verificó contra el documento del curso.
- Referencia Prasad y Chandra (2024): volumen 136 y artículo 103545, verificados en Crossref.
- Tamaño del archivo: 54.2 MiB; UCI lo muestra como 54.2 MB.
