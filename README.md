# Auditoría estadística de detección de phishing

Proyecto final de la materia **Estadística aplicada a Seguridad en TI**.

- **Grupo:** 061
- **Docente:** Dr. Álvaro E. Cordero-Franco
- **Integrantes:** Alejandro Martínez Moya, Fernando Garza Chávez, David Emiliano Rangel Tovar
- **Fecha de entrega:** 20/11/2026

## Pregunta

¿Qué características de una URL y de su página ayudan a distinguir sitios de phishing de sitios legítimos, y qué umbral conviene usar en la regresión logística?

## Archivos

- `informe.Rmd`: código y análisis, reproducible desde una sesión limpia.
- `informe.html`: informe generado a partir de `informe.Rmd`.
- `resumen_ejecutivo.pdf`: resumen de una página.
- `presentacion.pdf`: presentación del proyecto, titulada *Auditoría de phishing*.

## Datos

Usamos el conjunto **PhiUSIIL Phishing URL Dataset** de la UCI Machine Learning Repository, con licencia CC BY 4.0.

- Descarga: https://archive.ics.uci.edu/dataset/967/phiusiil+phishing+url+dataset
- Cita: Prasad, A. y Chandra, S. (2024). *Computers & Security*. DOI: 10.1016/j.cose.2023.103545

El archivo CSV no se incluye en el repositorio; hay que descargarlo y colocarlo como se indica abajo.

## Cómo reproducir el análisis

1. Instalar R (probamos con R 4.6.1).
2. Descargar el CSV desde la UCI y guardarlo en la misma carpeta que `informe.Rmd` con el nombre `PhiUSIIL_Phishing_URL_Dataset.csv`.
3. Instalar los paquetes del primer bloque de código de `informe.Rmd` (una sola vez).
4. Renderizar el informe:

```r
rmarkdown::render("informe.Rmd")
```

La semilla usada en todo el análisis es 2026.

## Licencia

Los datos se usan bajo CC BY 4.0. El código y el texto son de uso académico.
