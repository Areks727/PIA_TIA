# Convierte un HTML local a PDF con el navegador Edge (o Chrome) en modo sin ventana y
# cuenta las páginas del PDF. Uso: Rscript codigo_R/a_pdf.R archivo.html [salida.pdf]
args <- commandArgs(trailingOnly = TRUE)
html <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
pdf <- if (length(args) > 1) args[2] else sub("[.]html$", ".pdf", html)
cand <- c("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
          "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
          "C:/Program Files/Google/Chrome/Application/chrome.exe")
nav <- cand[file.exists(cand)][1]
if (is.na(nav)) stop("No se encontró Edge ni Chrome para convertir a PDF")
perfil <- file.path(tempdir(), "perfil_pdf")
pdf_abs <- normalizePath(pdf, winslash = "/", mustWork = FALSE)
system2(nav, c("--headless", "--disable-gpu", "--no-pdf-header-footer", paste0("--user-data-dir=", shQuote(perfil)),
               paste0("--print-to-pdf=", shQuote(pdf_abs)), shQuote(paste0("file:///", html))),
        stdout = FALSE, stderr = FALSE)
Sys.sleep(1)
if (!file.exists(pdf_abs)) stop("No se generó el PDF")
b <- readBin(pdf_abs, "raw", file.size(pdf_abs))
txt <- rawToChar(b[b != as.raw(0)])
paginas <- length(gregexpr("/Type[[:space:]]*/Page[^s]", txt, useBytes = TRUE)[[1]])
cat(sprintf("PDF: %s | páginas: %d\n", pdf_abs, paginas))
