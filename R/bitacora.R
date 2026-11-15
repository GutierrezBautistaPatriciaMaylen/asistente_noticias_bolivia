# Título: carga de paquetes y rutas de la bitácora
library(tidyverse)
library(jsonlite)

ARCHIVO_BITACORA  <- "bitacora/bitacora.csv"
CARPETA_SNAPSHOTS <- "bitacora/snapshots"
dir.create(CARPETA_SNAPSHOTS, showWarnings = FALSE, recursive = TRUE)

# Título: funciones de lectura y escritura de la bitácora
leer_bitacora <- function() {
  if (!file.exists(ARCHIVO_BITACORA)) return(NULL)
  read_csv(ARCHIVO_BITACORA, col_types = cols(.default = col_character()),
           na = character(), show_col_types = FALSE)
}

escribir_bitacora <- function(tipo, seccion = "", archivo = "", version = "",
                              motivo = "", fragmento = "", efecto = "") {
  nueva <- tibble(
    fecha = format(Sys.Date(), "%d/%m/%Y"),
    hora = format(Sys.time(), "%H:%M:%S"),
    responsable = Sys.info()[["user"]],
    tipo = tipo, seccion = seccion, archivo = archivo, version = version,
    motivo = motivo, fragmento = fragmento, efecto = efecto
  )
  previa <- leer_bitacora()
  if (!is.null(previa)) nueva <- bind_rows(previa, nueva)
  write_excel_csv(nueva, ARCHIVO_BITACORA, na = "")
  invisible(nueva)
}

contar_versiones <- function(nombre) {
  b <- leer_bitacora()
  if (is.null(b)) return(0L)
  sum(b$archivo == nombre & b$tipo %in% c("linea_base", "cambio"))
}

# Título: función que compara dos versiones de un script (líneas quitadas y agregadas)
resumen_diferencias <- function(ruta_vieja, ruta_nueva, max_lineas = 25) {
  vieja <- readLines(ruta_vieja, warn = FALSE, encoding = "UTF-8")
  nueva <- readLines(ruta_nueva, warn = FALSE, encoding = "UTF-8")
  quitadas  <- setdiff(vieja, nueva)
  agregadas <- setdiff(nueva, vieja)
  quitadas  <- quitadas[nzchar(trimws(quitadas))]
  agregadas <- agregadas[nzchar(trimws(agregadas))]
  lineas <- c(paste("-", head(quitadas, max_lineas)),
              paste("+", head(agregadas, max_lineas)))
  paste(lineas, collapse = " // ")
}

# Título: detección automática de cambios en los scripts de la carpeta R
registrar_cambios <- function(motivo = "(pendiente de justificar)", efecto = "",
                              carpeta = "R") {
  scripts <- list.files(carpeta, pattern = "\\.R$", full.names = TRUE)
  hubo <- FALSE
  
  # es la primera vez si aún no hay filas de linea_base ni de cambio
  b0 <- leer_bitacora()
  primera_vez <- is.null(b0) || !any(b0$tipo %in% c("linea_base", "cambio"))
  
  for (s in scripts) {
    nombre  <- basename(s)
    copia   <- file.path(CARPETA_SNAPSHOTS, nombre)
    seccion <- sub("^\\d+_", "", tools::file_path_sans_ext(nombre))
    version <- paste0("v", contar_versiones(nombre) + 1)
    
    if (!file.exists(copia)) {
      tipo <- if (primera_vez) "linea_base" else "cambio"
      n <- length(readLines(s, warn = FALSE))
      escribir_bitacora(tipo, seccion, nombre, version,
                        motivo = if (tipo == "linea_base") "Estado inicial del script al activar la bitácora" else motivo,
                        fragmento = paste0("archivo nuevo (", n, " líneas)"),
                        efecto = efecto)
      file.copy(s, copia, overwrite = TRUE)
      hubo <- TRUE
    } else if (unname(tools::md5sum(s)) != unname(tools::md5sum(copia))) {
      escribir_bitacora("cambio", seccion, nombre, version, motivo,
                        fragmento = resumen_diferencias(copia, s),
                        efecto = efecto)
      file.copy(s, copia, overwrite = TRUE)
      hubo <- TRUE
    }
  }
  if (!hubo) message("Bitácora: no se detectaron cambios en los scripts.")
  invisible(hubo)
}

# Título: funciones para registrar pasos del procedimiento y corridas
registrar_paso <- function(seccion, motivo, efecto = "") {
  escribir_bitacora("paso", seccion = seccion, motivo = motivo, efecto = efecto)
}

registrar_corrida <- function(seccion, archivo, motivo, efecto) {
  escribir_bitacora("corrida", seccion = seccion, archivo = archivo,
                    motivo = motivo, efecto = efecto)
}


# Título: exportación de la bitácora a JSON para mostrarla en la interfaz web
exportar_bitacora_json <- function(destino = "docs/bitacora.json") {
  dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
  b <- leer_bitacora()
  if (is.null(b)) return(invisible(NULL))
  b <- b[nrow(b):1, ]   # lo más reciente primero
  write_json(b, destino, pretty = TRUE, auto_unbox = TRUE)
  invisible(b)
}


